module Juvix.Compiler.Pipeline.Driver
  ( module Juvix.Compiler.Pipeline.Driver.Data,
    ModuleInfoCache,
    JvoCache,
    evalJvoCache,
    processFileUpTo,
    processProject,
    evalModuleInfoCachePackageDotJuvix,
    evalModuleInfoCacheSequential,
    evalModuleInfoCacheSetup,
    processFileToStoredCore,
    processFileUpToParsing,
    processModule,
    processImport,
    processRecursiveUpTo,
    processImports,
    processModuleToStoredCore,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print.Base (docNoCommentsDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping (getModuleId)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver.Data
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Pipeline.Package (readGlobalPackage)
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff (EvalFileEff)
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Compiler.Store.Scoped.Language qualified as Scoped
import Juvix.Data.CodeAnn
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude
import Parallel.ProgressLog
import Path.Posix qualified as Path

processModule ::
  (Members '[ModuleInfoCache] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule = cacheGet

evalModuleInfoCacheSequential ::
  forall r a.
  ( Members
      '[ TaggedLock,
         HighlightBuilder,
         TopModuleNameChecker,
         Error JuvixError,
         Files,
         Concurrent,
         Logger,
         Reader EntryPoint,
         Reader ImportTree,
         Reader PipelineOptions,
         PathResolver
       ]
      r
  ) =>
  Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCacheSequential = evalModuleInfoCacheSetup (const (void (compileSequentially)))

-- | Use only to compile package.juvix
evalModuleInfoCachePackageDotJuvix ::
  forall r a.
  ( Members
      '[ TaggedLock,
         HighlightBuilder,
         TopModuleNameChecker,
         Error JuvixError,
         Files,
         Concurrent,
         Logger,
         PathResolver
       ]
      r
  ) =>
  Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCachePackageDotJuvix =
  evalJvoCache
    . ignoreProgressLog
    . evalCacheEmpty processModuleCacheMiss

-- | Compiles the whole project sequentially (i.e. all modules in the ImportTree).
compileSequentially ::
  forall r.
  ( Members
      '[ ModuleInfoCache,
         Reader EntryPoint,
         PathResolver,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r (HashMap ImportNode (PipelineResult Store.ModuleInfo))
compileSequentially = do
  nodes :: HashSet ImportNode <- asks (^. importTreeNodes)
  hashMapFromHashSetM nodes (mkEntryIndex >=> compileNode)

compileNode ::
  (Members '[ModuleInfoCache, PathResolver] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
compileNode e =
  withResolverRoot (e ^. entryIxImportNode . importNodePackageRoot)
  -- As opposed to parallel compilation, here we don't force the result
  $
    processModule e

-- | Used for parallel compilation
evalModuleInfoCacheSetup ::
  forall r a.
  ( Members
      '[ TaggedLock,
         Logger,
         HighlightBuilder,
         TopModuleNameChecker,
         Concurrent,
         Error JuvixError,
         Files,
         Reader ImportTree,
         Reader PipelineOptions,
         PathResolver
       ]
      r
  ) =>
  (EntryIndex -> Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) ()) ->
  Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCacheSetup setup m = do
  evalJvoCache
    . runProgressLog
    . evalCacheEmptySetup setup processModuleCacheMiss
    $ m

logDecision :: (Members '[ProgressLog] r) => ThreadId -> ImportNode -> ProcessModuleDecision x -> Sem r ()
logDecision _logItemThreadId _logItemModule dec = do
  let reason :: Maybe (Doc CodeAnn) = case dec of
        ProcessModuleReuse {} -> Nothing
        ProcessModuleRecompile r -> case r ^. recompileReason of
          RecompileNoJvoFile -> Nothing
          RecompileImportsChanged -> Just "Because an imported module changed"
          RecompileSourceChanged -> Just "Because the source changed"
          RecompileOptionsChanged -> Just "Because compilation options changed"
          RecompileFieldSizeChanged -> Just "Because the field size changed"

      msg :: Doc CodeAnn =
        docNoCommentsDefault (_logItemModule ^. importNodeTopModulePathKey)
          <+?> (parens <$> reason)

  progressLog
    LogItem
      { _logItemMessage = msg,
        _logItemAction = processModuleDecisionAction dec,
        _logItemThreadId,
        _logItemModule
      }

processModuleCacheMissDecide ::
  forall r rrecompile.
  ( Members
      '[ ModuleInfoCache,
         Error JuvixError,
         Files,
         JvoCache,
         PathResolver
       ]
      r,
    Members
      '[ ModuleInfoCache,
         Error JuvixError,
         Files,
         TaggedLock,
         TopModuleNameChecker,
         HighlightBuilder,
         PathResolver
       ]
      rrecompile
  ) =>
  EntryIndex ->
  Sem r (ProcessModuleDecision rrecompile)
processModuleCacheMissDecide entryIx = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      sourcePath = fromJust (entry ^. entryPointModulePath)
      relPath =
        fromJust
          . replaceExtension ".jvo"
          . fromJust
          $ stripProperPrefix $(mkAbsDir "/") sourcePath
      absPath = buildDir Path.</> relPath
  sha256 <- SHA256.digestFile sourcePath

  let recompile :: Sem rrecompile (PipelineResult Store.ModuleInfo)
      recompile = do
        res <- processModuleToStoredCore sha256 entry
        Serialize.saveToFile absPath (res ^. pipelineResult)
        return res

      recompileWithReason :: RecompileReason -> ProcessModuleDecision rrecompile
      recompileWithReason reason =
        ProcessModuleRecompile
          Recompile
            { _recompileDo = recompile,
              _recompileReason = reason
            }

  runErrorWith (return . recompileWithReason) $ do
    info :: Store.ModuleInfo <- loadFromJvoFile absPath >>= errorMaybe RecompileNoJvoFile

    unless (info ^. Store.moduleInfoSHA256 == sha256) (throw RecompileSourceChanged)
    unless (info ^. Store.moduleInfoOptions == opts) (throw RecompileSourceChanged)
    unless (info ^. Store.moduleInfoFieldSize == entry ^. entryPointFieldSize) (throw RecompileFieldSizeChanged)
    CompileResult {..} <- runReader entry (processImports (info ^. Store.moduleInfoImports))
    if
        | _compileResultChanged -> throw RecompileImportsChanged
        | otherwise ->
            return $
              ProcessModuleReuse
                PipelineResult
                  { _pipelineResult = info,
                    _pipelineResultImports = _compileResultModuleTable,
                    _pipelineResultChanged = False
                  }
  where
    entry = entryIx ^. entryIxEntry
    root = entry ^. entryPointRoot
    opts = StoredModule.fromEntryPoint entry

processModuleCacheMiss ::
  forall r.
  ( Members
      '[ ModuleInfoCache,
         TaggedLock,
         HighlightBuilder,
         TopModuleNameChecker,
         Error JuvixError,
         Files,
         JvoCache,
         ProgressLog,
         Concurrent,
         PathResolver
       ]
      r
  ) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleCacheMiss entryIx = do
  p <- processModuleCacheMissDecide entryIx
  tid <- myThreadId
  logDecision tid (entryIx ^. entryIxImportNode) p
  case p of
    ProcessModuleReuse r -> do
      highlightMergeDocTable (r ^. pipelineResult . Store.moduleInfoScopedModule . Scoped.scopedModuleDocTable)
      return r
    ProcessModuleRecompile recomp -> recomp ^. recompileDo

processProject :: (Members '[ModuleInfoCache, Reader EntryPoint, Reader ImportTree] r) => Sem r [(ImportNode, PipelineResult ModuleInfo)]
processProject = do
  rootDir <- asks (^. entryPointRoot)
  nodes <- toList <$> asks (importTreeProjectNodes rootDir)
  forWithM nodes (mkEntryIndex >=> processModule)

processRecursiveUpTo ::
  forall a r.
  ( Members
      '[ Reader EntryPoint,
         TopModuleNameChecker,
         TaggedLock,
         HighlightBuilder,
         Error JuvixError,
         Files,
         PathResolver,
         ModuleInfoCache,
         EvalFileEff
       ]
      r
  ) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (a, [a])
processRecursiveUpTo a = do
  entry <- ask
  PipelineResult {..} <- processFileUpToParsing entry
  let imports = HashMap.keys (_pipelineResultImports ^. Store.moduleTable)
  ms <- forM imports $ \imp ->
    withPathFile imp goImport
  let pkg = entry ^. entryPointPackageId
  mid <- runReader pkg (getModuleId (_pipelineResult ^. Parser.resultModule . modulePath . to topModulePathKey))
  r <-
    evalTopNameIdGen mid
      . runReader _pipelineResultImports
      . runReader _pipelineResult
      $ a
  return (r, ms)
  where
    goImport :: ImportNode -> Sem r a
    goImport node = do
      pkgInfo <- fromJust . HashMap.lookup (node ^. importNodePackageRoot) <$> getPackageInfos
      pkg <- case pkgInfo ^. packagePackage of
        PackageReal p -> return p
        _ -> readGlobalPackage
      entry <- ask
      let entry' =
            entry
              { _entryPointPackageId = pkg ^. packageId,
                _entryPointStdin = Nothing,
                _entryPointModulePath = Just (node ^. importNodeAbsFile)
              }
      (^. pipelineResult) <$> local (const entry') (processFileUpTo a)

processImport ::
  forall r.
  (Members '[ModuleInfoCache, Reader EntryPoint, Error JuvixError, Files, PathResolver] r) =>
  TopModulePath ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport p = withPathFile p getCachedImport
  where
    getCachedImport :: ImportNode -> Sem r (PipelineResult Store.ModuleInfo)
    getCachedImport node = do
      hasParallelSupport <- supportsParallel
      eix <- mkEntryIndex node
      if
          | hasParallelSupport -> do
              res <- cacheGetResult eix
              return (res ^. cacheResult)
          | otherwise -> processModule eix

processFileUpToParsing ::
  forall r.
  (Members '[ModuleInfoCache, Reader EntryPoint, HighlightBuilder, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry = do
  res <- runReader entry upToParsing
  let imports :: [Import 'Parsed] = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- (^. compileResultModuleTable) <$> processImports (map (^. importModulePath) imports)
  return
    PipelineResult
      { _pipelineResult = res,
        _pipelineResultImports = mtab,
        _pipelineResultChanged = True
      }

processFileUpTo ::
  forall r a.
  (Members '[Reader EntryPoint, Error JuvixError, TopModuleNameChecker, PathResolver, Files, HighlightBuilder, ModuleInfoCache] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFileUpToParsing entry
  let pkg = entry ^. entryPointPackageId
  mid <- runReader pkg (getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath . to topModulePathKey))
  a' <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ a
  return (set pipelineResult a' res)

processImports ::
  forall r.
  (Members '[Reader EntryPoint, ModuleInfoCache, Error JuvixError, Files, PathResolver] r) =>
  [TopModulePath] ->
  Sem r CompileResult
processImports imports = do
  ms :: [PipelineResult Store.ModuleInfo] <- forM imports processImport
  let mtab =
        Store.mkModuleTable (map (^. pipelineResult) ms)
          <> mconcatMap (^. pipelineResultImports) ms
      changed = any (^. pipelineResultChanged) ms
  return
    CompileResult
      { _compileResultChanged = changed,
        _compileResultModuleTable = mtab
      }

processModuleToStoredCore ::
  forall r.
  (Members '[ModuleInfoCache, PathResolver, HighlightBuilder, TopModuleNameChecker, Error JuvixError, Files] r) =>
  Text ->
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleToStoredCore sha256 entry = over pipelineResult mkModuleInfo <$> processFileToStoredCore entry
  where
    mkModuleInfo :: Core.CoreResult -> Store.ModuleInfo
    mkModuleInfo Core.CoreResult {..} =
      Store.ModuleInfo
        { _moduleInfoScopedModule = scoperResult ^. Scoper.resultScopedModule,
          _moduleInfoInternalModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternalModule,
          _moduleInfoCoreTable = fromCore (_coreResultModule ^. Core.moduleInfoTable),
          _moduleInfoImports = map (^. importModulePath) $ scoperResult ^. Scoper.resultParserResult . Parser.resultParserState . parserStateImports,
          _moduleInfoOptions = StoredOptions.fromEntryPoint entry,
          _moduleInfoSHA256 = sha256,
          _moduleInfoFieldSize = entry ^. entryPointFieldSize
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper

processFileToStoredCore ::
  forall r.
  (Members '[ModuleInfoCache, HighlightBuilder, PathResolver, TopModuleNameChecker, Error JuvixError, Files] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry = runReader entry $ do
  res <- processFileUpToParsing entry
  let pkg = entry ^. entryPointPackageId
  mid <- runReader pkg (getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath . to topModulePathKey))
  r <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)
