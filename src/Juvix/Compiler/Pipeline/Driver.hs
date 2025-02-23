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
    processRecursivelyUpToTyped,
    processRecursivelyUpTo,
    processImports,
    processModuleToStoredCore,
    processProjectUpToScoping,
    processProjectUpToParsing,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print.Base (docNoCommentsDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed (scopeCheck)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping (getModuleId)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource (fromSource)
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context (ParserResult)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Data (InternalTypedResult)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver.Data
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Compiler.Store.Scoped.Language (ScopedModuleTable)
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
         Reader Migration,
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
    . evalCacheEmptySetup setup (runMigration . processModuleCacheMiss)
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
         Reader Migration,
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
      subdir = StoredOptions.getOptionsSubdir opts
      absPath = buildDir Path.</> subdir Path.</> relPath
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
    unless (info ^. Store.moduleInfoOptions == opts) (throw RecompileOptionsChanged)
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
         Reader Migration,
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

processProject ::
  (Members '[PathResolver, ModuleInfoCache, Reader EntryPoint, Reader ImportTree] r) =>
  Sem r [ProcessedNode ()]
processProject = do
  rootDir <- asks (^. entryPointRoot)
  nodes <- asks (importTreeProjectNodes rootDir)
  map mkProcessed <$> forWithM nodes (mkEntryIndex >=> processModule)
  where
    mkProcessed :: (ImportNode, PipelineResult ModuleInfo) -> ProcessedNode ()
    mkProcessed (_processedNode, _processedNodeInfo) =
      ProcessedNode
        { _processedNodeData = (),
          ..
        }

processProjectWith ::
  forall a r.
  ( Members
      '[ Error JuvixError,
         ModuleInfoCache,
         Reader Migration,
         PathResolver,
         Reader EntryPoint,
         Reader MainPackageId,
         Reader ImportTree,
         Files
       ]
      r
  ) =>
  ( forall r'.
    ( Members
        '[ Error JuvixError,
           Files,
           Reader Migration,
           Reader PackageId,
           Reader MainPackageId,
           HighlightBuilder,
           PathResolver
         ]
        r'
    ) =>
    ProcessedNode () ->
    Sem r' a
  ) ->
  Sem r [ProcessedNode a]
processProjectWith procNode = do
  l <- processProject
  pkgId <- asks (^. entryPointPackageId)
  runReader pkgId $
    sequence
      [ do
          d <-
            withResolverRoot (n ^. processedNode . importNodePackageRoot)
              . evalHighlightBuilder
              $ procNode n
          return (set processedNodeData d n)
        | n <- l
      ]

processProjectUpToScoping ::
  forall r.
  ( Members
      '[ Files,
         Error JuvixError,
         Reader Migration,
         PathResolver,
         ModuleInfoCache,
         Reader EntryPoint,
         Reader MainPackageId,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r [ProcessedNode ScoperResult]
processProjectUpToScoping = processProjectWith processNodeUpToScoping

processProjectUpToParsing ::
  forall r.
  ( Members
      '[ Files,
         Error JuvixError,
         PathResolver,
         Reader Migration,
         Reader MainPackageId,
         ModuleInfoCache,
         Reader EntryPoint,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r [ProcessedNode ParserResult]
processProjectUpToParsing = processProjectWith processNodeUpToParsing

processNodeUpToParsing ::
  ( Members
      '[ PathResolver,
         Error JuvixError,
         Files,
         HighlightBuilder,
         Reader PackageId
       ]
      r
  ) =>
  ProcessedNode () ->
  Sem r ParserResult
processNodeUpToParsing node =
  runTopModuleNameChecker $
    fromSource False Nothing (Just (node ^. processedNode . importNodeAbsFile))

processNodeUpToScoping ::
  ( Members
      '[ PathResolver,
         Error JuvixError,
         Reader Migration,
         Files,
         HighlightBuilder,
         Reader PackageId,
         Reader MainPackageId
       ]
      r
  ) =>
  ProcessedNode () ->
  Sem r ScoperResult
processNodeUpToScoping node = do
  parseRes <- processNodeUpToParsing node
  pkg <- ask @PackageId
  let modules = node ^. processedNodeInfo . pipelineResultImports
      scopedModules :: ScopedModuleTable = getScopedModuleTable modules
      tmp :: TopModulePathKey = relPathtoTopModulePathKey (node ^. processedNode . importNodeFile)
      moduleid :: ModuleId = run (runReader pkg (getModuleId tmp))
  evalTopNameIdGen moduleid $
    scopeCheck scopedModules parseRes

processRecursivelyUpTo ::
  forall a r.
  ( Members
      '[ Reader EntryPoint,
         TopModuleNameChecker,
         TaggedLock,
         HighlightBuilder,
         Error JuvixError,
         Files,
         PathResolver,
         ModuleInfoCache
       ]
      r
  ) =>
  (ImportNode -> Bool) ->
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (a, [a])
processRecursivelyUpTo shouldRecurse upto = do
  entry <- ask
  PipelineResult {..} <- processFileUpToParsing entry
  let imports = HashMap.keys (_pipelineResultImports ^. Store.moduleTable)
  ms <- fmap catMaybes . forM imports $ \imp ->
    withPathFile imp goImport
  let pkg = entry ^. entryPointPackageId
  mid <- runReader pkg (getModuleId (_pipelineResult ^. Parser.resultModule . modulePath . to topModulePathKey))
  res <-
    evalTopNameIdGen mid
      . runReader _pipelineResultImports
      . runReader _pipelineResult
      $ upto
  return (res, ms)
  where
    goImport :: ImportNode -> Sem r (Maybe a)
    goImport node = runFail $ do
      failUnless (shouldRecurse node)
      pkgInfo <- fromJust . HashMap.lookup (node ^. importNodePackageRoot) <$> getPackageInfos
      let pid = pkgInfo ^. packageInfoPackageId
      entry <- ask
      let entry' =
            entry
              { _entryPointStdin = Nothing,
                _entryPointResolverRoot = node ^. importNodePackageRoot,
                _entryPointRoot = node ^. importNodePackageRoot,
                _entryPointPackageId = pid,
                _entryPointModulePath = Just (node ^. importNodeAbsFile)
              }
      (^. pipelineResult) <$> runReader entry' (processFileUpTo (inject upto))

processRecursivelyUpToTyped ::
  forall r.
  ( Members
      '[ Reader EntryPoint,
         TopModuleNameChecker,
         Reader Migration,
         TaggedLock,
         HighlightBuilder,
         Error JuvixError,
         Files,
         PathResolver,
         ModuleInfoCache
       ]
      r
  ) =>
  Sem r (InternalTypedResult, [InternalTypedResult])
processRecursivelyUpToTyped = processRecursivelyUpTo (const True) upToInternalTyped

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
          | hasParallelSupport -> cacheGet eix
          | otherwise -> processModule eix

processFileUpToParsing ::
  forall r.
  (Members '[ModuleInfoCache, HighlightBuilder, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry = do
  res <- runReader entry upToParsing
  let imports :: [Import 'Parsed] = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- (^. compileResultModuleTable) <$> runReader entry (processImports (map (^. importModulePath) imports))
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
  (Members '[Reader Migration, ModuleInfoCache, PathResolver, HighlightBuilder, TopModuleNameChecker, Error JuvixError, Files] r) =>
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
          _moduleInfoSHA256 = sha256
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper

processFileToStoredCore ::
  forall r.
  (Members '[Reader Migration, ModuleInfoCache, HighlightBuilder, PathResolver, TopModuleNameChecker, Error JuvixError, Files] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry = do
  res <- processFileUpToParsing entry
  let pkg = entry ^. entryPointPackageId
  mid <- runReader pkg (getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath . to topModulePathKey))
  r <-
    evalTopNameIdGen mid
      . runReader entry
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)
