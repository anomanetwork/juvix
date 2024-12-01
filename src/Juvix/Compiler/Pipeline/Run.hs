module Juvix.Compiler.Pipeline.Run
  ( module Juvix.Compiler.Pipeline.Run,
    module Juvix.Compiler.Pipeline,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as P
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker (TopModuleNameChecker, runTopModuleNameChecker)
import Juvix.Compiler.Concrete.Translation.ImportScanner (ImportScanStrategy, defaultImportScanStrategy)
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Internal.Translation qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as Typed
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Artifacts.PathResolver
import Juvix.Compiler.Pipeline.Driver
import Juvix.Compiler.Pipeline.DriverParallel qualified as DriverPar
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree (withImportTree)
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Compiler.Pipeline.Package.Loader.PathResolver
import Juvix.Compiler.Store.Scoped.Language qualified as Scoped
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude
import Parallel.ProgressLog

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
runIOEither ::
  forall a r.
  (Members PipelineAppEffects r) =>
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r (Either JuvixError (ResolverState, PipelineResult a))
runIOEither entry = fmap snd . runIOEitherHelper entry

runPipelineHighlight ::
  forall a r.
  (Members PipelineAppEffects r) =>
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r HighlightInput
runPipelineHighlight entry = fmap fst . runIOEitherHelper entry

runPipelineRecursiveEither ::
  forall a r.
  (Members PipelineAppEffects r) =>
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r (Either JuvixError (a, [a]))
runPipelineRecursiveEither entry a = do
  x <- runIOEitherPipeline' entry $ do
    processRecursiveUpTo a
  return . mapRight snd $ snd x

runIOEitherHelper ::
  forall a r.
  (Members PipelineAppEffects r) =>
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r (HighlightInput, (Either JuvixError (ResolverState, PipelineResult a)))
runIOEitherHelper entry = runIOEitherPipeline' entry . processFileUpTo

runIOEitherPipeline ::
  forall a r.
  (Members PipelineAppEffects r) =>
  EntryPoint ->
  Sem (PipelineEff' r) a ->
  Sem r (Either JuvixError (ResolverState, a))
runIOEitherPipeline entry = fmap snd . runIOEitherPipeline' entry

-- | Runs the correct implementation of the PathResolver according to the input
-- file
runPathResolverInput ::
  ( Members
      '[ TaggedLock,
         Files,
         Reader EntryPoint,
         DependencyResolver,
         Reader DependenciesConfig,
         Error DependencyError,
         GitClone,
         Error JuvixError,
         EvalFileEff
       ]
      r
  ) =>
  Sem (PathResolver ': r) a ->
  Sem r (ResolverState, a)
runPathResolverInput m = do
  entry <- ask
  if
      | mainIsPackageFile entry -> runPackagePathResolver' (entry ^. entryPointResolverRoot) m
      | otherwise -> runPathResolverPipe m

runIOEitherPipeline' ::
  forall a r.
  (Members '[Reader PipelineOptions, Logger, TaggedLock, EmbedIO] r) =>
  EntryPoint ->
  Sem (PipelineEff' r) a ->
  Sem r (HighlightInput, (Either JuvixError (ResolverState, a)))
runIOEitherPipeline' entry a = do
  let hasInternet = not (entry ^. entryPointOffline)
  opts :: PipelineOptions <- ask
  runConcurrent
    . runReader (opts ^. pipelineNumThreads)
    . evalInternet hasInternet
    . runHighlightBuilder
    . runJuvixError
    . runFilesIO
    . runReader entry
    . runLogIO
    . runProcessIO
    . mapError (JuvixError @GitProcessError)
    . runGitProcess
    . mapError (JuvixError @DependencyError)
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    . runDependencyResolver
    . runReader (opts ^. pipelineDependenciesConfig)
    . runPathResolverInput
    . runTopModuleNameChecker
    . runReader (opts ^. pipelineImportStrategy)
    . withImportTree (entry ^. entryPointModulePath)
    . evalModuleInfoCacheHelper
    $ a

evalModuleInfoCacheHelper ::
  forall r a.
  ( Members
      '[ Reader EntryPoint,
         IOE,
         Reader ImportTree,
         Concurrent,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         HighlightBuilder,
         PathResolver,
         Reader ImportScanStrategy,
         Reader NumThreads,
         Reader PipelineOptions,
         Logger,
         Files
       ]
      r
  ) =>
  Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCacheHelper m = do
  hasParallelSupport <- supportsParallel
  threads <- ask >>= numThreads
  if
      | hasParallelSupport && threads > 1 -> DriverPar.evalModuleInfoCacheParallel m
      | otherwise -> evalModuleInfoCacheSequential m

mainIsPackageFile :: EntryPoint -> Bool
mainIsPackageFile entry = case entry ^. entryPointModulePath of
  Just p -> p == mkPackagePath (entry ^. entryPointResolverRoot)
  Nothing -> False

runIO ::
  forall a r.
  (Members PipelineAppEffects r) =>
  GenericOptions ->
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r (ResolverState, PipelineResult a)
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: (Members '[EmbedIO] r') => Either JuvixError x -> Sem r' x
    mayThrow = \case
      Left err -> runReader opts $ printErrorAnsiSafe err >> exitFailure
      Right r -> return r

runReplPipelineIO :: (MonadIO m) => EntryPoint -> m Artifacts
runReplPipelineIO = runReplPipelineIO' defaultGenericOptions

runReplPipelineIO' :: forall m. (MonadIO m) => GenericOptions -> EntryPoint -> m Artifacts
runReplPipelineIO' opts entry = runReplPipelineIOEither entry >>= mayThrow
  where
    mayThrow :: Either JuvixError r -> m r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> exitFailure
      Right r -> return r

runReplPipelineIOEither ::
  (MonadIO m) =>
  EntryPoint ->
  m (Either JuvixError Artifacts)
runReplPipelineIOEither = runReplPipelineIOEither' LockModePermissive

runReplPipelineIOEither' ::
  forall m.
  (MonadIO m) =>
  LockMode ->
  EntryPoint ->
  m (Either JuvixError Artifacts)
runReplPipelineIOEither' lockMode entry = do
  let hasInternet = not (entry ^. entryPointOffline)
      runPathResolver'
        | mainIsPackageFile entry = runPackagePathResolverArtifacts (entry ^. entryPointResolverRoot)
        | otherwise = runPathResolverArtifacts
  eith <-
    runM
      . runReader defaultPipelineOptions
      . runLoggerIO replLoggerOptions
      . runConcurrent
      . runReader defaultNumThreads
      . evalInternet hasInternet
      . ignoreHighlightBuilder
      . runError
      . runState initialArtifacts
      . runNameIdGenArtifacts
      . runFilesIO
      . runReader entry
      . runTaggedLock lockMode
      . runLogIO
      . mapError (JuvixError @GitProcessError)
      . runProcessIO
      . runGitProcess
      . mapError (JuvixError @DependencyError)
      . mapError (JuvixError @PackageLoaderError)
      . runEvalFileEffIO
      . runDependencyResolver
      . runReader defaultDependenciesConfig
      . runPathResolver'
      . runTopModuleNameChecker
      . runReader defaultImportScanStrategy
      . withImportTree (entry ^. entryPointModulePath)
      . evalModuleInfoCacheHelper
      $ processFileToStoredCore entry
  return $ case eith of
    Left err -> Left err
    Right (art, PipelineResult {..}) ->
      let typedResult :: Internal.InternalTypedResult
          typedResult =
            _pipelineResult
              ^. Core.coreResultInternalTypedResult

          typesTable :: Typed.TypeCheckingTables
          typesTable = typedResult ^. Typed.resultTypeCheckingTables

          typedTable :: Internal.InfoTable
          typedTable = typedResult ^. Typed.resultInternalModule . Typed.internalModuleInfoTable

          internalResult :: Internal.InternalResult
          internalResult =
            typedResult
              ^. Typed.resultInternal

          coreModule :: Core.Module
          coreModule = _pipelineResult ^. Core.coreResultModule

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            internalResult
              ^. Internal.resultScoper

          parserResult :: P.ParserResult
          parserResult = scopedResult ^. Scoped.resultParserResult

          resultScoperTable :: InfoTable
          resultScoperTable = Scoped.getCombinedInfoTable (scopedResult ^. Scoped.resultScopedModule)
       in Right $
            appendArtifactsModuleTable _pipelineResultImports $
              Artifacts
                { _artifactMainModuleScope = Just $ scopedResult ^. Scoped.resultScope,
                  _artifactParsing = parserResult ^. P.resultParserState,
                  _artifactInternalTypedTable = typedTable,
                  _artifactTerminationState = typedResult ^. Typed.resultTermination,
                  _artifactCoreModule = coreModule,
                  _artifactScopeTable = resultScoperTable,
                  _artifactScopeExports = scopedResult ^. Scoped.resultExports,
                  _artifactTypeCheckingTables = typesTable,
                  _artifactScoperState = scopedResult ^. Scoped.resultScoperState,
                  _artifactResolver = art ^. artifactResolver,
                  _artifactNameIdState = art ^. artifactNameIdState,
                  _artifactModuleTable = mempty
                }
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactParsing = mempty,
          _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTerminationState = iniTerminationState,
          _artifactResolver = iniResolverState,
          _artifactNameIdState = genNameIdState defaultModuleId,
          _artifactTypeCheckingTables = mempty,
          _artifactCoreModule = Core.emptyModule,
          _artifactScopeTable = mempty,
          _artifactScopeExports = mempty,
          _artifactScoperState = Scoper.iniScoperState mempty,
          _artifactModuleTable = mempty
        }
