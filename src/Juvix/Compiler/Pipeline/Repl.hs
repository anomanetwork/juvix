module Juvix.Compiler.Pipeline.Repl where

import Juvix.Compiler.Concrete (evalHighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder (runParserResultBuilder)
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker (runTopModuleNameChecker)
import Juvix.Compiler.Concrete.Translation.ImportScanner (defaultImportScanStrategy)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames)
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Internal.Translation.Repl qualified as Repl
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.Artifacts.PathResolver
import Juvix.Compiler.Pipeline.Driver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree (withImportTree)
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Compiler.Pipeline.Run (defaultPipelineOptions, evalModuleInfoCacheHelper)
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process (runProcessIO)
import Juvix.Prelude

upToInternalExpression ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts, Termination] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Internal.Expression
upToInternalExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  mtab <- gets (^. artifactModuleTable)
  pkg <- asks (^. entryPointPackageId)
  mpkg <- asks (^. entryPointMainPackageId)
  runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    . runReader pkg
    . runReader mpkg
    $ runNameIdGenArtifacts (Scoper.scopeCheckExpression (Store.getScopedModuleTable mtab) scopeTable p)
      >>= runNameIdGenArtifacts
        . runReader scopeTable
        . Internal.fromConcreteExpression

expressionUpToAtomsParsed ::
  (Members '[State Artifacts, Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionUpToAtomsParsed fp txt =
  runNameIdGenArtifacts $
    Parser.expressionFromTextSource fp txt

expressionUpToAtomsScoped ::
  (Members '[Reader EntryPoint, State Artifacts, Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Scoped)
expressionUpToAtomsScoped fp txt = do
  scopeTable <- gets (^. artifactScopeTable)
  mtab <- gets (^. artifactModuleTable)
  pkg <- asks (^. entryPointPackageId)
  mpkg <- asks (^. entryPointMainPackageId)
  runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    . runNameIdGenArtifacts
    . runReader pkg
    . runReader mpkg
    $ Parser.expressionFromTextSource fp txt
      >>= Scoper.scopeCheckExpressionAtoms (Store.getScopedModuleTable mtab) scopeTable

scopeCheckExpression ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  mtab <- gets (^. artifactModuleTable)
  pkg <- asks (^. entryPointPackageId)
  mpkg <- asks (^. entryPointMainPackageId)
  runNameIdGenArtifacts
    . runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    . runReader pkg
    . runReader mpkg
    $ Scoper.scopeCheckExpression (Store.getScopedModuleTable mtab) scopeTable p

parseReplInput ::
  (Members '[PathResolver, Files, State Artifacts, Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r Parser.ReplInput
parseReplInput fp txt =
  evalHighlightBuilder
    . runNameIdGenArtifacts
    . runStateLikeArtifacts runParserResultBuilder artifactParsing
    $ Parser.replInputFromTextSource fp txt

expressionUpToTyped ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r) =>
  Path Abs File ->
  Text ->
  Sem r Internal.TypedExpression
expressionUpToTyped fp txt = do
  p <- expressionUpToAtomsParsed fp txt
  runTerminationArtifacts
    ( upToInternalExpression p
        >>= Repl.typeCheckExpressionType
    )

compileExpression ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Core.Node
compileExpression p =
  runTerminationArtifacts
    ( upToInternalExpression p
        >>= Repl.typeCheckExpression
    )
    >>= fromInternalExpression

registerImport ::
  (Members '[TaggedLock, Error JuvixError, State Artifacts, Reader EntryPoint, Files, GitClone, PathResolver, ModuleInfoCache] r) =>
  Import 'Parsed ->
  Sem r ()
registerImport i = do
  PipelineResult {..} <- processImport (i ^. importModulePath)
  let mtab' = Store.insertModule (i ^. importModulePath) _pipelineResult _pipelineResultImports
  modify' (appendArtifactsModuleTable mtab')
  scopeTable <- gets (^. artifactScopeTable)
  mtab'' <- gets (^. artifactModuleTable)
  pkg <- asks (^. entryPointPackageId)
  mpkg <- asks (^. entryPointMainPackageId)
  void
    . runNameIdGenArtifacts
    . runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    . runReader pkg
    . runReader mpkg
    $ Scoper.scopeCheckReplImport (Store.getScopedModuleTable mtab'') scopeTable i

fromInternalExpression :: (Members '[State Artifacts, Error JuvixError] r) => Internal.Expression -> Sem r Core.Node
fromInternalExpression exp = do
  typedTable <- gets (^. artifactInternalTypedTable)
  runNameIdGenArtifacts
    . runReader typedTable
    . tmpCoreInfoTableBuilderArtifacts
    . readerFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    . mapError (JuvixError . ErrBadScope)
    $ Core.goExpression exp

data ReplPipelineResult
  = ReplPipelineResultNode Core.Node
  | ReplPipelineResultImport TopModulePath
  | ReplPipelineResultOpen Name

compileReplInputIO ::
  (Members '[Reader EntryPoint, State Artifacts, EmbedIO] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError ReplPipelineResult)
compileReplInputIO fp txt = do
  hasInternet <- not <$> asks (^. entryPointOffline)
  runError
    . runConcurrent
    . runReader defaultPipelineOptions
    . runLoggerIO replLoggerOptions
    . runReader defaultNumThreads
    . evalInternet hasInternet
    . evalHighlightBuilder
    . runTaggedLockPermissive
    . runFilesIO
    . mapError (JuvixError @GitProcessError)
    . runProcessIO
    . runGitProcess
    . mapError (JuvixError @DependencyError)
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    . runDependencyResolver
    . runReader defaultDependenciesConfig
    . runPathResolverArtifacts
    . runTopModuleNameChecker
    . runReader defaultImportScanStrategy
    . withImportTree (Just fp)
    . evalModuleInfoCacheHelper
    $ do
      p <- parseReplInput fp txt
      case p of
        Parser.ReplExpression e -> ReplPipelineResultNode <$> compileExpression e
        Parser.ReplImport i -> registerImport i $> ReplPipelineResultImport (i ^. importModulePath)
        Parser.ReplOpen i -> return (ReplPipelineResultOpen (i ^. openModuleName))

runTransformations ::
  forall r.
  (Members '[State Artifacts, Error JuvixError, Reader EntryPoint] r) =>
  Bool ->
  [Core.TransformationId] ->
  Core.Node ->
  Sem r Core.Node
runTransformations shouldDisambiguate ts n = runCoreInfoTableBuilderArtifacts $ do
  sym <- addNode n
  applyTransforms shouldDisambiguate ts
  getNode sym
  where
    addNode :: Core.Node -> Sem (Core.InfoTableBuilder ': r) Core.Symbol
    addNode node = do
      sym <- Core.freshSymbol
      Core.registerIdentNode sym node
      -- `n` will get filtered out by the transformations unless it has a
      -- corresponding entry in `infoIdentifiers`
      md <- Core.getModule
      let name = Core.freshIdentName md "_repl"
          idenInfo =
            Core.IdentifierInfo
              { _identifierName = name,
                _identifierSymbol = sym,
                _identifierLocation = Nothing,
                _identifierArgsNum = 0,
                _identifierType = Core.mkDynamic',
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = mempty,
                _identifierArgNames = []
              }
      Core.registerIdent name idenInfo
      return sym

    applyTransforms :: Bool -> [Core.TransformationId] -> Sem (Core.InfoTableBuilder ': r) ()
    applyTransforms shouldDisambiguate' ts' = do
      md <- Core.getModule
      md' <- mapReader Core.fromEntryPoint $ Core.applyTransformations ts' md
      let md'' =
            if
                | shouldDisambiguate' -> disambiguateNames md'
                | otherwise -> md'
      Core.setModule md''

    getNode :: Core.Symbol -> Sem (Core.InfoTableBuilder ': r) Core.Node
    getNode sym = fromMaybe impossible . flip Core.lookupIdentifierNode' sym <$> Core.getModule
