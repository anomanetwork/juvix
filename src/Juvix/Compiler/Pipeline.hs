module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root,
  )
where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState qualified as Concrete
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver qualified as PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromSource qualified as P
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Prelude

type PipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, HighlightBuilder, Embed IO]

type TopPipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, State Artifacts, Error JuvixError, HighlightBuilder, Embed IO]

--- An option specifiying how symbols should be pruned in the Internal to Core translation
data SymbolPruningMode
  = FilterUnreachable
  | KeepAll
  deriving stock (Eq)

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, Error JuvixError, NameIdGen, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >> ask >>= Parser.fromSource

upToScoping ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToAbstract ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >>= Abstract.fromConcrete

upToInternal ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Internal.InternalResult
upToInternal = upToAbstract >>= Internal.fromAbstract

upToInternalArity ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >>= Internal.arityChecking

upToInternalTyped ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >>= Internal.typeChecking

upToInternalReachability ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  Internal.filterUnreachable <$> upToInternalTyped

upToCore ::
  Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r =>
  SymbolPruningMode ->
  Sem r Core.CoreResult
upToCore mode =
  let toInternal = case mode of
        FilterUnreachable -> upToInternalReachability
        KeepAll -> upToInternalTyped
   in toInternal >>= Core.fromInternal

upToAsm ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToCore FilterUnreachable >>= \Core.CoreResult {..} -> coreToAsm _coreResultTable

upToMiniC ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToVampIR ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r VampIR.Result
upToVampIR =
  upToCore FilterUnreachable >>= \Core.CoreResult {..} -> coreToVampIR _coreResultTable

upToGeb ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToCore FilterUnreachable >>= \Core.CoreResult {..} -> coreToGeb spec _coreResultTable

upToCoreTypecheck ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck =
  upToCore FilterUnreachable >>= \r -> Core.toTypechecked (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

upToEval ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Core.CoreResult
upToEval =
  upToCore FilterUnreachable >>= \r -> Core.toEval (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

--------------------------------------------------------------------------------
-- Internal workflows
--------------------------------------------------------------------------------

coreToAsm :: Members '[Error JuvixError, Reader EntryPoint] r => Core.InfoTable -> Sem r Asm.InfoTable
coreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore

coreToMiniC :: Members '[Error JuvixError, Reader EntryPoint] r => Core.InfoTable -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

asmToMiniC :: Members '[Error JuvixError, Reader EntryPoint] r => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = Asm.toReg >=> regToMiniC . Reg.fromAsm

regToMiniC :: Member (Reader EntryPoint) r => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  e <- ask
  return $ C.fromReg (Backend.getLimits (e ^. entryPointTarget) (e ^. entryPointDebug)) tab

coreToGeb :: Members '[Error JuvixError, Reader EntryPoint] r => Geb.ResultSpec -> Core.InfoTable -> Sem r Geb.Result
coreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore

coreToVampIR :: Members '[Error JuvixError, Reader EntryPoint] r => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR = Core.toVampIR >=> return . VampIR.toResult . VampIR.fromCore

asmToMiniC' :: Members '[Error JuvixError, Reader Asm.Options] r => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: Member (Reader Asm.Options) r => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab

coreToVampIR' :: Members '[Error JuvixError, Reader Core.CoreOptions] r => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR' = Core.toVampIR' >=> return . VampIR.toResult . VampIR.fromCore

--------------------------------------------------------------------------------
-- Run pipeline
--------------------------------------------------------------------------------

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
runIOEither :: forall a. EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (ResolverState, a))
runIOEither entry = fmap snd . runIOEitherHelper entry

runPipelineHighlight :: forall a. EntryPoint -> Sem PipelineEff a -> IO HighlightInput
runPipelineHighlight entry = fmap fst . runIOEitherHelper entry

runIOEitherHelper :: forall a. EntryPoint -> Sem PipelineEff a -> IO (HighlightInput, (Either JuvixError (ResolverState, a)))
runIOEitherHelper entry =
  runM
    . runHighlightBuilder
    . runJuvixError
    . evalTopBuiltins
    . evalTopNameIdGen
    . runFilesIO
    . runReader entry
    . runPathResolverPipe

runIO :: GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (ResolverState, a)
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO' :: EntryPoint -> Sem PipelineEff a -> IO (ResolverState, a)
runIO' = runIO defaultGenericOptions

corePipelineIO' :: Sem TopPipelineEff (Core.CoreResult) -> EntryPoint -> IO Artifacts
corePipelineIO' p = corePipelineIO defaultGenericOptions p

corePipelineIO :: GenericOptions -> Sem TopPipelineEff (Core.CoreResult) -> EntryPoint -> IO Artifacts
corePipelineIO opts p entry = corePipelineIOEither p entry >>= mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

corePipelineIOEither ::
  Sem TopPipelineEff (Core.CoreResult) ->
  EntryPoint ->
  IO (Either JuvixError Artifacts)
corePipelineIOEither p entry = do
  eith <-
    runM
      . ignoreHighlightBuilder
      . runError
      . runState initialArtifacts
      . runBuiltinsArtifacts
      . runNameIdGenArtifacts
      . runFilesIO
      . runReader entry
      . runPathResolverArtifacts
      $ p
  return $ case eith of
    Left err -> Left err
    Right (art, coreRes) ->
      let typedResult :: Internal.InternalTypedResult
          typedResult =
            coreRes
              ^. Core.coreResultInternalTypedResult

          typesTable :: Typed.TypesTable
          typesTable = typedResult ^. Typed.resultIdenTypes

          functionsTable :: Typed.FunctionsTable
          functionsTable = typedResult ^. Typed.resultFunctions

          typedTable :: Internal.InfoTable
          typedTable = typedResult ^. Typed.resultInfoTable

          coreTable :: Core.InfoTable
          coreTable = coreRes ^. Core.coreResultTable

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            typedResult
              ^. Typed.resultInternalArityResult
                . Arity.resultInternalResult
                . Internal.resultAbstract
                . Abstract.resultScoper

          parserResult :: P.ParserResult
          parserResult = scopedResult ^. Scoped.resultParserResult

          resultScoperTable :: Scoped.InfoTable
          resultScoperTable = scopedResult ^. Scoped.resultScoperTable

          mainModuleScope_ :: Scope
          mainModuleScope_ = Scoped.mainModuleSope scopedResult

          abstractResult :: Abstract.AbstractResult
          abstractResult = typedResult ^. Typed.resultInternalArityResult . Arity.resultInternalResult . Internal.resultAbstract
       in Right $
            foldl'
              (flip ($))
              art
              [ set artifactMainModuleScope (Just mainModuleScope_),
                set artifactParsing (parserResult ^. P.resultBuilderState),
                set artifactAbstractInfoTable (abstractResult ^. Abstract.resultTable),
                set artifactInternalTypedTable typedTable,
                set artifactCoreTable coreTable,
                set artifactScopeTable resultScoperTable,
                set artifactScopeExports (scopedResult ^. Scoped.resultExports),
                set artifactTypes typesTable,
                set artifactFunctions functionsTable,
                set artifactAbstractModuleCache (abstractResult ^. Abstract.resultModulesCache),
                set artifactScoperState (scopedResult ^. Scoped.resultScoperState)
              ]
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactParsing = Concrete.iniState,
          _artifactAbstractInfoTable = Abstract.emptyInfoTable,
          _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTypes = mempty,
          _artifactResolver = PathResolver.iniResolverState,
          _artifactNameIdState = allNameIds,
          _artifactFunctions = mempty,
          _artifactCoreTable = Core.emptyInfoTable,
          _artifactScopeTable = Scoped.emptyInfoTable,
          _artifactBuiltins = iniBuiltins,
          _artifactScopeExports = mempty,
          _artifactInternalTranslationState = Internal.TranslationState mempty,
          _artifactAbstractModuleCache = Abstract.ModulesCache mempty,
          _artifactScoperState = Scoper.iniScoperState
        }
