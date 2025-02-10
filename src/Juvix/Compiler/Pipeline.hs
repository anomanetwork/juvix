module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Options,
    module Juvix.Compiler.Pipeline.Root.Base,
    module Juvix.Compiler.Pipeline.Result,
  )
where

import Data.List.Singletons (type (++))
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromTree qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Cairo qualified as Cairo
import Juvix.Compiler.Backend.Isabelle.Data.Result qualified as Isabelle
import Juvix.Compiler.Backend.Isabelle.Translation.FromTyped qualified as Isabelle
import Juvix.Compiler.Backend.Rust.Translation.FromReg qualified as Rust
import Juvix.Compiler.Casm.Data.Builtins qualified as Casm
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pipeline qualified as Casm
import Juvix.Compiler.Casm.Translation.FromReg qualified as Casm
import Juvix.Compiler.Concrete.Data.Highlight.Builder
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Nockma.Translation.FromTree qualified as NockmaTree
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependencyResolver
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Pipeline.Options
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Compiler.Reg.Pipeline qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Tree qualified as Tree
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process.Base (ProcessE)
import Juvix.Data.Field
import Parallel.ProgressLog

type PipelineAppEffects = '[TaggedLock, Reader PipelineOptions, Logger, EmbedIO]

type PipelineLocalEff =
  '[ ModuleInfoCache,
     ProgressLog,
     JvoCache,
     Reader ImportTree,
     Reader ImportScanStrategy,
     TopModuleNameChecker,
     PathResolver,
     Reader DependenciesConfig,
     DependencyResolver,
     EvalFileEff,
     Error PackageLoaderError,
     Error DependencyError,
     GitClone,
     Error GitProcessError,
     ProcessE,
     Reader EntryPoint,
     Files,
     Error JuvixError,
     HighlightBuilder,
     Internet,
     Reader NumThreads,
     Concurrent
   ]

type PipelineEff' r = PipelineLocalEff ++ r

type PipelineEff r =
  '[ Reader Parser.ParserResult,
     Reader Store.ModuleTable,
     NameIdGen
   ]
    ++ PipelineEff' r

--------------------------------------------------------------------------------
-- Workflows from source
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[HighlightBuilder, TopModuleNameChecker, Reader EntryPoint, Error JuvixError, Files] r) =>
  Sem r Parser.ParserResult
upToParsing = do
  e <- ask
  Parser.fromSource (e ^. entryPointModulePath == e ^. entryPointMainFile) (e ^. entryPointStdin) (e ^. entryPointModulePath)

--------------------------------------------------------------------------------
-- Workflows from parsed source
--------------------------------------------------------------------------------

upToParsedSource ::
  (Members '[Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Parser.ParserResult
upToParsedSource = ask

upToScopingEntry ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Scoper.ScoperResult
upToScopingEntry = do
  pkg <- asks (^. entryPointPackageId)
  runReader pkg (upToScoping)

upToScoping ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader PackageId, Reader Store.ModuleTable, Error JuvixError, NameIdGen] r) =>
  Sem r Scoper.ScoperResult
upToScoping = Scoper.fromParsed

upToInternal ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Error JuvixError, NameIdGen, Termination] r) =>
  Sem r Internal.InternalResult
upToInternal = do
  pkg <- asks (^. entryPointPackageId)
  runReader pkg upToScoping >>= Internal.fromConcrete

upToInternalTyped ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Error JuvixError, Reader EntryPoint, Reader Store.ModuleTable, NameIdGen] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = Internal.typeCheckingNew upToInternal

upToIsabelle ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Error JuvixError, Reader EntryPoint, Reader Store.ModuleTable, NameIdGen] r) =>
  Sem r Isabelle.Result
upToIsabelle = upToInternalTyped >>= Isabelle.fromInternal

upToCore ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Core.CoreResult
upToCore = upToInternalTyped >>= Core.fromInternal

upToStoredCore ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Core.CoreResult
upToStoredCore = do
  r <- upToCore
  md <- Core.toStored (r ^. Core.coreResultModule)
  return r {Core._coreResultModule = md}

upToStoredCore' ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Core.PipelineId ->
  Sem r Core.CoreResult
upToStoredCore' p = do
  r <- upToCore
  md <- Core.toStored' p (r ^. Core.coreResultModule)
  return r {Core._coreResultModule = md}

upToReg ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Reg.InfoTable
upToReg =
  upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToReg _coreResultModule

upToTree ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Tree.InfoTable
upToTree =
  upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToTree Core.IdentityTrans _coreResultModule

upToAsm ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToAsm _coreResultModule

upToCasm ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Casm.Result
upToCasm =
  upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToCasm _coreResultModule

upToCairo ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Cairo.Result
upToCairo =
  upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToCairo _coreResultModule

upToMiniC ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToAnoma ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r NockmaTree.AnomaResult
upToAnoma = upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToAnoma _coreResultModule

upToRust ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Rust.Result
upToRust = upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToRust _coreResultModule

upToRiscZeroRust ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Rust.Result
upToRiscZeroRust = upToStoredCore' Core.PipelineExec >>= \Core.CoreResult {..} -> storedCoreToRiscZeroRust _coreResultModule

upToCoreTypecheck ::
  (Members '[HighlightBuilder, Reader Parser.ParserResult, Reader EntryPoint, Reader Store.ModuleTable, Files, NameIdGen, Error JuvixError] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck = do
  r <- upToCore
  md <- Core.toTypechecked (r ^. Core.coreResultModule)
  return r {Core._coreResultModule = md}

--------------------------------------------------------------------------------
-- Workflows from stored Core
--------------------------------------------------------------------------------

storedCoreToTree ::
  (Members '[Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.Module ->
  Sem r Tree.InfoTable
storedCoreToTree checkId md = do
  Tree.fromCore
    . Stripped.fromCore
    . Core.computeCombinedInfoTable
    <$> Core.toStripped checkId md

storedCoreToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r NockmaTree.AnomaResult
storedCoreToAnoma = storedCoreToTree Core.CheckAnoma >=> treeToAnoma

storedCoreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
storedCoreToAsm = storedCoreToTree Core.CheckExec >=> treeToAsm

storedCoreToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Reg.InfoTable
storedCoreToReg = storedCoreToAsm >=> asmToReg

storedCoreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r C.MiniCResult
storedCoreToMiniC = storedCoreToAsm >=> asmToMiniC

storedCoreToRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Rust.Result
storedCoreToRust = storedCoreToTree Core.CheckRust >=> treeToReg >=> regToRust

storedCoreToRiscZeroRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Rust.Result
storedCoreToRiscZeroRust = storedCoreToTree Core.CheckRust >=> treeToReg >=> regToRiscZeroRust

storedCoreToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Casm.Result
storedCoreToCasm = local (set entryPointFieldSize cairoFieldSize) . storedCoreToTree Core.CheckCairo >=> treeToCasm

storedCoreToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Cairo.Result
storedCoreToCairo = storedCoreToCasm >=> casmToCairo

--------------------------------------------------------------------------------
-- Workflows from Core
--------------------------------------------------------------------------------

coreToTree :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.TransformationId -> Core.Module -> Sem r Tree.InfoTable
coreToTree checkId = Core.toStored >=> storedCoreToTree checkId

coreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Asm.InfoTable
coreToAsm = Core.toStored >=> storedCoreToAsm

coreToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Reg.InfoTable
coreToReg = Core.toStored >=> storedCoreToReg

coreToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Casm.Result
coreToCasm = Core.toStored >=> storedCoreToCasm

coreToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Cairo.Result
coreToCairo = Core.toStored >=> storedCoreToCairo

coreToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r NockmaTree.AnomaResult
coreToAnoma = coreToTree Core.CheckAnoma >=> treeToAnoma

coreToRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Rust.Result
coreToRust = Core.toStored >=> storedCoreToRust

coreToRiscZeroRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r Rust.Result
coreToRiscZeroRust = Core.toStored >=> storedCoreToRiscZeroRust

coreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.Module -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

--------------------------------------------------------------------------------
-- Other workflows
--------------------------------------------------------------------------------

treeToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Asm.InfoTable
treeToAsm = Tree.toAsm >=> return . Asm.fromTree

treeToCairoAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Asm.InfoTable
treeToCairoAsm = Tree.toCairoAsm >=> return . Asm.fromTree

treeToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Reg.InfoTable
treeToReg = treeToAsm >=> asmToReg

treeToAnoma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r (NockmaTree.AnomaResult)
treeToAnoma = Tree.toNockma >=> mapReader NockmaTree.fromEntryPoint . NockmaTree.fromTreeTable

treeToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r C.MiniCResult
treeToMiniC = treeToAsm >=> asmToMiniC

treeToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Casm.Result
treeToCasm = treeToCairoAsm >=> asmToCasm

treeToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Cairo.Result
treeToCairo = treeToCasm >=> casmToCairo

treeToRust' :: (Members '[Error JuvixError, Reader EntryPoint] r) => Rust.Backend -> Tree.InfoTable -> Sem r Rust.Result
treeToRust' backend = treeToReg >=> regToRust' backend

treeToRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Rust.Result
treeToRust = treeToRust' Rust.BackendRust

treeToRiscZeroRust :: (Members '[Error JuvixError, Reader EntryPoint] r) => Tree.InfoTable -> Sem r Rust.Result
treeToRiscZeroRust = treeToRust' Rust.BackendRiscZero

asmToReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Reg.InfoTable
asmToReg = Asm.toReg >=> return . Reg.fromAsm

asmToCasm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Casm.Result
asmToCasm = asmToReg >=> regToCasm

asmToCairo :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r Cairo.Result
asmToCairo = asmToReg >=> regToCairo

asmToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = asmToReg >=> regToMiniC

regToMiniC :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  tab' <- Reg.toC tab
  e <- ask
  return $ C.fromReg (Backend.getLimits (fromJust (e ^. entryPointTarget)) (e ^. entryPointDebug)) tab'

regToRust' :: (Member (Reader EntryPoint) r) => Rust.Backend -> Reg.InfoTable -> Sem r Rust.Result
regToRust' backend tab = do
  tab' <- Reg.toRust tab
  e <- ask
  return $ Rust.fromReg backend (Backend.getLimits (fromJust (e ^. entryPointTarget)) (e ^. entryPointDebug)) tab'

regToRust :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r Rust.Result
regToRust = regToRust' Rust.BackendRust

regToRiscZeroRust :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r Rust.Result
regToRiscZeroRust = regToRust' Rust.BackendRiscZero

regToCasm :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r Casm.Result
regToCasm = Reg.toCasm >=> return . Casm.fromReg

casmToCairo :: (Member (Reader EntryPoint) r) => Casm.Result -> Sem r Cairo.Result
casmToCairo Casm.Result {..} = do
  code' <- Casm.toCairo _resultCode
  return
    . Cairo.serialize _resultOutputSize (map Casm.builtinName _resultBuiltins)
    $ Cairo.fromCasm code'

regToCairo :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r Cairo.Result
regToCairo = regToCasm >=> casmToCairo
