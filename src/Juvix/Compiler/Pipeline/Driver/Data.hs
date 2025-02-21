module Juvix.Compiler.Pipeline.Driver.Data
  ( module Juvix.Compiler.Pipeline.Driver.Data,
    module Juvix.Compiler.Pipeline.Result,
  )
where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prelude (show)

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultImportTables :: HashMap ModuleId Core.InfoTable,
    _compileResultChanged :: Bool
  }

data ProcessedNode a = ProcessedNode
  { _processedNode :: ImportNode,
    _processedNodeInfo :: PipelineResult ModuleInfo,
    _processedNodeData :: a
  }

makeLenses ''CompileResult
makeLenses ''ProcessedNode

instance Functor ProcessedNode where
  fmap = over processedNodeData

instance Semigroup CompileResult where
  sconcat l =
    CompileResult
      { _compileResultChanged = any (^. compileResultChanged) l,
        _compileResultModuleTable = sconcatMap (^. compileResultModuleTable) l,
        _compileResultImportTables = sconcatMap (^. compileResultImportTables) l
      }

instance Monoid CompileResult where
  mempty =
    CompileResult
      { _compileResultChanged = False,
        _compileResultModuleTable = mempty,
        _compileResultImportTables = mempty
      }

data ProcessModuleDecision (r :: [Effect])
  = ProcessModuleReuse (PipelineResult Store.ModuleInfo)
  | ProcessModuleRecompile (Recompile r)

data RecompileReason
  = RecompileImportsChanged
  | RecompileNoJvoFile
  | RecompileSourceChanged
  | RecompileOptionsChanged

data Recompile (r :: [Effect]) = Recompile
  { _recompileDo :: Sem r (PipelineResult Store.ModuleInfo),
    _recompileReason :: RecompileReason
  }

makeLenses ''Recompile

processModuleDecisionAction :: ProcessModuleDecision r -> CompileAction
processModuleDecisionAction = \case
  ProcessModuleReuse {} -> CompileActionReuseJvo
  ProcessModuleRecompile r -> case r ^. recompileReason of
    RecompileNoJvoFile -> CompileActionCompile
    _ -> CompileActionRecompile

data CompileAction
  = CompileActionReuseJvo
  | CompileActionRecompile
  | CompileActionCompile

instance Show CompileAction where
  show = \case
    CompileActionReuseJvo -> "Loading"
    CompileActionRecompile -> "Recompiling"
    CompileActionCompile -> "Compiling"

instance Pretty CompileAction where
  pretty = pretty . Prelude.show

compileActionLogLevel :: CompileAction -> LogLevel
compileActionLogLevel = \case
  CompileActionReuseJvo -> LogLevelVerbose
  CompileActionRecompile -> LogLevelProgress
  CompileActionCompile -> LogLevelProgress
