module Juvix.Compiler.Pipeline.Result where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

data PipelineResult a = PipelineResult
  { _pipelineResult :: a,
    -- | Transitive imports. The imports table contains all dependencies,
    -- transitively. E.g., if module M imports A but not B, but A imports B,
    -- then still both A and B will be in the imports table in the pipeline
    -- result for processing M.
    _pipelineResultImports :: Store.ModuleTable,
    -- | Core imports table for every transitive import stored in
    -- _pipelineResultImports.
    _pipelineResultImportTables :: HashMap ModuleId Core.InfoTable,
    -- | True if the module had to be recompiled. False if the module was loaded
    -- from disk.
    _pipelineResultChanged :: Bool
  }
  deriving stock (Generic)

makeLenses ''PipelineResult

instance (NFData a) => NFData (PipelineResult a)
