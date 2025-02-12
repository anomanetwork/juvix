module Juvix.Compiler.Core.Data.Stripped.Module
  ( module Juvix.Compiler.Core.Data.Stripped.Module,
    module Juvix.Compiler.Core.Data.Stripped.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.Stripped.InfoTable
import Juvix.Data.ModuleId
import Juvix.Prelude

data Module = Module
  { _moduleId :: ModuleId,
    _moduleInfoTable :: InfoTable,
    -- | The imports field contains all direct (non-transitive) dependencies of
    -- the module.
    _moduleImports :: [ModuleId]
  }
  deriving stock (Generic)
