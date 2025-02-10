module Juvix.Compiler.Store.Backend.Module where

import Juvix.Compiler.Store.Backend.Options
import Juvix.Data.ModuleId
import Juvix.Extra.Serialize
import Juvix.Prelude

data Module' t = Module
  { _moduleId :: ModuleId,
    _moduleInfoTable :: t,
    -- | The imports field contains all direct (non-transitive) dependencies of
    -- the module.
    _moduleImports :: [ModuleId],
    _moduleOptions :: Options
  }
  deriving stock (Generic)

instance (Serialize t) => Serialize (Module' t)

instance (NFData t) => NFData (Module' t)

makeLenses ''Module'

newtype ModuleTable' t = ModuleTable
  { _moduleTable :: HashMap ModuleId (Module' t)
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

makeLenses ''ModuleTable'

instance (NFData t) => NFData (ModuleTable' t)
