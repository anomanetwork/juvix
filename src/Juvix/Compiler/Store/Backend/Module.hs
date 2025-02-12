module Juvix.Compiler.Store.Backend.Module
  ( module Juvix.Compiler.Store.Backend.Module,
    module Juvix.Compiler.Store.Backend.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Store.Backend.Options
import Juvix.Data.ModuleId
import Juvix.Data.PPOutput (prettyText)
import Juvix.Extra.Serialize
import Juvix.Prelude

data Module' t = Module
  { _moduleId :: ModuleId,
    _moduleInfoTable :: t,
    -- | The imports field contains all direct (non-transitive) dependencies of
    -- the module.
    _moduleImports :: [ModuleId],
    _moduleOptions :: Options,
    _moduleSHA256 :: Text
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

lookupModuleTable' :: ModuleTable' t -> ModuleId -> Maybe (Module' t)
lookupModuleTable' mt mid = HashMap.lookup mid (mt ^. moduleTable)

lookupModuleTable :: ModuleTable' t -> ModuleId -> Module' t
lookupModuleTable mt mid =
  fromMaybe (impossibleError ("Could not find module " <> prettyText mid)) (lookupModuleTable' mt mid)
