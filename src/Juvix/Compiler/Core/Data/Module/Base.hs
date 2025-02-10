module Juvix.Compiler.Core.Data.Module.Base where

import Juvix.Compiler.Core.Language

data Module' t = Module
  { _moduleId :: ModuleId,
    _moduleInfoTable :: t,
    -- | The imports field contains all direct (non-transitive) dependencies of
    -- the module.
    _moduleImports :: [ModuleId],
    -- | The imports table contains all dependencies, transitively. E.g., if the
    -- module M imports A but not B, but A imports B, then all identifiers from
    -- B will be in the imports table of M nonetheless.
    _moduleImportsTable :: t
  }
  deriving stock (Generic)

instance (Serialize t) => Serialize (Module' t)

instance (NFData t) => NFData (Module' t)

makeLenses ''Module'

withInfoTable :: (Monoid t) => (Module' t -> Module' t) -> t -> t
withInfoTable f tab =
  f (moduleFromInfoTable tab) ^. moduleInfoTable

emptyModule :: (Monoid t) => Module' t
emptyModule = Module defaultModuleId mempty mempty mempty

moduleFromInfoTable :: (Monoid t) => t -> Module' t
moduleFromInfoTable tab = Module defaultModuleId tab mempty mempty

computeCombinedInfoTable :: (Monoid t) => Module' t -> t
computeCombinedInfoTable Module {..} = _moduleInfoTable <> _moduleImportsTable
