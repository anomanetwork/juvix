module Juvix.Compiler.Core.Data.Module.Base
  ( module Juvix.Compiler.Core.Data.Module.Base,
    module Juvix.Data.ModuleId,
  )
where

import Data.HashMap.Strict qualified as HashMap
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
    -- | The imports table contains all dependencies, transitively. E.g., if the
    -- module M imports A but not B, but A imports B, then all identifiers from
    -- B will be in the imports table of M nonetheless.
    _moduleImportsTable :: t,
    _moduleSHA256 :: Maybe Text
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

withInfoTable :: (Monoid t) => (Module' t -> Module' t) -> t -> t
withInfoTable f tab =
  f (moduleFromInfoTable tab) ^. moduleInfoTable

emptyModule :: (Monoid t) => ModuleId -> Module' t
emptyModule mid = Module mid mempty mempty mempty Nothing

moduleFromInfoTable :: (Monoid t) => t -> Module' t
moduleFromInfoTable tab = Module defaultModuleId tab mempty mempty Nothing

computeCombinedInfoTable :: (Monoid t) => Module' t -> t
computeCombinedInfoTable Module {..} = _moduleInfoTable <> _moduleImportsTable

combineInfoTables :: (Monoid t) => Module' t -> Module' t
combineInfoTables md =
  md
    { _moduleInfoTable = computeCombinedInfoTable md,
      _moduleImportsTable = mempty
    }

lookupModuleTable' :: ModuleTable' t -> ModuleId -> Maybe (Module' t)
lookupModuleTable' mt mid = HashMap.lookup mid (mt ^. moduleTable)

lookupModuleTable :: ModuleTable' t -> ModuleId -> Module' t
lookupModuleTable mt mid =
  fromMaybe (impossibleError ("Could not find module " <> prettyText mid)) (lookupModuleTable' mt mid)

computeImportsTable :: (Monoid t) => ModuleTable' t -> [ModuleId] -> t
computeImportsTable mt = foldMap (computeCombinedInfoTable . lookupModuleTable mt)

updateImportsTable :: (Monoid t) => ModuleTable' t -> Module' t -> Module' t
updateImportsTable mt m =
  set moduleImportsTable (computeImportsTable mt (m ^. moduleImports)) m

updateImportsTableM :: (Monoid t, Members '[Reader (ModuleTable' t)] r) => Module' t -> Sem r (Module' t)
updateImportsTableM md = do
  mt <- ask
  return $ updateImportsTable mt md
