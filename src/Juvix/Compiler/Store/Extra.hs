module Juvix.Compiler.Store.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Internal.Language
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Scoped.Data.InfoTable
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

getModulePath :: ModuleInfo -> C.TopModulePath
getModulePath mi = mi ^. moduleInfoScopedModule . scopedModulePath . S.nameConcrete

getModulePathKey :: ModuleInfo -> TopModulePathKey
getModulePathKey = C.topModulePathKey . getModulePath

getScopedModuleTable :: ModuleTable -> ScopedModuleTable
getScopedModuleTable mtab =
  ScopedModuleTable $ HashMap.mapKeys C.topModulePathKey ((^. moduleInfoScopedModule) <$> mtab ^. moduleTable)

getInternalModuleTable :: ModuleTable -> InternalModuleTable
getInternalModuleTable mtab =
  InternalModuleTable $
    hashMap
      [ (mi ^. moduleInfoInternalModule . internalModuleName, mi ^. moduleInfoInternalModule)
        | mi <- toList (mtab ^. moduleTable)
      ]

mkModuleTable :: [ModuleInfo] -> ModuleTable
mkModuleTable = ModuleTable . hashMap . map (\mi -> (getModulePath mi, mi))

lookupModule :: ModuleTable -> C.TopModulePath -> ModuleInfo
lookupModule mtab n = fromJust (mtab ^. moduleTable . at n)

insertModule :: C.TopModulePath -> ModuleInfo -> ModuleTable -> ModuleTable
insertModule p mi = over moduleTable (HashMap.insert p mi)

computeCombinedScopedInfoTable :: ModuleTable -> Scoped.InfoTable
computeCombinedScopedInfoTable mtab =
  mconcatMap (^. moduleInfoScopedModule . scopedModuleInfoTable) (HashMap.elems (mtab ^. moduleTable))

computeCombinedCoreInfoTable :: ModuleTable -> Core.InfoTable
computeCombinedCoreInfoTable mtab =
  mconcatMap (toCore . (^. moduleInfoCoreTable)) (HashMap.elems (mtab ^. moduleTable))

computeCombinedBuiltins :: ModuleTable -> BuiltinsTable
computeCombinedBuiltins mtab =
  mconcatMap
    (^. moduleInfoScopedModule . scopedModuleInfoTable . infoBuiltins)
    (HashMap.elems (mtab ^. moduleTable))

toCoreModuleTable :: HashMap ModuleId Core.InfoTable -> [ModuleInfo] -> Core.ModuleTable
toCoreModuleTable imports modules =
  Core.ModuleTable
    . HashMap.fromList
    . map (\md -> (md ^. Core.moduleId, md))
    . map (toCoreModule imports)
    $ modules

toCoreModule :: HashMap ModuleId Core.InfoTable -> ModuleInfo -> Core.Module
toCoreModule imports ModuleInfo {..} =
  Core.Module
    { _moduleId = mid,
      _moduleInfoTable = toCore _moduleInfoCoreTable,
      _moduleImports = _moduleInfoInternalModule ^. internalModuleImports,
      _moduleImportsTable = fromJust $ HashMap.lookup mid imports,
      _moduleSHA256 = Just _moduleInfoSHA256
    }
  where
    mid = _moduleInfoInternalModule ^. internalModuleId
