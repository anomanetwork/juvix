module Juvix.Compiler.Store.Backend.Core
  ( module Juvix.Compiler.Store.Backend.Core,
    module Juvix.Compiler.Store.Backend.Module,
    module Juvix.Compiler.Store.Core.Data.InfoTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Backend.Module
import Juvix.Compiler.Store.Core.Data.InfoTable
import Juvix.Compiler.Store.Internal.Language qualified as Internal
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

type Module = Module' InfoTable

type ModuleTable = ModuleTable' InfoTable

fromModuleInfo :: (Member (Reader EntryPoint) r) => Store.ModuleInfo -> Sem r Module
fromModuleInfo Store.ModuleInfo {..} = do
  ep <- ask
  return
    Module
      { _moduleId = _moduleInfoInternalModule ^. Internal.internalModuleId,
        _moduleInfoTable = _moduleInfoCoreTable,
        _moduleImports = _moduleInfoInternalModule ^. Internal.internalModuleImports,
        _moduleOptions = fromEntryPoint ep,
        _moduleSHA256 = _moduleInfoSHA256
      }

fromModuleInfoTable :: (Member (Reader EntryPoint) r) => Store.ModuleTable -> Sem r ModuleTable
fromModuleInfoTable mt = do
  let mis = HashMap.elems (mt ^. Store.moduleTable)
  mds <- mapM fromModuleInfo mis
  return
    . ModuleTable
    . HashMap.fromList
    . map (\m -> (m ^. moduleId, m))
    $ mds
