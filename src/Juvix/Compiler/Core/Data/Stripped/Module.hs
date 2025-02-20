module Juvix.Compiler.Core.Data.Stripped.Module
  ( module Juvix.Compiler.Core.Data.Stripped.Module,
    module Juvix.Compiler.Core.Data.Stripped.InfoTable,
    module Juvix.Compiler.Core.Data.Module.Base,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.Stripped.InfoTable
import Juvix.Compiler.Core.Language.Stripped

type Module = Module' InfoTable

type ModuleTable = ModuleTable' InfoTable

lookupInductiveInfo' :: Module -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' Module {..} sym =
  lookupTabInductiveInfo' _moduleInfoTable sym
    <|> lookupTabInductiveInfo' _moduleImportsTable sym

lookupConstructorInfo' :: Module -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' Module {..} tag =
  lookupTabConstructorInfo' _moduleInfoTable tag
    <|> lookupTabConstructorInfo' _moduleImportsTable tag

lookupFunInfo' :: Module -> Symbol -> Maybe FunctionInfo
lookupFunInfo' Module {..} sym =
  lookupTabFunInfo' _moduleInfoTable sym
    <|> lookupTabFunInfo' _moduleImportsTable sym

impossibleSymbolNotFound :: (HasCallStack) => Symbol -> a
impossibleSymbolNotFound sym = impossibleError ("Could not find symbol " <> show sym)

lookupInductiveInfo :: Module -> Symbol -> InductiveInfo
lookupInductiveInfo m sym = fromMaybe (impossibleSymbolNotFound sym) (lookupInductiveInfo' m sym)

lookupConstructorInfo :: Module -> Tag -> ConstructorInfo
lookupConstructorInfo m tag = fromJust (lookupConstructorInfo' m tag)

lookupFunInfo :: Module -> Symbol -> FunctionInfo
lookupFunInfo m sym = fromMaybe (impossibleSymbolNotFound sym) (lookupFunInfo' m sym)
