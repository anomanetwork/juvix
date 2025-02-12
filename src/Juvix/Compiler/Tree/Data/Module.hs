module Juvix.Compiler.Tree.Data.Module
  ( module Juvix.Compiler.Tree.Data.Module,
    module Juvix.Compiler.Tree.Data.InfoTable,
    module Juvix.Compiler.Core.Data.Module.Base,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Language.Base

type Module = Module' InfoTable

type ModuleTable = ModuleTable' InfoTable

lookupInductiveInfo' :: Module -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' Module {..} sym =
  lookupTabInductiveInfo' _moduleInfoTable sym
    <|> lookupTabInductiveInfo' _moduleImportsTable sym

lookupConstrInfo' :: Module -> Tag -> Maybe ConstructorInfo
lookupConstrInfo' Module {..} tag =
  lookupTabConstrInfo' _moduleInfoTable tag
    <|> lookupTabConstrInfo' _moduleImportsTable tag

lookupFunInfo' :: Module -> Symbol -> Maybe FunctionInfo
lookupFunInfo' Module {..} sym =
  lookupTabFunInfo' _moduleInfoTable sym
    <|> lookupTabFunInfo' _moduleImportsTable sym

lookupInductiveInfo :: Module -> Symbol -> InductiveInfo
lookupInductiveInfo m sym = fromJust (lookupInductiveInfo' m sym)

lookupConstrInfo :: Module -> Tag -> ConstructorInfo
lookupConstrInfo m tag = fromJust (lookupConstrInfo' m tag)

lookupFunInfo :: Module -> Symbol -> FunctionInfo
lookupFunInfo m sym = fromJust (lookupFunInfo' m sym)
