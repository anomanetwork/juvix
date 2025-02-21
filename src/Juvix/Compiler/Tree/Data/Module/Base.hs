module Juvix.Compiler.Tree.Data.Module.Base
  ( module Juvix.Compiler.Tree.Data.Module.Base,
    module Juvix.Compiler.Core.Data.Module.Base,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Base

type Module'' a e = Module' (InfoTable' a e)

type ModuleTable'' a e = ModuleTable' (InfoTable' a e)

lookupFunInfo' :: Module'' a e -> Symbol -> Maybe (FunctionInfo' a e)
lookupFunInfo' md sym =
  lookupTabFunInfo' (md ^. moduleInfoTable) sym
    <|> lookupTabFunInfo' (md ^. moduleImportsTable) sym

lookupConstrInfo' :: Module'' a e -> Tag -> Maybe ConstructorInfo
lookupConstrInfo' md tag =
  lookupTabConstrInfo' (md ^. moduleInfoTable) tag
    <|> lookupTabConstrInfo' (md ^. moduleImportsTable) tag

lookupInductiveInfo' :: Module'' a e -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' md sym =
  lookupTabInductiveInfo' (md ^. moduleInfoTable) sym
    <|> lookupTabInductiveInfo' (md ^. moduleImportsTable) sym

lookupFunInfo :: Module'' a e -> Symbol -> FunctionInfo' a e
lookupFunInfo md sym = fromMaybe (error "invalid function symbol") (lookupFunInfo' md sym)

lookupConstrInfo :: Module'' a e -> Tag -> ConstructorInfo
lookupConstrInfo md tag = fromMaybe (error "invalid constructor tag") (lookupConstrInfo' md tag)

lookupInductiveInfo :: Module'' a e -> Symbol -> InductiveInfo
lookupInductiveInfo md sym = fromMaybe (error "invalid inductive symbol") (lookupInductiveInfo' md sym)
