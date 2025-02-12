module Juvix.Compiler.Tree.Extra.Rep where

import Juvix.Compiler.Tree.Data.Module.Base
import Juvix.Compiler.Tree.Language.Base

isRecord :: InductiveInfo -> Bool
isRecord InductiveInfo {..} = length _inductiveConstructors == 1

isInductiveRecord :: Module'' a e -> Symbol -> Bool
isInductiveRecord md sym = isRecord (lookupInductiveInfo md sym)

isConstrRecord :: Module'' a e -> Tag -> Bool
isConstrRecord md tag =
  isInductiveRecord md (lookupConstrInfo md tag ^. constructorInductive)

isTabInductiveRecord :: InfoTable' a e -> Symbol -> Bool
isTabInductiveRecord tab sym = isRecord (lookupTabInductiveInfo tab sym)

isTabConstrRecord :: InfoTable' a e -> Tag -> Bool
isTabConstrRecord tab tag =
  isTabInductiveRecord tab (lookupTabConstrInfo tab tag ^. constructorInductive)
