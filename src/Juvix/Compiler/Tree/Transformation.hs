module Juvix.Compiler.Tree.Transformation
  ( module Juvix.Compiler.Tree.Transformation.Base,
    module Juvix.Compiler.Tree.Transformation,
    module Juvix.Compiler.Tree.Data.TransformationId,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.TransformationId
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Transformation.Apply
import Juvix.Compiler.Tree.Transformation.Base
import Juvix.Compiler.Tree.Transformation.CheckNoAnoma
import Juvix.Compiler.Tree.Transformation.CheckNoByteArray
import Juvix.Compiler.Tree.Transformation.FilterUnreachable
import Juvix.Compiler.Tree.Transformation.IdentityTrans
import Juvix.Compiler.Tree.Transformation.Optimize.ConvertUnaryCalls
import Juvix.Compiler.Tree.Transformation.Optimize.Phase.Main
import Juvix.Compiler.Tree.Transformation.Validate

applyTransformations :: forall r. (Members '[Error JuvixError, Reader Options] r) => [TransformationId] -> Module -> Sem r Module
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> Module -> Sem r Module
    appTrans = \case
      IdentityTrans -> return . identity
      IdentityU -> return . identityU
      IdentityD -> return . identityD
      ConvertUnaryCalls -> return . convertUnaryCalls
      OptPhaseMain -> optimize
      Apply -> return . computeApply
      FilterUnreachable -> return . filterUnreachable
      Validate -> mapError (JuvixError @TreeError) . validate
      CheckNoAnoma -> \tbl' -> mapError (JuvixError @TreeError) (checkNoAnoma tbl') $> tbl'
      CheckNoByteArray -> \tbl' -> mapError (JuvixError @TreeError) (checkNoByteArray tbl') $> tbl'
