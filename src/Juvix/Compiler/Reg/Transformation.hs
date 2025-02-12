module Juvix.Compiler.Reg.Transformation
  ( module Juvix.Compiler.Reg.Transformation.Base,
    module Juvix.Compiler.Reg.Transformation,
    module Juvix.Compiler.Reg.Data.TransformationId,
  )
where

import Juvix.Compiler.Reg.Data.TransformationId
import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Reg.Transformation.Cleanup
import Juvix.Compiler.Reg.Transformation.IdentityTrans
import Juvix.Compiler.Reg.Transformation.InitBranchVars
import Juvix.Compiler.Reg.Transformation.Optimize.BranchToIf
import Juvix.Compiler.Reg.Transformation.Optimize.ConstantPropagation
import Juvix.Compiler.Reg.Transformation.Optimize.CopyPropagation
import Juvix.Compiler.Reg.Transformation.Optimize.DeadCodeElimination
import Juvix.Compiler.Reg.Transformation.Optimize.Phase.Cairo qualified as Phase.Cairo
import Juvix.Compiler.Reg.Transformation.Optimize.Phase.Main qualified as Phase.Main
import Juvix.Compiler.Reg.Transformation.SSA

applyTransformations :: forall r. (Member (Reader Options) r) => [TransformationId] -> Module -> Sem r Module
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> Module -> Sem r Module
    appTrans = \case
      IdentityTrans -> return . identity
      Cleanup -> return . cleanup
      CleanupCairo -> return . cleanup' True
      SSA -> return . computeSSA
      InitBranchVars -> return . initBranchVars
      CopyPropagation -> return . copyPropagate
      ConstantPropagation -> return . constantPropagate
      DeadCodeElimination -> return . removeDeadAssignments
      BranchToIf -> return . convertBranchToIf
      BranchOnZeroToIf -> return . convertBranchOnZeroToIf
      OptPhaseMain -> Phase.Main.optimize
      OptPhaseCairo -> Phase.Cairo.optimize
