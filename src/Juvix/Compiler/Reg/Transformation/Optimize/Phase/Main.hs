module Juvix.Compiler.Reg.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Reg.Transformation.Optimize.ConstantPropagation
import Juvix.Compiler.Reg.Transformation.Optimize.CopyPropagation
import Juvix.Compiler.Reg.Transformation.Optimize.DeadCodeElimination

optimize' :: Options -> Module -> Module
optimize' Options {..} =
  compose
    (2 * _optOptimizationLevel)
    ( copyPropagate
        . constantPropagate
        . removeDeadAssignments
    )

optimize :: (Member (Reader Options) r) => Module -> Sem r Module
optimize tab = do
  opts <- ask
  return $ optimize' opts tab
