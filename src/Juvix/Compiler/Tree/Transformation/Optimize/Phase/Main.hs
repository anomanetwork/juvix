module Juvix.Compiler.Tree.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Tree.Transformation.Base
import Juvix.Compiler.Tree.Transformation.Optimize.ConvertUnaryCalls

optimize :: (Member (Reader Options) r) => InfoTable -> Sem r InfoTable
optimize =
  withOptimizationLevel 1 $
    return . convertUnaryCalls
