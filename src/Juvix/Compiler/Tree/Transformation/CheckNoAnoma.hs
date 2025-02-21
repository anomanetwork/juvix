module Juvix.Compiler.Tree.Transformation.CheckNoAnoma where

import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

checkNoAnoma :: forall r. (Member (Error TreeError) r) => Module -> Sem r ()
checkNoAnoma = walkT checkNode
  where
    checkNode :: Symbol -> Node -> Sem r ()
    checkNode _ = \case
      Anoma NodeAnoma {..} ->
        throw
          TreeError
            { _treeErrorMsg = show _nodeAnomaOpcode <> " is unsupported",
              _treeErrorLoc = _nodeAnomaInfo ^. nodeInfoLocation
            }
      _ -> return ()
