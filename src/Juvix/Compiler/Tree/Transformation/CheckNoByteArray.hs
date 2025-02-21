module Juvix.Compiler.Tree.Transformation.CheckNoByteArray where

import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

checkNoByteArray :: forall r. (Member (Error TreeError) r) => Module -> Sem r ()
checkNoByteArray = walkT checkNode
  where
    checkNode :: Symbol -> Node -> Sem r ()
    checkNode _ = \case
      ByteArray NodeByteArray {..} -> case _nodeByteArrayOpcode of
        OpByteArrayLength -> unsupportedErr "OpByteArrayLength"
        OpByteArrayFromListUInt8 -> unsupportedErr "OpByteArrayFromListUInt8"
        where
          unsupportedErr :: Text -> Sem r ()
          unsupportedErr opName =
            throw
              TreeError
                { _treeErrorMsg = opName <> " is unsupported",
                  _treeErrorLoc = _nodeByteArrayInfo ^. nodeInfoLocation
                }
      _ -> return ()
