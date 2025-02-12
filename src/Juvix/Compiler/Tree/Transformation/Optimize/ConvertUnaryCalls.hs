module Juvix.Compiler.Tree.Transformation.Optimize.ConvertUnaryCalls where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Extra.Type
import Juvix.Compiler.Tree.Transformation.Base

-- | Replaces generic calls (with CallClosures) to unknown unary functions with
-- known non-function target types by direct closure calls (with Call)
convertUnaryCalls :: Module -> Module
convertUnaryCalls md = mapT convert md
  where
    convert :: Symbol -> Node -> Node
    convert sym = umapL (go argtys)
      where
        funInfo = lookupFunInfo md sym
        argtys
          | funInfo ^. functionArgsNum == 0 = []
          | otherwise = typeArgs (funInfo ^. functionType)

    go :: [Type] -> BinderList TempVar -> Node -> Node
    go argtys tmps node = case node of
      CallClosures ncl@NodeCallClosures {..}
        | length _nodeCallClosuresArgs == 1 ->
            case _nodeCallClosuresFun of
              MemRef NodeMemRef {..}
                | DRef (ArgRef OffsetRef {..}) <- _nodeMemRef,
                  isUnaryWithAtomicTarget (argtys !! _offsetRefOffset) ->
                    mkClosureCall ncl
                | DRef (TempRef (RefTemp OffsetRef {..})) <- _nodeMemRef,
                  isUnaryWithAtomicTarget (BL.lookupLevel _offsetRefOffset tmps ^. tempVarType) ->
                    mkClosureCall ncl
                | ConstrRef (Field {..}) <- _nodeMemRef,
                  constrInfo <- lookupConstrInfo md _fieldTag,
                  isUnaryWithAtomicTarget (typeArgs (constrInfo ^. constructorType) !! _fieldOffset) ->
                    mkClosureCall ncl
              _ -> node
      _ -> node

    mkClosureCall :: NodeCallClosures -> Node
    mkClosureCall NodeCallClosures {..} =
      Call
        NodeCall
          { _nodeCallInfo = _nodeCallClosuresInfo,
            _nodeCallType = CallClosure _nodeCallClosuresFun,
            _nodeCallArgs = toList _nodeCallClosuresArgs
          }

    isUnaryWithAtomicTarget :: Type -> Bool
    isUnaryWithAtomicTarget ty =
      length (typeArgs ty) == 1
        && isConcreteAtomType (typeTarget ty)
