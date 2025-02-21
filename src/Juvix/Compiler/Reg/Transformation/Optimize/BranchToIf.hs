module Juvix.Compiler.Reg.Transformation.Optimize.BranchToIf where

import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

convertBranchToIf' :: (BoolOp -> Value -> Value -> Bool) -> Module -> Module
convertBranchToIf' f = mapT (const goFun)
  where
    goFun :: Code -> Code
    goFun =
      snd
        . runIdentity
        . recurseB
          BackwardRecursorSig
            { _backwardFun = \is () _ -> return ((), go is),
              _backwardAdjust = id
            }
          mempty

    go :: Code -> Code
    go = \case
      binop@(Binop InstrBinop {..}) : Branch InstrBranch {..} : is'
        | OpBool op <- _instrBinopOpcode,
          f op _instrBinopArg1 _instrBinopArg2,
          VRef r <- _instrBranchValue,
          r == _instrBinopResult,
          r `notElem` getValueRefs binop ->
            binop
              : If
                InstrIf
                  { _instrIfOp = op,
                    _instrIfArg1 = _instrBinopArg1,
                    _instrIfArg2 = _instrBinopArg2,
                    _instrIfOutVar = _instrBranchOutVar,
                    _instrIfTrue = _instrBranchTrue,
                    _instrIfFalse = _instrBranchFalse
                  }
              : is'
      is -> is

convertBranchToIf :: Module -> Module
convertBranchToIf = convertBranchToIf' (\_ _ _ -> True)

convertBranchOnZeroToIf :: Module -> Module
convertBranchOnZeroToIf = convertBranchToIf' check
  where
    check :: BoolOp -> Value -> Value -> Bool
    check op arg1 arg2 = case op of
      OpEq
        | ValConst (ConstInt 0) <- arg1 -> True
        | ValConst (ConstInt 0) <- arg2 -> True
      _ -> False
