module Juvix.Compiler.Reg.Transformation.Optimize.ConstantPropagation where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Tree.Evaluator.Builtins

type VarMap = HashMap VarRef Constant

constantPropagate :: Module -> Module
constantPropagate = mapT (const goFun)
  where
    goFun :: Code -> Code
    goFun =
      snd
        . runIdentity
        . recurseF
          ForwardRecursorSig
            { _forwardFun = \i acc -> return (go i acc),
              _forwardCombine = combine
            }
          mempty

    go :: Instruction -> VarMap -> (VarMap, Instruction)
    go instr mpv = case instr' of
      Assign InstrAssign {..}
        | ValConst c <- _instrAssignValue ->
            (HashMap.insert _instrAssignResult c mpv', instr')
      Binop InstrBinop {..}
        | ValConst c1 <- _instrBinopArg1,
          ValConst c2 <- _instrBinopArg2 ->
            case evalBinop' _instrBinopOpcode c1 c2 of
              Left _ -> (mpv', instr')
              Right c ->
                ( HashMap.insert _instrBinopResult c mpv',
                  Assign
                    InstrAssign
                      { _instrAssignResult = _instrBinopResult,
                        _instrAssignValue = ValConst c
                      }
                )
      _ ->
        (mpv', instr')
      where
        instr' = overValueRefs' (adjustVarRef mpv) instr
        mpv' = maybe mpv (`HashMap.delete` mpv) (getResultVar instr)

    adjustVarRef :: VarMap -> VarRef -> Value
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> VRef vref
      VarGroupLocal -> maybe (VRef vref) ValConst (HashMap.lookup vref mpv)

    combine :: Instruction -> NonEmpty VarMap -> (VarMap, Instruction)
    combine instr mpvs = case instr of
      Branch InstrBranch {..}
        | ValConst (ConstBool True) <- _instrBranchValue ->
            (mpv1, Block $ InstrBlock _instrBranchTrue)
        | ValConst (ConstBool False) <- _instrBranchValue ->
            (mpv2, Block $ InstrBlock _instrBranchFalse)
        where
          (mpv1, mpv2) = case mpvs of
            mpv1' :| [mpv2'] -> (mpv1', mpv2')
            _ -> impossible
      _ ->
        (combineMaps mpvs, instr)
