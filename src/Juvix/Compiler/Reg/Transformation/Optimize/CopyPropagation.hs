module Juvix.Compiler.Reg.Transformation.Optimize.CopyPropagation where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

type VarMap = HashMap VarRef VarRef

copyPropagate :: Module -> Module
copyPropagate = mapT (const goFun)
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
        | VRef v <- _instrAssignValue ->
            (HashMap.insert _instrAssignResult v mpv', instr')
      _ ->
        (mpv', instr')
      where
        instr' = overValueRefs (adjustVarRef mpv) instr
        mpv' = maybe mpv (filterOutVars mpv) (getResultVar instr)

    filterOutVars :: VarMap -> VarRef -> VarMap
    filterOutVars mpv v = HashMap.delete v $ HashMap.filter (/= v) mpv

    adjustVarRef :: VarMap -> VarRef -> VarRef
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> vref
      VarGroupLocal -> fromMaybe vref $ HashMap.lookup vref mpv

    combine :: Instruction -> NonEmpty VarMap -> (VarMap, Instruction)
    combine instr mpvs = (mpv, instr')
      where
        mpv = combineMaps mpvs
        instr' = case instr of
          If x -> If $ over instrIfOutVar (fmap (adjustVarRef mpv)) x
          Branch x -> Branch $ over instrBranchOutVar (fmap (adjustVarRef mpv)) x
          Case x -> Case $ over instrCaseOutVar (fmap (adjustVarRef mpv)) x
          _ -> impossible
