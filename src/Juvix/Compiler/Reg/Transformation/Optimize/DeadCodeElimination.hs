module Juvix.Compiler.Reg.Transformation.Optimize.DeadCodeElimination where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

removeDeadAssignments :: Module -> Module
removeDeadAssignments = mapT (const goFun)
  where
    goFun :: Code -> Code
    goFun =
      snd
        . runIdentity
        . recurseB
          BackwardRecursorSig
            { _backwardFun = \is a as -> return (go is a as),
              _backwardAdjust = id
            }
          mempty

    -- The accumulator contains live variables
    go :: Code -> HashSet VarRef -> [HashSet VarRef] -> (HashSet VarRef, Code)
    go is live lives = case is of
      Assign InstrAssign {..} : is'
        | VRef r <- _instrAssignValue,
          _instrAssignResult == r ->
            (live, is')
      instr : is' -> case getResultVar instr of
        Just var
          | not (HashSet.member var liveVars) ->
              (liveVars, is')
        _ ->
          (liveVars', instr : is')
        where
          liveVars' = updateInstrLiveVars instr liveVars
          liveVars = computeBackwardLiveVars instr live lives
      [] ->
        (live, [])
