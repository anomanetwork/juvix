module Juvix.Compiler.Reg.Transformation.SSA where

import Data.Functor.Identity
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid
import Juvix.Compiler.Reg.Data.IndexMap (IndexMap)
import Juvix.Compiler.Reg.Data.IndexMap qualified as IndexMap
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

computeFunctionSSA :: Code -> Code
computeFunctionSSA =
  snd
    . runIdentity
    . recurseF
      ForwardRecursorSig
        { _forwardFun = \i acc -> return (go i acc),
          _forwardCombine = combine
        }
      mempty
  where
    go :: Instruction -> IndexMap VarRef -> (IndexMap VarRef, Instruction)
    go instr mp = case getResultVar instr' of
      Just vref -> (mp', updateLiveVars' (adjustVarRef' mp') (setResultVar instr' (mkVarRef VarGroupLocal idx)))
        where
          (idx, mp') = IndexMap.assign mp vref
      Nothing -> (mp, updateLiveVars' (adjustVarRef' mp) instr')
      where
        instr' = overValueRefs (adjustVarRef mp) instr

    -- For branches, when necessary we insert assignments unifying the renamed
    -- output variables into a single output variable for both branches.
    combine :: Instruction -> NonEmpty (IndexMap VarRef) -> (IndexMap VarRef, Instruction)
    combine instr mps = case instr of
      Branch InstrBranch {..} -> case mps of
        mp1 :| mp2 : []
          | isNothing _instrBranchOutVar ->
              (mp, instr)
          | idx1 == idx2 ->
              ( mp,
                Branch
                  InstrBranch
                    { _instrBranchOutVar = Just $ mkVarRef VarGroupLocal idx1,
                      ..
                    }
              )
          | otherwise ->
              ( mp',
                Branch
                  InstrBranch
                    { _instrBranchTrue = assignInBranch _instrBranchTrue idx' idx1,
                      _instrBranchFalse = assignInBranch _instrBranchFalse idx' idx2,
                      _instrBranchOutVar = Just $ mkVarRef VarGroupLocal idx',
                      ..
                    }
              )
          where
            var = fromJust _instrBranchOutVar
            idx1 = IndexMap.lookup mp1 var
            idx2 = IndexMap.lookup mp2 var
            mp = IndexMap.combine mp1 mp2
            (idx', mp') = IndexMap.assign mp var
        _ -> impossible
      Case InstrCase {..} -> case mps of
        mp0 :| mps'
          | isNothing _instrCaseOutVar ->
              (mp, instr)
          | all (== head idxs) (NonEmpty.tail idxs) ->
              ( mp,
                Case
                  InstrCase
                    { _instrCaseOutVar = Just $ mkVarRef VarGroupLocal (head idxs),
                      ..
                    }
              )
          | otherwise ->
              ( mp',
                Case
                  InstrCase
                    { _instrCaseBranches = brs',
                      _instrCaseDefault = def',
                      _instrCaseOutVar = Just $ mkVarRef VarGroupLocal idx',
                      ..
                    }
              )
          where
            var = fromJust _instrCaseOutVar
            idxs = fmap (flip IndexMap.lookup var) mps
            mp = foldr IndexMap.combine mp0 mps'
            (idx', mp') = IndexMap.assign mp var
            n = length _instrCaseBranches
            brs' = zipWithExact updateBranch _instrCaseBranches (take n (toList idxs))
            def' = fmap (\is -> assignInBranch is idx' (last idxs)) _instrCaseDefault

            updateBranch :: CaseBranch -> Index -> CaseBranch
            updateBranch br idx =
              over caseBranchCode (\is -> assignInBranch is idx' idx) br
      _ -> impossible

    adjustVarRef :: IndexMap VarRef -> VarRef -> VarRef
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> vref
      VarGroupLocal -> mkVarRef VarGroupLocal (IndexMap.lookup mpv vref)

    adjustVarRef' :: IndexMap VarRef -> VarRef -> Maybe VarRef
    adjustVarRef' mpv vref = case IndexMap.lookup' mpv vref of
      Just idx -> Just $ mkVarRef VarGroupLocal idx
      Nothing -> case vref ^. varRefGroup of
        VarGroupArgs -> Just vref
        VarGroupLocal -> Nothing

    assignInBranch :: Code -> Index -> Index -> Code
    assignInBranch is idx idx' =
      is
        ++ [ Assign
               InstrAssign
                 { _instrAssignResult = mkVarRef VarGroupLocal idx,
                   _instrAssignValue = VRef $ mkVarRef VarGroupLocal idx'
                 }
           ]

computeSSA :: Module -> Module
computeSSA = mapT (const computeFunctionSSA)

checkSSA :: Module -> Bool
checkSSA md = all (checkFun . (^. functionCode)) (md ^. moduleInfoTable . infoFunctions)
  where
    checkFun :: Code -> Bool
    checkFun is = getAll $ snd $ ifoldF check (mempty, All True) is
      where
        check :: (HashSet VarRef, All) -> Instruction -> (HashSet VarRef, All)
        check (refs, b) instr = case getResultVar instr of
          Just var -> (HashSet.insert var refs, b <> All (not (HashSet.member var refs)))
          Nothing -> (refs, b)
