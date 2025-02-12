module Juvix.Compiler.Asm.Data.CallGraph where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language

-- | Call graph type
type CallGraph = DependencyInfo Symbol

-- | Compute the call graph
createCallGraph :: (Member (Error AsmError) r) => Module -> Sem r CallGraph
createCallGraph md = do
  graph <- createCallGraphMap md
  return $ createDependencyInfo graph startVertices
  where
    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = maybe [] singleton (md ^. moduleInfoTable . infoMainFunction)

createCallGraphMap :: (Member (Error AsmError) r) => Module -> Sem r (HashMap Symbol (HashSet Symbol))
createCallGraphMap md =
  mapM
    (\FunctionInfo {..} -> getFunSymbols md _functionCode)
    (md ^. moduleInfoTable . infoFunctions)

getFunSymbols :: (Member (Error AsmError) r) => Module -> Code -> Sem r (HashSet Symbol)
getFunSymbols md code = foldS sig code mempty
  where
    sig :: FoldSig StackInfo r (HashSet Symbol)
    sig =
      FoldSig
        { _foldModule = md,
          _foldAdjust = const mempty,
          _foldInstr = \_ CmdInstr {..} acc -> return $ goInstr acc _cmdInstrInstruction,
          _foldBranch = \_ _ a1 a2 a3 -> return $ a1 <> a2 <> a3,
          _foldCase = \_ _ as ma a -> return $ mconcat as <> fromMaybe mempty ma <> a,
          _foldSave = \_ _ a1 a2 -> return $ a1 <> a2
        }

    goInstr :: HashSet Symbol -> Instruction -> HashSet Symbol
    goInstr syms = \case
      AllocClosure InstrAllocClosure {..} -> HashSet.insert _allocClosureFunSymbol syms
      Call InstrCall {..} -> goCallType syms _callType
      TailCall InstrCall {..} -> goCallType syms _callType
      _ -> syms

    goCallType :: HashSet Symbol -> CallType -> HashSet Symbol
    goCallType syms = \case
      CallFun sym -> HashSet.insert sym syms
      CallClosure -> syms
