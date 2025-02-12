module Juvix.Compiler.Reg.Translation.Blocks.FromReg where

import Juvix.Compiler.Reg.Data.Blocks.Module
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Compiler.Reg.Language.Blocks

fromReg :: Reg.Module -> Module
fromReg md =
  Module
    { _moduleId = md ^. moduleId,
      _moduleInfoTable = tab,
      _moduleImports = md ^. moduleImports,
      _moduleImportsTable = mempty
    }
  where
    tab = over infoFunctions (fmap (over functionCode goCode)) (computeCombinedInfoTable md)

    goCode :: Reg.Code -> Block
    goCode = fromMaybe emptyBlock . goCode'

    goCode' :: Reg.Code -> Maybe Block
    goCode' = \case
      [] -> Nothing
      i : is -> Just $ case i of
        Reg.Binop x -> over blockBody (Binop x :) (goCode is)
        Reg.Unop x -> over blockBody (Unop x :) (goCode is)
        Reg.Cairo x -> over blockBody (Cairo x :) (goCode is)
        Reg.Assign x -> over blockBody (Assign x :) (goCode is)
        Reg.Alloc x -> over blockBody (Alloc x :) (goCode is)
        Reg.AllocClosure x -> over blockBody (AllocClosure x :) (goCode is)
        Reg.ExtendClosure x -> mkBlock (ExtendClosure x)
        Reg.Call x -> mkBlock (Call x)
        Reg.TailCall x -> mkBlock (TailCall x)
        Reg.Return x -> mkBlock (Return x)
        Reg.If x -> mkBlock (If (fmap goCode x))
        Reg.Branch x -> mkBlock (Branch (fmap goCode x))
        Reg.Case x -> mkBlock (Case (fmap goCode x))
        Reg.CallClosures {} -> impossible
        Reg.TailCallClosures {} -> impossible
        Reg.Assert x -> over blockBody (Assert x :) (goCode is)
        Reg.Trace x -> over blockBody (Trace x :) (goCode is)
        Reg.Dump -> over blockBody (Dump :) (goCode is)
        Reg.Failure x -> over blockBody (Failure x :) (goCode is)
        Reg.Prealloc {} -> goCode is
        Reg.Nop -> goCode is
        Reg.Block Reg.InstrBlock {..} -> goCode (_instrBlockCode ++ is)
        where
          mkBlock :: FinalInstruction -> Block
          mkBlock i' =
            emptyBlock
              { _blockFinal = Just i',
                _blockNext = goCode' is
              }
