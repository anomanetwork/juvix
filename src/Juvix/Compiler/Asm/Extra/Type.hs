module Juvix.Compiler.Asm.Extra.Type
  ( module Juvix.Compiler.Asm.Extra.Type,
    module Juvix.Compiler.Tree.Extra.Type,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Type

unifyTypes'' :: forall t e r. (Member (Error AsmError) r) => Maybe Location -> Module'' t e -> Type -> Type -> Sem r Type
unifyTypes'' loc tab ty1 ty2 = mapError toAsmError $ unifyTypes' loc tab ty1 ty2
  where
    toAsmError :: TreeError -> AsmError
    toAsmError TreeError {..} =
      AsmError
        { _asmErrorLoc = _treeErrorLoc,
          _asmErrorMsg = _treeErrorMsg
        }
