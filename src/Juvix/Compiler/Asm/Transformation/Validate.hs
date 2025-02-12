module Juvix.Compiler.Asm.Transformation.Validate where

import Juvix.Compiler.Asm.Transformation.Base

validateCode :: forall r. (Member (Error AsmError) r) => Module -> FunctionInfo -> Code -> Sem r Code
validateCode md fi code = do
  recurse sig (argumentsFromFunctionInfo fi) code
  return code
  where
    sig :: RecursorSig Memory r ()
    sig =
      RecursorSig
        { _recursorModule = md,
          _recurseInstr = \_ _ -> return (),
          _recurseBranch = \_ _ _ _ _ -> return (),
          _recurseCase = \_ _ _ _ _ -> return (),
          _recurseSave = \_ _ _ -> return ()
        }

validateFunction :: (Member (Error AsmError) r) => Module -> FunctionInfo -> Sem r FunctionInfo
validateFunction md fi = liftCodeTransformation (validateCode md fi) fi

validate :: (Member (Error AsmError) r) => Module -> Sem r Module
validate md = liftFunctionTransformation (validateFunction md) md

validate' :: Module -> Maybe AsmError
validate' md =
  case run $ runError $ validate md of
    Left err -> Just err
    _ -> Nothing
