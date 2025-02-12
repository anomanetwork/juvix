module Juvix.Compiler.Asm.Transformation.Base
  ( module Juvix.Compiler.Asm.Transformation.Base,
    module Juvix.Compiler.Asm.Data.Module,
    module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Language,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language

liftCodeTransformation :: (Monad m) => (Code -> m Code) -> FunctionInfo -> m FunctionInfo
liftCodeTransformation f fi = do
  code <- f (fi ^. functionCode)
  return fi {_functionCode = code}

liftFunctionTransformation :: (Monad m) => (FunctionInfo -> m FunctionInfo) -> Module -> m Module
liftFunctionTransformation f md = do
  fns <- mapM f (md ^. moduleInfoTable . infoFunctions)
  return $ over moduleInfoTable (set infoFunctions fns) md

runTransformation :: (Module -> Sem '[Error AsmError] Module) -> Module -> Either AsmError Module
runTransformation trans tab =
  case run $ runError $ trans tab of
    Left err -> Left err
    Right tab' -> Right tab'
