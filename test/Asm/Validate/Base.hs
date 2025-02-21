module Asm.Validate.Base where

import Base
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Transformation.Validate
import Juvix.Compiler.Asm.Translation.FromSource

asmValidateErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
asmValidateErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right md -> do
      step "Validate"
      case validate' md of
        Just _ -> assertBool "" True
        Nothing -> assertFailure "no error"

parseFile :: Path Abs File -> IO (Either MegaparsecError Module)
parseFile f = do
  s <- readFile f
  return (runParser f s)
