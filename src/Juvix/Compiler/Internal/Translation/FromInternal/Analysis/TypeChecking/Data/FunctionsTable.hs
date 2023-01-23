module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

newtype FunctionsTable = FunctionsTable
  { _functionsTable :: HashMap FunctionName Expression
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''FunctionsTable

askFunctionDef :: (Member (Reader FunctionsTable) r) => FunctionName -> Sem r (Maybe Expression)
askFunctionDef f = asks (^. functionsTable . at f)
