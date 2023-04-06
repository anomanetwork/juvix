module Juvix.Compiler.Core.Extra.Value where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Value

toValue :: InfoTable -> Node -> Value
toValue tab = \case
  NCst Constant {..} -> ValueConstant _constantValue
  NCtr Constr {..} ->
    ValueConstrApp
      ConstrApp
        { _constrAppName = ci ^. constructorName,
          _constrAppFixity = ci ^. constructorFixity,
          _constrAppArgs = map (toValue tab) (drop paramsNum _constrArgs)
        }
    where
      ci = lookupConstructorInfo tab _constrTag
      ii = lookupInductiveInfo tab (ci ^. constructorInductive)
      paramsNum = length (ii ^. inductiveParams)
  NLam {} -> ValueFun
  _ -> impossible
