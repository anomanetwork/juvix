module Juvix.Compiler.Asm.Transformation.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.CallGraph
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error
import Juvix.Prelude

filterUnreachable :: (Member (Error AsmError) r) => Module -> Sem r Module
filterUnreachable md
  | isJust (md ^. moduleInfoTable . infoMainFunction) = do
      graph <- createCallGraph md
      return $ over (moduleInfoTable . infoFunctions) (HashMap.filterWithKey (const . isReachable graph)) md
  | otherwise =
      return md
