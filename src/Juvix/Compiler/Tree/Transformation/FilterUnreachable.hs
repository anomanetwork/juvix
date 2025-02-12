module Juvix.Compiler.Tree.Transformation.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.CallGraph
import Juvix.Compiler.Tree.Data.Module
import Juvix.Prelude

filterUnreachable :: Module -> Module
filterUnreachable md
  | isJust (md ^. moduleInfoTable . infoMainFunction) =
      over (moduleInfoTable . infoFunctions) (HashMap.filterWithKey (const . isReachable graph)) md
  | otherwise = md
  where
    graph = createCallGraph (md ^. moduleInfoTable)
