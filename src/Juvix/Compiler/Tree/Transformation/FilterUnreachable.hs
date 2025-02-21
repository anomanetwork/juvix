module Juvix.Compiler.Tree.Transformation.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.CallGraph
import Juvix.Compiler.Tree.Data.Module
import Juvix.Prelude

filterUnreachable' :: InfoTable -> InfoTable
filterUnreachable' tab
  | isJust (tab ^. infoMainFunction) =
      over infoFunctions (HashMap.filterWithKey (const . isReachable graph)) tab
  | otherwise = tab
  where
    graph = createCallGraph tab

filterUnreachable :: Module -> Module
filterUnreachable = over moduleInfoTable filterUnreachable'
