module Juvix.Compiler.Tree.Transformation.Generic.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Data.Module.Base
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Options

mapFunctionsM :: (Monad m) => (FunctionInfo' a e -> m (FunctionInfo' a e)) -> Module'' a e -> m (Module'' a e)
mapFunctionsM = overM (moduleInfoTable . infoFunctions) . mapM

mapInductivesM :: (Monad m) => (InductiveInfo -> m InductiveInfo) -> Module'' a e -> m (Module'' a e)
mapInductivesM = overM (moduleInfoTable . infoInductives) . mapM

mapConstructorsM :: (Monad m) => (ConstructorInfo -> m ConstructorInfo) -> Module'' a e -> m (Module'' a e)
mapConstructorsM = overM (moduleInfoTable . infoConstrs) . mapM

mapFunctions :: (FunctionInfo' a e -> FunctionInfo' a e) -> Module'' a e -> Module'' a e
mapFunctions = over (moduleInfoTable . infoFunctions) . fmap

mapInductives :: (InductiveInfo -> InductiveInfo) -> Module'' a e -> Module'' a e
mapInductives = over (moduleInfoTable . infoInductives) . fmap

mapConstructors :: (ConstructorInfo -> ConstructorInfo) -> Module'' a e -> Module'' a e
mapConstructors = over (moduleInfoTable . infoConstrs) . fmap

mapT :: (Symbol -> a -> a) -> Module'' a e -> Module'' a e
mapT f = over (moduleInfoTable . infoFunctions) (HashMap.mapWithKey (over functionCode . f))

mapT' :: forall a e r. (Symbol -> a -> Sem (InfoTableBuilder' a e ': r) a) -> Module'' a e -> Sem r (Module'' a e)
mapT' f md =
  fmap fst $
    runInfoTableBuilder md $
      mapM_
        (\(sym, fi) -> overM functionCode (f sym) fi >>= registerFunction' @a @e)
        (HashMap.toList (md ^. moduleInfoTable . infoFunctions))

walkT :: (Applicative f) => (Symbol -> a -> f ()) -> Module'' a e -> f ()
walkT f md = for_ (HashMap.toList (md ^. moduleInfoTable . infoFunctions)) (\(k, v) -> f k (v ^. functionCode))

withOptimizationLevel :: (Member (Reader Options) r) => Int -> (Module'' a e -> Sem r (Module'' a e)) -> Module'' a e -> Sem r (Module'' a e)
withOptimizationLevel n f md = do
  l <- asks (^. optOptimizationLevel)
  if
      | l >= n -> f md
      | otherwise -> return md

withOptimizationLevel' :: (Member (Reader Options) r) => Module'' a e -> Int -> (Module'' a e -> Sem r (Module'' a e)) -> Sem r (Module'' a e)
withOptimizationLevel' md n f = withOptimizationLevel n f md
