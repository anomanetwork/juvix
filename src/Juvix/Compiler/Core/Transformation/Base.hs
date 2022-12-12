module Juvix.Compiler.Core.Transformation.Base
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Core.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Language

mapIdents :: (IdentifierInfo -> IdentifierInfo) -> InfoTable -> InfoTable
mapIdents = over infoIdentifiers . fmap

mapInductives :: (InductiveInfo -> InductiveInfo) -> InfoTable -> InfoTable
mapInductives = over infoInductives . fmap

mapConstructors :: (ConstructorInfo -> ConstructorInfo) -> InfoTable -> InfoTable
mapConstructors = over infoConstructors . fmap

mapAxioms :: (AxiomInfo -> AxiomInfo) -> InfoTable -> InfoTable
mapAxioms = over infoAxioms . fmap

mapT :: (Symbol -> Node -> Node) -> InfoTable -> InfoTable
mapT f tab = tab {_identContext = HashMap.mapWithKey f (tab ^. identContext)}

mapT' :: (Symbol -> Node -> Sem (InfoTableBuilder ': r) Node) -> InfoTable -> Sem r InfoTable
mapT' f tab =
  fmap fst $
    runInfoTableBuilder tab $
      mapM_
        (\(k, v) -> f k v >>= registerIdentNode k)
        (HashMap.toList (tab ^. identContext))

walkT :: Applicative f => (Symbol -> Node -> f ()) -> InfoTable -> f ()
walkT f tab = for_ (HashMap.toList (tab ^. identContext)) (uncurry f)
