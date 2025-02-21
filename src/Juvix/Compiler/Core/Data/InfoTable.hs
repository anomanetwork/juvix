module Juvix.Compiler.Core.Data.InfoTable
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Compiler.Core.Data.InfoTable.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Core.Data.InfoTable.Base
import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

type InfoTable = InfoTable' Node

type IdentifierInfo = IdentifierInfo' Node

type InductiveInfo = InductiveInfo' Node

type ConstructorInfo = ConstructorInfo' Node

type ParameterInfo = ParameterInfo' Node

type SpecialisationInfo = SpecialisationInfo' Node

nextSymbolId :: InfoTable -> Word
nextSymbolId tab =
  maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoIdentifiers)) ++ map (^. symbolId) (HashMap.keys (tab ^. infoInductives)))
    + 1

nextTagId :: InfoTable -> Word
nextTagId tab =
  maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstructors))) + 1

lookupTabInductiveInfo' :: InfoTable -> Symbol -> Maybe InductiveInfo
lookupTabInductiveInfo' tab sym = HashMap.lookup sym (tab ^. infoInductives)

lookupTabConstructorInfo' :: InfoTable -> Tag -> Maybe ConstructorInfo
lookupTabConstructorInfo' tab tag = HashMap.lookup tag (tab ^. infoConstructors)

lookupTabIdentifierInfo' :: InfoTable -> Symbol -> Maybe IdentifierInfo
lookupTabIdentifierInfo' tab sym = HashMap.lookup sym (tab ^. infoIdentifiers)

lookupTabIdentifierNode' :: InfoTable -> Symbol -> Maybe Node
lookupTabIdentifierNode' tab sym = HashMap.lookup sym (tab ^. identContext)

lookupTabSpecialisationInfo' :: InfoTable -> Symbol -> Maybe [SpecialisationInfo]
lookupTabSpecialisationInfo' tab sym = HashMap.lookup sym (tab ^. infoSpecialisations)

lookupTabSpecialisationInfo :: InfoTable -> Symbol -> [SpecialisationInfo]
lookupTabSpecialisationInfo tab sym = fromMaybe [] $ lookupTabSpecialisationInfo' tab sym

lookupTabInductiveInfo :: InfoTable -> Symbol -> InductiveInfo
lookupTabInductiveInfo tab sym = fromJust $ lookupTabInductiveInfo' tab sym

lookupTabConstructorInfo :: InfoTable -> Tag -> ConstructorInfo
lookupTabConstructorInfo tab tag = fromMaybe (error ("tag: " <> show tag)) $ lookupTabConstructorInfo' tab tag

lookupTabIdentifierInfo :: InfoTable -> Symbol -> IdentifierInfo
lookupTabIdentifierInfo tab sym = fromJust $ lookupTabIdentifierInfo' tab sym

lookupTabIdentifierNode :: InfoTable -> Symbol -> Node
lookupTabIdentifierNode tab sym = fromJust $ lookupTabIdentifierNode' tab sym

lookupTabBuiltinInductive :: InfoTable -> BuiltinInductive -> Maybe InductiveInfo
lookupTabBuiltinInductive tab b = (HashMap.!) (tab ^. infoInductives) . indSym <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsInductive b) (tab ^. infoBuiltins)

    indSym :: IdentKind -> Symbol
    indSym = \case
      IdentInd s -> s
      _ -> error "core infotable: expected inductive identifier"

lookupTabBuiltinConstructor :: InfoTable -> BuiltinConstructor -> Maybe ConstructorInfo
lookupTabBuiltinConstructor tab b = (HashMap.!) (tab ^. infoConstructors) . ctorTag <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsConstructor b) (tab ^. infoBuiltins)

    ctorTag :: IdentKind -> Tag
    ctorTag = \case
      IdentConstr t -> t
      _ -> error "core infotable: expected constructor identifier"

lookupTabBuiltinFunction :: InfoTable -> BuiltinFunction -> Maybe IdentifierInfo
lookupTabBuiltinFunction tab b = (HashMap.!) (tab ^. infoIdentifiers) . funSym <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsFunction b) (tab ^. infoBuiltins)

    funSym :: IdentKind -> Symbol
    funSym = \case
      IdentFun s -> s
      _ -> error "core infotable: expected function identifier"

identName' :: InfoTable -> Symbol -> Text
identName' tab sym = lookupTabIdentifierInfo tab sym ^. identifierName

typeName' :: InfoTable -> Symbol -> Text
typeName' tab sym = lookupTabInductiveInfo tab sym ^. inductiveName

identNames' :: InfoTable -> HashSet Text
identNames' = HashSet.fromList . identNamesList'

identNamesList' :: InfoTable -> [Text]
identNamesList' tab =
  map (^. identifierName) (HashMap.elems (tab ^. infoIdentifiers))
    ++ map (^. constructorName) (HashMap.elems (tab ^. infoConstructors))
    ++ map (^. inductiveName) (HashMap.elems (tab ^. infoInductives))

freshIdentName' :: InfoTable -> Text -> Text
freshIdentName' tab = freshName (identNames' tab)

-- | Prunes the orphaned entries of identMap, indentContext and
-- infoConstructors, i.e., ones that have no corresponding entries in
-- infoIdentifiers or infoInductives
pruneInfoTable' :: InfoTable -> InfoTable
pruneInfoTable' tab =
  pruneIdentMap
    $ over
      infoConstructors
      ( HashMap.filter
          ( \ConstructorInfo {..} ->
              isBuiltinTag _constructorTag
                || HashMap.member _constructorInductive (tab ^. infoInductives)
          )
      )
    $ over
      identContext
      (HashMap.filterWithKey (\s _ -> HashMap.member s (tab ^. infoIdentifiers)))
      tab
  where
    pruneIdentMap :: InfoTable -> InfoTable
    pruneIdentMap tab' =
      over
        identMap
        ( HashMap.filter
            ( \case
                IdentFun s -> HashMap.member s (tab' ^. infoIdentifiers)
                IdentInd s -> HashMap.member s (tab' ^. infoInductives)
                IdentConstr tag -> HashMap.member tag (tab' ^. infoConstructors)
            )
        )
        tab'
