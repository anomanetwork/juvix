module Juvix.Compiler.Core.Data.Module
  ( module Juvix.Compiler.Core.Data.Module,
    module Juvix.Compiler.Core.Data.Module.Base,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty

type Module = Module' InfoTable

type ModuleTable = ModuleTable' InfoTable

lookupInductiveInfo' :: Module -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' Module {..} sym =
  lookupTabInductiveInfo' _moduleInfoTable sym
    <|> lookupTabInductiveInfo' _moduleImportsTable sym

lookupConstructorInfo' :: Module -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' Module {..} tag =
  lookupTabConstructorInfo' _moduleInfoTable tag
    <|> lookupTabConstructorInfo' _moduleImportsTable tag

lookupIdentifierInfo' :: Module -> Symbol -> Maybe IdentifierInfo
lookupIdentifierInfo' Module {..} sym =
  lookupTabIdentifierInfo' _moduleInfoTable sym
    <|> lookupTabIdentifierInfo' _moduleImportsTable sym

lookupIdentifierNode' :: Module -> Symbol -> Maybe Node
lookupIdentifierNode' Module {..} sym =
  lookupTabIdentifierNode' _moduleInfoTable sym
    <|> lookupTabIdentifierNode' _moduleImportsTable sym

lookupSpecialisationInfo :: Module -> Symbol -> [SpecialisationInfo]
lookupSpecialisationInfo Module {..} sym =
  fromMaybe [] $
    lookupTabSpecialisationInfo' _moduleInfoTable sym
      <|> lookupTabSpecialisationInfo' _moduleImportsTable sym

impossibleSymbolNotFound :: (HasCallStack) => Symbol -> a
impossibleSymbolNotFound sym = impossibleError ("Could not find symbol " <> ppTrace sym)

lookupInductiveInfo :: Module -> Symbol -> InductiveInfo
lookupInductiveInfo m sym = fromMaybe (impossibleSymbolNotFound sym) (lookupInductiveInfo' m sym)

lookupConstructorInfo :: Module -> Tag -> ConstructorInfo
lookupConstructorInfo m tag = fromJust (lookupConstructorInfo' m tag)

lookupIdentifierInfo :: Module -> Symbol -> IdentifierInfo
lookupIdentifierInfo m sym = fromMaybe (impossibleSymbolNotFound sym) (lookupIdentifierInfo' m sym)

lookupIdentifierNode :: Module -> Symbol -> Node
lookupIdentifierNode m sym = fromMaybe (impossibleSymbolNotFound sym) (lookupIdentifierNode' m sym)

lookupBuiltinInductive :: Module -> BuiltinInductive -> Maybe InductiveInfo
lookupBuiltinInductive Module {..} b =
  lookupTabBuiltinInductive _moduleInfoTable b
    <|> lookupTabBuiltinInductive _moduleImportsTable b

lookupBuiltinConstructor :: Module -> BuiltinConstructor -> Maybe ConstructorInfo
lookupBuiltinConstructor Module {..} b =
  lookupTabBuiltinConstructor _moduleInfoTable b
    <|> lookupTabBuiltinConstructor _moduleImportsTable b

getInfoLiteralIntToNat :: Module -> Maybe Symbol
getInfoLiteralIntToNat Module {..} =
  _moduleInfoTable ^. infoLiteralIntToNat
    <|> _moduleImportsTable ^. infoLiteralIntToNat

getInfoLiteralIntToInt :: Module -> Maybe Symbol
getInfoLiteralIntToInt Module {..} =
  _moduleInfoTable ^. infoLiteralIntToInt
    <|> _moduleImportsTable ^. infoLiteralIntToInt

getInfoMain :: Module -> Maybe Symbol
getInfoMain Module {..} =
  _moduleInfoTable ^. infoMain
    <|> _moduleImportsTable ^. infoMain

identName :: Module -> Symbol -> Text
identName md sym = lookupIdentifierInfo md sym ^. identifierName

typeName :: Module -> Symbol -> Text
typeName md sym = lookupInductiveInfo md sym ^. inductiveName

constrName :: Module -> Tag -> Text
constrName md tag = lookupConstructorInfo md tag ^. constructorName

identNames :: Module -> HashSet Text
identNames m = identNames' (computeCombinedInfoTable m)

identNamesList :: Module -> [Text]
identNamesList m = identNamesList' (computeCombinedInfoTable m)

freshIdentName :: Module -> Text -> Text
freshIdentName m = freshName (identNames m)

pruneInfoTable :: Module -> Module
pruneInfoTable = over moduleInfoTable pruneInfoTable'
