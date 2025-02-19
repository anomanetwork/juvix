module Juvix.Compiler.Core.Translation.Stripped.FromCore (fromCore, fromCore') where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core
import Juvix.Compiler.Core.Data.Stripped.Module qualified as Stripped
import Juvix.Compiler.Core.Extra.Stripped.Base qualified as Stripped
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped
import Juvix.Compiler.Core.Pretty

fromCore :: Module -> Stripped.Module
fromCore Module {..} =
  Stripped.Module
    { _moduleId = _moduleId,
      _moduleInfoTable = fromCore' _moduleInfoTable,
      _moduleImports = _moduleImports,
      _moduleSHA256 = _moduleSHA256
    }

fromCore' :: InfoTable -> Stripped.InfoTable
fromCore' tab =
  Stripped.InfoTable
    { _infoMain = tab ^. infoMain,
      _infoFunctions = fmap (translateFunctionInfo tab) (tab' ^. infoIdentifiers),
      _infoInductives = fmap translateInductiveInfo (tab' ^. infoInductives),
      _infoConstructors = fmap translateConstructorInfo (tab' ^. infoConstructors)
    }
  where
    tab' =
      tab
        { _infoIdentifiers = HashMap.filter (maybe True shouldKeepFunction . (^. identifierBuiltin)) (tab ^. infoIdentifiers),
          _infoInductives = HashMap.filter (maybe True shouldKeepType . (^. inductiveBuiltin)) (tab ^. infoInductives),
          _infoConstructors = HashMap.filter (maybe True shouldKeepConstructor . (^. constructorBuiltin)) (tab ^. infoConstructors)
        }

    shouldKeepFunction :: BuiltinFunction -> Bool
    shouldKeepFunction = \case
      BuiltinAssert -> False
      BuiltinNatPlus -> False
      BuiltinNatSub -> False
      BuiltinNatMul -> False
      BuiltinNatUDiv -> False
      BuiltinNatDiv -> False
      BuiltinNatMod -> False
      BuiltinNatLe -> False
      BuiltinNatLt -> False
      BuiltinNatEq -> False
      BuiltinBoolIf -> False
      BuiltinBoolOr -> False
      BuiltinBoolAnd -> False
      BuiltinIntEq -> False
      BuiltinIntPlus -> False
      BuiltinIntSubNat -> False
      BuiltinIntNegNat -> False
      BuiltinIntNeg -> False
      BuiltinIntMul -> False
      BuiltinIntDiv -> False
      BuiltinIntMod -> False
      BuiltinIntSub -> False
      BuiltinIntNonNeg -> False
      BuiltinIntLe -> False
      BuiltinIntLt -> False
      BuiltinSeq -> False
      BuiltinIsEqual -> False
      BuiltinOrdCompare -> False
      BuiltinMonadBind -> True -- TODO revise
      BuiltinFromNat -> True
      BuiltinFromInt -> True

    shouldKeepConstructor :: BuiltinConstructor -> Bool
    shouldKeepConstructor = \case
      BuiltinListNil -> True
      BuiltinListCons -> True
      BuiltinMkEq -> True
      BuiltinMkOrd -> True
      BuiltinOrderingLT -> True
      BuiltinOrderingGT -> True
      BuiltinOrderingEQ -> True
      BuiltinMkPoseidonState -> True
      BuiltinMkEcPoint -> True
      BuiltinMaybeNothing -> True
      BuiltinMaybeJust -> True
      BuiltinPairConstr -> True
      BuiltinMkAnomaResource -> True
      BuiltinMkAnomaAction -> True
      BuiltinNatZero -> False
      BuiltinNatSuc -> False
      BuiltinBoolTrue -> False
      BuiltinBoolFalse -> False
      BuiltinIntOfNat -> False
      BuiltinIntNegSuc -> False

    shouldKeepType :: BuiltinType -> Bool
    shouldKeepType = \case
      BuiltinTypeAxiom a -> case a of
        BuiltinIO -> True
        BuiltinNatPrint -> False
        BuiltinNatToString -> False
        BuiltinStringPrint -> False
        BuiltinStringConcat -> False
        BuiltinStringEq -> False
        BuiltinStringToNat -> False
        BuiltinBoolPrint -> False
        BuiltinString -> False
        BuiltinField -> False
        BuiltinFieldEq -> False
        BuiltinFieldAdd -> False
        BuiltinFieldSub -> False
        BuiltinFieldMul -> False
        BuiltinFieldDiv -> False
        BuiltinFieldFromInt -> False
        BuiltinFieldToNat -> False
        BuiltinIOSequence -> False
        BuiltinIOReadline -> False
        BuiltinTrace -> False
        BuiltinFail -> False
        BuiltinIntToString -> False
        BuiltinIntPrint -> False
        BuiltinAnomaGet -> False
        BuiltinAnomaEncode -> False
        BuiltinAnomaDecode -> False
        BuiltinAnomaVerifyDetached -> False
        BuiltinAnomaSign -> False
        BuiltinAnomaSignDetached -> False
        BuiltinAnomaVerifyWithMessage -> False
        BuiltinAnomaByteArrayToAnomaContents -> False
        BuiltinAnomaByteArrayFromAnomaContents -> False
        BuiltinAnomaSha256 -> False
        BuiltinAnomaDelta -> False
        BuiltinAnomaKind -> False
        BuiltinAnomaResourceCommitment -> False
        BuiltinAnomaResourceDelta -> False
        BuiltinAnomaResourceNullifier -> False
        BuiltinAnomaResourceKind -> False
        BuiltinAnomaActionDelta -> False
        BuiltinAnomaActionsDelta -> False
        BuiltinAnomaAddDelta -> False
        BuiltinAnomaSubDelta -> False
        BuiltinAnomaZeroDelta -> False
        BuiltinAnomaProveAction -> False
        BuiltinAnomaProveDelta -> False
        BuiltinAnomaRandomGenerator -> False
        BuiltinAnomaRandomGeneratorInit -> False
        BuiltinAnomaRandomNextBytes -> False
        BuiltinAnomaRandomSplit -> False
        BuiltinAnomaIsCommitment -> False
        BuiltinAnomaIsNullifier -> False
        BuiltinAnomaSet -> False
        BuiltinAnomaSetToList -> False
        BuiltinAnomaSetFromList -> False
        BuiltinPoseidon -> False
        BuiltinEcOp -> False
        BuiltinRandomEcPoint -> False
        BuiltinByte -> False
        BuiltinByteEq -> False
        BuiltinByteToNat -> False
        BuiltinByteFromNat -> False
        BuiltinByteArray -> False
        BuiltinByteArrayFromListByte -> False
        BuiltinByteArrayLength -> False
      BuiltinTypeInductive i -> case i of
        BuiltinList -> True
        BuiltinEq -> True
        BuiltinMaybe -> True
        BuiltinPair -> True
        BuiltinOrd -> True
        BuiltinOrdering -> True
        BuiltinPoseidonState -> True
        BuiltinEcPoint -> True
        BuiltinNat -> False
        BuiltinInt -> False
        BuiltinBool -> False
        BuiltinAnomaResource -> True
        BuiltinAnomaAction -> True

translateFunctionInfo :: InfoTable -> IdentifierInfo -> Stripped.FunctionInfo
translateFunctionInfo tab IdentifierInfo {..} =
  assert (length (typeArgsBinders _identifierType) == _identifierArgsNum) $
    Stripped.FunctionInfo
      { _functionName = _identifierName,
        _functionLocation = _identifierLocation,
        _functionSymbol = _identifierSymbol,
        _functionBody = translateFunction _identifierArgsNum body,
        _functionType = translateType _identifierType,
        _functionArgsNum = _identifierArgsNum,
        _functionArgsInfo = map translateArgInfo (lambdaBinders body),
        _functionIsExported = _identifierIsExported
      }
  where
    body = lookupTabIdentifierNode tab _identifierSymbol

translateArgInfo :: Binder -> Stripped.ArgumentInfo
translateArgInfo Binder {..} =
  Stripped.ArgumentInfo
    { _argumentName = _binderName,
      _argumentLocation = _binderLocation,
      _argumentType = translateType _binderType
    }

translateInductiveInfo :: InductiveInfo -> Stripped.InductiveInfo
translateInductiveInfo InductiveInfo {..} =
  Stripped.InductiveInfo
    { _inductiveName = _inductiveName,
      _inductiveLocation = _inductiveLocation,
      _inductiveSymbol = _inductiveSymbol,
      _inductiveKind = translateType _inductiveKind,
      _inductiveConstructors = _inductiveConstructors,
      _inductiveParams = map translateParamInfo _inductiveParams
    }

translateParamInfo :: ParameterInfo -> Stripped.ParameterInfo
translateParamInfo ParameterInfo {..} =
  Stripped.ParameterInfo
    { _paramName = _paramName,
      _paramLocation = _paramLocation,
      _paramKind = translateType _paramKind,
      _paramIsImplicit = _paramIsImplicit
    }

translateConstructorInfo :: ConstructorInfo -> Stripped.ConstructorInfo
translateConstructorInfo ConstructorInfo {..} =
  Stripped.ConstructorInfo
    { _constructorName = _constructorName,
      _constructorLocation = _constructorLocation,
      _constructorInductive = _constructorInductive,
      _constructorTag = _constructorTag,
      _constructorType = translateType _constructorType,
      _constructorArgNames,
      _constructorArgsNum,
      _constructorFixity
    }

translateFunction :: Int -> Node -> Stripped.Node
translateFunction argsNum node =
  let (k, body) = unfoldLambdas' node
   in if
          | k /= argsNum ->
              error
                ( "wrong number of arguments. argsNum = "
                    <> show argsNum
                    <> ", unfoldLambdas = "
                    <> show k
                    <> "\nNode = "
                    <> ppTrace node
                )
          | otherwise -> translateNode body

translateNode :: Node -> Stripped.Node
translateNode node = case node of
  NVar v ->
    Stripped.NVar $ translateVar v
  NIdt idt ->
    Stripped.NIdt $ translateIdent idt
  NCst Constant {..} ->
    Stripped.mkConstant _constantValue
  NApp App {} ->
    let (tgt, args) = unfoldApps' node
        args' = map translateNode args
     in case tgt of
          NVar v -> Stripped.mkApps (Stripped.FunVar $ translateVar v) (nonEmpty' args')
          NIdt idt -> Stripped.mkApps (Stripped.FunIdent $ translateIdent idt) (nonEmpty' args')
          _ -> unsupported
  NBlt BuiltinApp {..} ->
    Stripped.mkBuiltinApp _builtinAppOp (map translateNode _builtinAppArgs)
  NCtr Constr {..} ->
    Stripped.mkConstr
      ( Stripped.ConstrInfo
          { _constrInfoName = getInfoName _constrInfo,
            _constrInfoLocation = getInfoLocation _constrInfo,
            _constrInfoType = Stripped.TyDynamic
          }
      )
      _constrTag
      (map translateNode _constrArgs)
  NLet Let {..} ->
    Stripped.mkLet
      ( Stripped.Binder
          { _binderName = _letItem ^. letItemBinder . binderName,
            _binderLocation = _letItem ^. letItemBinder . binderLocation,
            _binderType = translateType (_letItem ^. letItemBinder . binderType)
          }
      )
      (translateNode (_letItem ^. letItemValue))
      (translateNode _letBody)
  NCase c@Case {..} -> translateCase translateIf dflt c
    where
      dflt =
        Stripped.mkCase
          _caseInductive
          (translateNode _caseValue)
          (map translateCaseBranch _caseBranches)
          (fmap translateNode _caseDefault)
  _
    | isType' node ->
        Stripped.mkConstr (Stripped.ConstrInfo "()" Nothing Stripped.TyDynamic) (BuiltinTag TagTrue) []
  _ ->
    unsupported
  where
    unsupported :: a
    unsupported = error "Core to Core.Stripped: unsupported node"

    translateIf :: Node -> Node -> Node -> Stripped.Node
    translateIf val br1 br2 = Stripped.mkIf (translateNode val) (translateNode br1) (translateNode br2)

    translateCaseBranch :: CaseBranch -> Stripped.CaseBranch
    translateCaseBranch CaseBranch {..}
      | _caseBranchTag == BuiltinTag TagTrue || _caseBranchTag == BuiltinTag TagFalse =
          error "Core to Core.Stripped: invalid case on booleans"
    translateCaseBranch CaseBranch {..} =
      Stripped.CaseBranch
        { _caseBranchInfo =
            Stripped.CaseBranchInfo
              { _caseBranchInfoConstrName = getInfoName _caseBranchInfo,
                _caseBranchInfoConstrType = Stripped.TyDynamic
              },
          _caseBranchTag = _caseBranchTag,
          _caseBranchBinders = map (over binderType translateType) _caseBranchBinders,
          _caseBranchBindersNum = _caseBranchBindersNum,
          _caseBranchBody = translateNode _caseBranchBody
        }

    translateVar :: Var -> Stripped.Var
    translateVar Var {..} =
      Stripped.Var
        ( Stripped.VarInfo
            { _varInfoName = getInfoName _varInfo,
              _varInfoLocation = getInfoLocation _varInfo,
              _varInfoType = Stripped.TyDynamic
            }
        )
        _varIndex

    translateIdent :: Ident -> Stripped.Ident
    translateIdent Ident {..} =
      Stripped.Ident
        ( Stripped.IdentInfo
            { _identInfoName = getInfoName _identInfo,
              _identInfoLocation = getInfoLocation _identInfo,
              _identInfoType = Stripped.TyDynamic
            }
        )
        _identSymbol

translateType :: Node -> Stripped.Type
translateType node = case node of
  NVar {} ->
    Stripped.TyDynamic
  NPi Pi {} ->
    let (args, tgt) = unfoldPi node
        tyargs = map (^. piLhsBinder . binderType) args
     in Stripped.mkFunType (map translateType tyargs) (translateType tgt)
  NUniv Univ {} ->
    Stripped.TyDynamic
  NTyp TypeConstr {..} ->
    Stripped.TyApp $
      Stripped.TypeApp
        { _typeAppName = getInfoName _typeConstrInfo,
          _typeAppLocation = getInfoLocation _typeConstrInfo,
          _typeAppSymbol = _typeConstrSymbol,
          _typeAppArgs = map translateType _typeConstrArgs
        }
  NPrim TypePrim {..} ->
    Stripped.TyPrim _typePrimPrimitive
  NDyn DynamicTy {} ->
    Stripped.TyDynamic
  _ ->
    Stripped.TyDynamic
