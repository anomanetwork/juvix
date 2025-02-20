module Juvix.Compiler.Tree.Translation.FromCore where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.Stripped.Module qualified as Core
import Juvix.Compiler.Core.Language.Stripped qualified as Core
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Extra.Type
import Juvix.Compiler.Tree.Language

type BinderList = BL.BinderList

fromCore :: Core.Module -> Module
fromCore md@Core.Module {..} =
  Module
    { _moduleId = _moduleId,
      _moduleInfoTable = fromCore' md,
      _moduleImports = _moduleImports,
      _moduleImportsTable = mempty,
      _moduleSHA256 = _moduleSHA256
    }

fromCore' :: Core.Module -> InfoTable
fromCore' md =
  InfoTable
    { _infoMainFunction = tab ^. Core.infoMain,
      _infoFunctions = genCode md <$> tab ^. Core.infoFunctions,
      _infoInductives = translateInductiveInfo <$> tab ^. Core.infoInductives,
      _infoConstrs = translateConstructorInfo <$> tab ^. Core.infoConstructors
    }
  where
    tab = md ^. Core.moduleInfoTable

toTreeOp :: Core.BuiltinOp -> TreeOp
toTreeOp = \case
  -- TreeBinaryOpcode
  Core.OpIntAdd -> TreeBinaryOpcode (PrimBinop OpIntAdd)
  Core.OpIntSub -> TreeBinaryOpcode (PrimBinop OpIntSub)
  Core.OpIntMul -> TreeBinaryOpcode (PrimBinop OpIntMul)
  Core.OpIntDiv -> TreeBinaryOpcode (PrimBinop OpIntDiv)
  Core.OpIntMod -> TreeBinaryOpcode (PrimBinop OpIntMod)
  Core.OpIntLt -> TreeBinaryOpcode (PrimBinop (OpBool OpIntLt))
  Core.OpIntLe -> TreeBinaryOpcode (PrimBinop (OpBool OpIntLe))
  Core.OpFieldAdd -> TreeBinaryOpcode (PrimBinop OpFieldAdd)
  Core.OpFieldSub -> TreeBinaryOpcode (PrimBinop OpFieldSub)
  Core.OpFieldMul -> TreeBinaryOpcode (PrimBinop OpFieldMul)
  Core.OpFieldDiv -> TreeBinaryOpcode (PrimBinop OpFieldDiv)
  Core.OpEq -> TreeBinaryOpcode (PrimBinop (OpBool OpEq))
  Core.OpStrConcat -> TreeBinaryOpcode (PrimBinop OpStrConcat)
  Core.OpSeq -> TreeBinaryOpcode OpSeq
  -- TreeUnaryOpcode
  Core.OpShow -> TreeUnaryOpcode (PrimUnop OpShow)
  Core.OpStrToInt -> TreeUnaryOpcode (PrimUnop OpStrToInt)
  Core.OpFieldFromInt -> TreeUnaryOpcode (PrimUnop OpIntToField)
  Core.OpFieldToInt -> TreeUnaryOpcode (PrimUnop OpFieldToInt)
  Core.OpTrace -> TreeUnaryOpcode OpTrace
  Core.OpFail -> TreeUnaryOpcode OpFail
  Core.OpUInt8FromInt -> TreeUnaryOpcode (PrimUnop OpIntToUInt8)
  Core.OpUInt8ToInt -> TreeUnaryOpcode (PrimUnop OpUInt8ToInt)
  Core.OpAssert -> TreeUnaryOpcode OpAssert
  -- TreeAnomaOp
  Core.OpAnomaGet -> TreeAnomaOp OpAnomaGet
  Core.OpAnomaEncode -> TreeAnomaOp OpAnomaEncode
  Core.OpAnomaDecode -> TreeAnomaOp OpAnomaDecode
  Core.OpAnomaVerifyDetached -> TreeAnomaOp OpAnomaVerifyDetached
  Core.OpAnomaSign -> TreeAnomaOp OpAnomaSign
  Core.OpAnomaSignDetached -> TreeAnomaOp OpAnomaSignDetached
  Core.OpAnomaVerifyWithMessage -> TreeAnomaOp OpAnomaVerifyWithMessage
  Core.OpAnomaByteArrayToAnomaContents -> TreeAnomaOp OpAnomaByteArrayToAnomaContents
  Core.OpAnomaByteArrayFromAnomaContents -> TreeAnomaOp OpAnomaByteArrayFromAnomaContents
  Core.OpAnomaSha256 -> TreeAnomaOp OpAnomaSha256
  Core.OpAnomaResourceCommitment -> TreeAnomaOp OpAnomaResourceCommitment
  Core.OpAnomaResourceNullifier -> TreeAnomaOp OpAnomaResourceNullifier
  Core.OpAnomaResourceKind -> TreeAnomaOp OpAnomaResourceKind
  Core.OpAnomaResourceDelta -> TreeAnomaOp OpAnomaResourceDelta
  Core.OpAnomaActionDelta -> TreeAnomaOp OpAnomaActionDelta
  Core.OpAnomaActionsDelta -> TreeAnomaOp OpAnomaActionsDelta
  Core.OpAnomaProveAction -> TreeAnomaOp OpAnomaProveAction
  Core.OpAnomaProveDelta -> TreeAnomaOp OpAnomaProveDelta
  Core.OpAnomaZeroDelta -> TreeAnomaOp OpAnomaZeroDelta
  Core.OpAnomaAddDelta -> TreeAnomaOp OpAnomaAddDelta
  Core.OpAnomaSubDelta -> TreeAnomaOp OpAnomaSubDelta
  Core.OpAnomaRandomGeneratorInit -> TreeAnomaOp OpAnomaRandomGeneratorInit
  Core.OpAnomaRandomNextBytes -> TreeAnomaOp OpAnomaRandomNextBytes
  Core.OpAnomaRandomSplit -> TreeAnomaOp OpAnomaRandomSplit
  Core.OpAnomaIsCommitment -> TreeAnomaOp OpAnomaIsCommitment
  Core.OpAnomaIsNullifier -> TreeAnomaOp OpAnomaIsNullifier
  Core.OpAnomaSetToList -> TreeAnomaOp OpAnomaSetToList
  Core.OpAnomaSetFromList -> TreeAnomaOp OpAnomaSetFromList
  -- TreeCairoOp
  Core.OpPoseidonHash -> TreeCairoOp OpCairoPoseidon
  Core.OpEc -> TreeCairoOp OpCairoEc
  Core.OpRandomEcPoint -> TreeCairoOp OpCairoRandomEcPoint
  -- TreeByteArrayOp
  Core.OpByteArrayFromListByte -> TreeByteArrayOp OpByteArrayFromListUInt8
  Core.OpByteArrayLength -> TreeByteArrayOp OpByteArrayLength

-- Generate code for a single function.
genCode :: Core.Module -> Core.FunctionInfo -> FunctionInfo
genCode md fi =
  let argnames = map (Just . (^. Core.argumentName)) (fi ^. Core.functionArgsInfo)
      bl =
        BL.fromList . reverse $
          ( zipWithExact
              (\x y -> DRef $ ArgRef $ OffsetRef x y)
              [0 .. fi ^. Core.functionArgsNum - 1]
              argnames
          )
      code = go 0 bl (fi ^. Core.functionBody)
   in FunctionInfo
        { _functionName = fi ^. Core.functionName,
          _functionLocation = fi ^. Core.functionLocation,
          _functionSymbol = fi ^. Core.functionSymbol,
          _functionArgsNum = fi ^. Core.functionArgsNum,
          _functionArgNames = argnames,
          _functionType = convertType (fi ^. Core.functionArgsNum) (fi ^. Core.functionType),
          _functionCode = code,
          _functionExtra = ()
        }
  where
    go :: Int -> BinderList MemRef -> Core.Node -> Node
    go tempSize refs node = case node of
      Core.NVar v -> goVar refs v
      Core.NIdt idt -> goIdent idt
      Core.NCst cst -> goConstant cst
      Core.NApp apps -> goApps tempSize refs apps
      Core.NBlt blt -> goBuiltinApp tempSize refs blt
      Core.NCtr ctr -> goConstr tempSize refs ctr
      Core.NLet lt -> goLet tempSize refs lt
      Core.NCase c -> goCase tempSize refs c
      Core.NIf x -> goIf tempSize refs x

    goVar :: BinderList MemRef -> Core.Var -> Node
    goVar refs Core.Var {..} =
      mkMemRef (BL.lookup _varIndex refs)

    goIdent :: Core.Ident -> Node
    goIdent Core.Ident {..}
      | getArgsNum _identSymbol == 0 =
          Call $
            NodeCall
              { _nodeCallInfo = mempty,
                _nodeCallType = CallFun _identSymbol,
                _nodeCallArgs = []
              }
      | otherwise =
          AllocClosure $
            NodeAllocClosure
              { _nodeAllocClosureInfo = mempty,
                _nodeAllocClosureFunSymbol = _identSymbol,
                _nodeAllocClosureArgs = []
              }

    goConstant :: Core.Constant -> Node
    goConstant = \case
      Core.Constant _ (Core.ConstInteger i) ->
        mkConst (ConstInt i)
      Core.Constant _ (Core.ConstString s) ->
        mkConst (ConstString s)
      Core.Constant _ (Core.ConstField fld) ->
        mkConst (ConstField fld)
      Core.Constant _ (Core.ConstUInt8 i) ->
        mkConst (ConstUInt8 i)
      Core.Constant _ (Core.ConstByteArray bs) ->
        mkConst (ConstByteArray bs)

    goApps :: Int -> BinderList MemRef -> Core.Apps -> Node
    goApps tempSize refs Core.Apps {..} =
      let suppliedArgs' = fmap (go tempSize refs) _appsArgs
          suppliedArgs = toList suppliedArgs'
          suppliedArgsNum = length suppliedArgs
       in case _appsFun of
            Core.FunIdent Core.Ident {..} ->
              if
                  | argsNum > suppliedArgsNum ->
                      AllocClosure $
                        NodeAllocClosure
                          { _nodeAllocClosureInfo = mempty,
                            _nodeAllocClosureFunSymbol = _identSymbol,
                            _nodeAllocClosureArgs = suppliedArgs
                          }
                  | argsNum == suppliedArgsNum ->
                      Call $
                        NodeCall
                          { _nodeCallInfo = mempty,
                            _nodeCallType = CallFun _identSymbol,
                            _nodeCallArgs = suppliedArgs
                          }
                  | otherwise ->
                      -- If more arguments are supplied (suppliedArgsNum) than
                      -- the function eats up (argsNum), then the function
                      -- returns a closure. We should first call the function
                      -- (with Call) and then use CallClosures on the result
                      -- with the remaining arguments.
                      CallClosures $
                        NodeCallClosures
                          { _nodeCallClosuresInfo = mempty,
                            _nodeCallClosuresFun =
                              Call $
                                NodeCall
                                  { _nodeCallInfo = mempty,
                                    _nodeCallType = CallFun _identSymbol,
                                    _nodeCallArgs = take argsNum suppliedArgs
                                  },
                            _nodeCallClosuresArgs = nonEmpty' $ drop argsNum suppliedArgs
                          }
              where
                argsNum = getArgsNum _identSymbol
            Core.FunVar Core.Var {..} ->
              CallClosures $
                NodeCallClosures
                  { _nodeCallClosuresInfo = mempty,
                    _nodeCallClosuresFun = mkMemRef $ BL.lookup _varIndex refs,
                    _nodeCallClosuresArgs = suppliedArgs'
                  }

    goBuiltinApp :: Int -> BinderList MemRef -> Core.BuiltinApp -> Node
    goBuiltinApp tempSize refs Core.BuiltinApp {..} = case toTreeOp _builtinAppOp of
      TreeByteArrayOp op ->
        ByteArray $
          NodeByteArray
            { _nodeByteArrayInfo = mempty,
              _nodeByteArrayOpcode = op,
              _nodeByteArrayArgs = args
            }
      TreeCairoOp op ->
        Cairo $
          NodeCairo
            { _nodeCairoInfo = mempty,
              _nodeCairoOpcode = op,
              _nodeCairoArgs = args
            }
      TreeAnomaOp op ->
        Anoma $
          NodeAnoma
            { _nodeAnomaInfo = mempty,
              _nodeAnomaOpcode = op,
              _nodeAnomaArgs = args
            }
      TreeBinaryOpcode op -> case args of
        [arg1, arg2] ->
          Binop $
            NodeBinop
              { _nodeBinopInfo = mempty,
                _nodeBinopOpcode = op,
                _nodeBinopArg1 = arg1,
                _nodeBinopArg2 = arg2
              }
        _ -> impossible
      TreeUnaryOpcode op -> case args of
        [arg] ->
          Unop $
            NodeUnop
              { _nodeUnopInfo = mempty,
                _nodeUnopOpcode = op,
                _nodeUnopArg = arg
              }
        _ -> impossible
      where
        args = map (go tempSize refs) _builtinAppArgs

    goConstr :: Int -> BinderList MemRef -> Core.Constr -> Node
    goConstr tempSize refs = \case
      Core.Constr _ (Core.BuiltinTag Core.TagTrue) _ ->
        mkConst (ConstBool True)
      Core.Constr _ (Core.BuiltinTag Core.TagFalse) _ ->
        mkConst (ConstBool False)
      Core.Constr {..} ->
        AllocConstr $
          NodeAllocConstr
            { _nodeAllocConstrInfo = mempty,
              _nodeAllocConstrTag = _constrTag,
              _nodeAllocConstrArgs = args
            }
        where
          args = map (go tempSize refs) _constrArgs

    goLet :: Int -> BinderList MemRef -> Core.Let -> Node
    goLet tempSize refs (Core.Let {..}) =
      Save $
        NodeSave
          { _nodeSaveInfo = mempty,
            _nodeSaveArg = arg,
            _nodeSaveBody = body,
            _nodeSaveTempVar =
              TempVar
                { _tempVarName = Just name,
                  _tempVarLocation = loc,
                  _tempVarType = convertType 0 (_letItem ^. Core.letItemBinder . Core.binderType)
                }
          }
      where
        name = _letItem ^. Core.letItemBinder . Core.binderName
        loc = _letItem ^. Core.letItemBinder . Core.binderLocation
        nameRef = OffsetRef tempSize (Just name)
        arg = go tempSize refs (_letItem ^. Core.letItemValue)
        body = go (tempSize + 1) (BL.cons (DRef (mkTempRef nameRef)) refs) _letBody

    goCase :: Int -> BinderList MemRef -> Core.Case -> Node
    goCase tempSize refs Core.Case {..} =
      Case $
        NodeCase
          { _nodeCaseInfo = mempty,
            _nodeCaseArg = go tempSize refs _caseValue,
            _nodeCaseInductive = _caseInductive,
            _nodeCaseBranches = compileCaseBranches _caseBranches,
            _nodeCaseDefault = fmap compileCaseDefault _caseDefault
          }
      where
        compileCaseBranches :: [Core.CaseBranch] -> [CaseBranch]
        compileCaseBranches branches =
          map
            ( \Core.CaseBranch {..} ->
                if
                    | _caseBranchBindersNum == 0 ->
                        compileCaseBranchNoBinders _caseBranchTag _caseBranchBody
                    | otherwise ->
                        compileCaseBranch _caseBranchBindersNum _caseBranchTag _caseBranchBody
            )
            branches

        compileCaseBranchNoBinders :: Tag -> Core.Node -> CaseBranch
        compileCaseBranchNoBinders tag body =
          CaseBranch
            { _caseBranchLocation = Nothing,
              _caseBranchTag = tag,
              _caseBranchBody = go tempSize refs body,
              _caseBranchSave = False
            }

        compileCaseBranch :: Int -> Tag -> Core.Node -> CaseBranch
        compileCaseBranch bindersNum tag body =
          CaseBranch
            { _caseBranchLocation = Nothing,
              _caseBranchTag = tag,
              _caseBranchBody =
                go
                  (tempSize + 1)
                  ( BL.prepend
                      (map mkFieldRef (reverse [0 .. bindersNum - 1]))
                      refs
                  )
                  body,
              _caseBranchSave = True
            }
          where
            mkFieldRef :: Offset -> MemRef
            mkFieldRef off =
              ConstrRef $
                Field
                  { _fieldName = Nothing,
                    _fieldTag = tag,
                    _fieldRef = mkTempRef (OffsetRef tempSize Nothing),
                    _fieldOffset = off
                  }

        compileCaseDefault :: Core.Node -> Node
        compileCaseDefault = go tempSize refs

    goIf :: Int -> BinderList MemRef -> Core.If -> Node
    goIf tempSize refs Core.If {..} =
      Branch $
        NodeBranch
          { _nodeBranchInfo = mempty,
            _nodeBranchArg = go tempSize refs _ifValue,
            _nodeBranchTrue = go tempSize refs _ifTrue,
            _nodeBranchFalse = go tempSize refs _ifFalse
          }

    getArgsNum :: Symbol -> Int
    getArgsNum sym =
      Core.lookupFunInfo md sym
        ^. Core.functionArgsNum

-- | Be mindful that JuvixTree types are explicitly uncurried, while
-- Core.Stripped types are always curried. If a function takes `n` arguments,
-- then the first `n` arguments should be uncurried in its JuvixTree type.
convertType :: Int -> Core.Type -> Type
convertType argsNum ty =
  case ty of
    Core.TyDynamic ->
      TyDynamic
    Core.TyPrim x ->
      convertPrimitiveType x
    Core.TyApp Core.TypeApp {..} ->
      TyInductive (TypeInductive _typeAppSymbol)
    Core.TyFun {} ->
      let (tgt, tyargs) = Core.unfoldType ty
          tyargs' = map convertNestedType tyargs
          tgt' = convertType 0 tgt
       in mkTypeFun (take argsNum tyargs') (mkTypeFun (drop argsNum tyargs') tgt')

convertPrimitiveType :: Core.Primitive -> Type
convertPrimitiveType = \case
  Core.PrimInteger Core.PrimIntegerInfo {..} ->
    TyInteger (TypeInteger _infoMinValue _infoMaxValue)
  Core.PrimBool Core.PrimBoolInfo {..} ->
    TyBool (TypeBool _infoTrueTag _infoFalseTag)
  Core.PrimString ->
    TyString
  Core.PrimField ->
    TyField
  Core.PrimByteArray ->
    TyByteArray
  Core.PrimRandomGenerator ->
    TyRandomGenerator

-- | `convertNestedType` ensures that the conversion of a type with Dynamic in the
-- target is curried. The result of `convertType 0 ty` is always uncurried.
convertNestedType :: Core.Type -> Type
convertNestedType ty =
  case ty of
    Core.TyFun {} ->
      let (tgt, tyargs) = Core.unfoldType ty
       in case tgt of
            Core.TyDynamic ->
              curryType (convertType 0 ty)
            _ ->
              mkTypeFun (map convertNestedType tyargs) (convertType 0 tgt)
    _ ->
      convertType 0 ty

translateInductiveInfo :: Core.InductiveInfo -> InductiveInfo
translateInductiveInfo ii =
  InductiveInfo
    { _inductiveName = ii ^. Core.inductiveName,
      _inductiveLocation = ii ^. Core.inductiveLocation,
      _inductiveSymbol = ii ^. Core.inductiveSymbol,
      _inductiveKind = convertType 0 (ii ^. Core.inductiveKind),
      _inductiveConstructors = ii ^. Core.inductiveConstructors,
      _inductiveRepresentation = IndRepStandard
    }

translateConstructorInfo :: Core.ConstructorInfo -> ConstructorInfo
translateConstructorInfo ci =
  ConstructorInfo
    { _constructorName = ci ^. Core.constructorName,
      _constructorLocation = ci ^. Core.constructorLocation,
      _constructorTag = ci ^. Core.constructorTag,
      _constructorArgNames = ci ^. Core.constructorArgNames,
      _constructorArgsNum = ci ^. Core.constructorArgsNum,
      _constructorType = ty,
      _constructorInductive = ci ^. Core.constructorInductive,
      _constructorRepresentation = MemRepConstr,
      _constructorFixity = ci ^. Core.constructorFixity
    }
  where
    ty = convertType 0 (ci ^. Core.constructorType)
