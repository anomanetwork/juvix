module Juvix.Compiler.Tree.Transformation.Validate where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Base (getNodeLocation)
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Extra.Type
import Juvix.Compiler.Tree.Transformation.Base

inferType :: forall r. (Member (Error TreeError) r) => Module -> FunctionInfo -> Node -> Sem r Type
inferType md funInfo = goInfer mempty
  where
    goInfer :: BinderList Type -> Node -> Sem r Type
    goInfer bl = \case
      Binop x -> goBinop bl x
      Unop x -> goUnop bl x
      ByteArray x -> goByteArray bl x
      Cairo x -> goCairo bl x
      Anoma x -> goAnoma bl x
      Constant x -> goConst bl x
      MemRef x -> goMemRef bl x
      AllocConstr x -> goAllocConstr bl x
      AllocClosure x -> goAllocClosure bl x
      ExtendClosure x -> goExtendClosure bl x
      Call x -> goCall bl x
      CallClosures x -> goCallClosures bl x
      Branch x -> goBranch bl x
      Case x -> goCase bl x
      Save x -> goSave bl x

    goBinop :: BinderList Type -> NodeBinop -> Sem r Type
    goBinop bl NodeBinop {..} = case _nodeBinopOpcode of
      PrimBinop x -> checkPrimBinop x
      OpSeq -> do
        checkType bl _nodeBinopArg1 TyDynamic
        goInfer bl _nodeBinopArg2
      where
        loc = _nodeBinopInfo ^. nodeInfoLocation

        checkBinop :: Type -> Type -> Type -> Sem r Type
        checkBinop ty1' ty2' rty = do
          ty1 <- goInfer bl _nodeBinopArg1
          ty2 <- goInfer bl _nodeBinopArg2
          void $ unifyTypes' loc md ty1 ty1'
          void $ unifyTypes' loc md ty2 ty2'
          return rty

        checkPrimBinop :: BinaryOp -> Sem r Type
        checkPrimBinop = \case
          OpIntAdd -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
          OpIntSub -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
          OpIntMul -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
          OpIntDiv -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
          OpIntMod -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
          OpBool OpIntLt -> checkBinop mkTypeInteger mkTypeInteger mkTypeBool
          OpBool OpIntLe -> checkBinop mkTypeInteger mkTypeInteger mkTypeBool
          OpFieldAdd -> checkBinop TyField TyField TyField
          OpFieldSub -> checkBinop TyField TyField TyField
          OpFieldMul -> checkBinop TyField TyField TyField
          OpFieldDiv -> checkBinop TyField TyField TyField
          OpBool OpEq -> checkBinop TyDynamic TyDynamic mkTypeBool
          OpStrConcat -> checkBinop TyString TyString TyString

    goUnop :: BinderList Type -> NodeUnop -> Sem r Type
    goUnop bl NodeUnop {..} = case _nodeUnopOpcode of
      PrimUnop x -> checkPrimUnop x
      OpAssert -> goInfer bl _nodeUnopArg
      OpTrace -> goInfer bl _nodeUnopArg
      OpFail -> checkUnop TyDynamic TyDynamic
      where
        loc = _nodeUnopInfo ^. nodeInfoLocation

        checkUnop :: Type -> Type -> Sem r Type
        checkUnop ty rty = do
          ty' <- goInfer bl _nodeUnopArg
          void $ unifyTypes' loc md ty ty'
          return rty

        checkPrimUnop :: UnaryOp -> Sem r Type
        checkPrimUnop = \case
          OpShow -> checkUnop TyDynamic TyString
          OpStrToInt -> checkUnop TyString mkTypeInteger
          OpArgsNum -> checkUnop TyDynamic mkTypeInteger
          OpIntToField -> checkUnop mkTypeInteger TyField
          OpFieldToInt -> checkUnop TyField mkTypeInteger
          OpUInt8ToInt -> checkUnop mkTypeUInt8 mkTypeInteger
          OpIntToUInt8 -> checkUnop mkTypeInteger mkTypeUInt8

    goByteArray :: BinderList Type -> NodeByteArray -> Sem r Type
    goByteArray bl NodeByteArray {..} = do
      mapM_ (\arg -> checkType bl arg TyDynamic) _nodeByteArrayArgs
      return TyDynamic

    goCairo :: BinderList Type -> NodeCairo -> Sem r Type
    goCairo bl NodeCairo {..} = do
      mapM_ (\arg -> checkType bl arg TyDynamic) _nodeCairoArgs
      return TyDynamic

    goAnoma :: BinderList Type -> NodeAnoma -> Sem r Type
    goAnoma bl NodeAnoma {..} = do
      mapM_ (\arg -> checkType bl arg TyDynamic) _nodeAnomaArgs
      return TyDynamic

    goConst :: BinderList Type -> NodeConstant -> Sem r Type
    goConst _ NodeConstant {..} = case _nodeConstant of
      ConstInt {} -> return mkTypeInteger
      ConstBool {} -> return mkTypeBool
      ConstString {} -> return TyString
      ConstField {} -> return TyField
      ConstUnit {} -> return TyUnit
      ConstVoid {} -> return TyVoid
      ConstUInt8 {} -> return mkTypeUInt8
      ConstByteArray {} -> return TyByteArray

    goMemRef :: BinderList Type -> NodeMemRef -> Sem r Type
    goMemRef bl NodeMemRef {..} = case _nodeMemRef of
      DRef d -> goDirectRef (_nodeMemRefInfo ^. nodeInfoLocation) bl d
      ConstrRef x -> goField bl x

    goDirectRef :: Maybe Location -> BinderList Type -> DirectRef -> Sem r Type
    goDirectRef loc bl = \case
      ArgRef x -> goArgRef loc bl x
      TempRef RefTemp {..} -> goTempRef bl _refTempOffsetRef

    goArgRef :: Maybe Location -> BinderList Type -> OffsetRef -> Sem r Type
    goArgRef loc _ OffsetRef {..}
      | _offsetRefOffset < length tys = return $ tys !! _offsetRefOffset
      | typeTarget (funInfo ^. functionType) == TyDynamic = return TyDynamic
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = loc,
                _treeErrorMsg = "Wrong target type"
              }
      where
        tys = typeArgs (funInfo ^. functionType)

    goTempRef :: BinderList Type -> OffsetRef -> Sem r Type
    goTempRef bl OffsetRef {..} = return $ BL.lookupLevel _offsetRefOffset bl

    goField :: BinderList Type -> Field -> Sem r Type
    goField _ Field {..}
      | _fieldOffset < length tys = return $ tys !! _fieldOffset
      | otherwise = return TyDynamic
      where
        ci = lookupConstrInfo md _fieldTag
        tys = typeArgs (ci ^. constructorType)

    goAllocConstr :: BinderList Type -> NodeAllocConstr -> Sem r Type
    goAllocConstr bl NodeAllocConstr {..}
      | length _nodeAllocConstrArgs == length tys = do
          forM_ (zipExact _nodeAllocConstrArgs tys) (uncurry (checkType bl))
          return $ typeTarget (ci ^. constructorType)
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = _nodeAllocConstrInfo ^. nodeInfoLocation,
                _treeErrorMsg = ""
              }
      where
        ci = lookupConstrInfo md _nodeAllocConstrTag
        tys = typeArgs (ci ^. constructorType)

    goAllocClosure :: BinderList Type -> NodeAllocClosure -> Sem r Type
    goAllocClosure bl NodeAllocClosure {..}
      | n <= fi ^. functionArgsNum = do
          forM_ (zipExact _nodeAllocClosureArgs (take n tys)) (uncurry (checkType bl))
          return $ mkTypeFun (drop n tys) (typeTarget (fi ^. functionType))
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = _nodeAllocClosureInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Wrong number of arguments"
              }
      where
        n = length _nodeAllocClosureArgs
        fi = lookupFunInfo md _nodeAllocClosureFunSymbol
        tys = typeArgs (fi ^. functionType)

    goExtendClosure :: BinderList Type -> NodeExtendClosure -> Sem r Type
    goExtendClosure bl NodeExtendClosure {..} = do
      ty <- goInfer bl _nodeExtendClosureFun
      let tys = typeArgs ty
          m = length tys
          n = length _nodeExtendClosureArgs
      if
          | n < m -> do
              forM_ (zipExact (toList _nodeExtendClosureArgs) (take n tys)) (uncurry (checkType bl))
              return $ mkTypeFun (drop n tys) (typeTarget ty)
          | typeTarget ty == TyDynamic -> do
              let tys' = tys ++ replicate (n - m) TyDynamic
              forM_ (zipExact (toList _nodeExtendClosureArgs) tys') (uncurry (checkType bl))
              return $ typeTarget ty
          | otherwise ->
              throw $
                TreeError
                  { _treeErrorLoc = _nodeExtendClosureInfo ^. nodeInfoLocation,
                    _treeErrorMsg = "Too many arguments"
                  }

    goCall :: BinderList Type -> NodeCall -> Sem r Type
    goCall bl NodeCall {..} = case _nodeCallType of
      CallFun sym
        | n == fi ^. functionArgsNum -> do
            unless (n == 0) $
              forM_ (zipExact _nodeCallArgs tys) (uncurry (checkType bl))
            return $ mkTypeFun (drop n tys) (typeTarget (fi ^. functionType))
        | otherwise ->
            throw $
              TreeError
                { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                  _treeErrorMsg = "Wrong number of arguments"
                }
        where
          n = length _nodeCallArgs
          fi = lookupFunInfo md sym
          tys = typeArgs (fi ^. functionType)
      CallClosure cl -> do
        ty <- goInfer bl cl
        let tys = typeArgs ty
            n = length _nodeCallArgs
        when (length tys > n) $
          throw $
            TreeError
              { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Too few arguments"
              }
        when (length tys < n && typeTarget ty /= TyDynamic) $
          throw $
            TreeError
              { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Too many arguments"
              }
        let tys' = tys ++ replicate (n - length tys) TyDynamic
        forM_ (zipExact _nodeCallArgs tys') (uncurry (checkType bl))
        return $ typeTarget ty

    goCallClosures :: BinderList Type -> NodeCallClosures -> Sem r Type
    goCallClosures bl NodeCallClosures {..} = do
      ty <- goInfer bl _nodeCallClosuresFun
      go ty (toList _nodeCallClosuresArgs)
      where
        go :: Type -> [Node] -> Sem r Type
        go ty args
          | m == 0 =
              return ty
          | m <= n = do
              forM_ (zipExact (take m args) tys) (uncurry (checkType bl))
              go (typeTarget ty) (drop m args)
          | otherwise = do
              forM_ (zipExact args (take n tys)) (uncurry (checkType bl))
              return $ mkTypeFun (drop n tys) (typeTarget ty)
          where
            tys = typeArgs ty
            m = length tys
            n = length args

    goBranch :: BinderList Type -> NodeBranch -> Sem r Type
    goBranch bl NodeBranch {..} = do
      checkType bl _nodeBranchArg mkTypeBool
      ty1 <- goInfer bl _nodeBranchTrue
      ty2 <- goInfer bl _nodeBranchFalse
      unifyTypes' (_nodeBranchInfo ^. nodeInfoLocation) md ty1 ty2

    goCase :: BinderList Type -> NodeCase -> Sem r Type
    goCase bl NodeCase {..} = do
      ity <- goInfer bl _nodeCaseArg
      unless (ity == mkTypeInductive _nodeCaseInductive || ity == TyDynamic) $
        throw $
          TreeError
            { _treeErrorLoc = _nodeCaseInfo ^. nodeInfoLocation,
              _treeErrorMsg = "Inductive type mismatch"
            }
      ty <- maybe (return TyDynamic) (goInfer bl) _nodeCaseDefault
      go ity ty _nodeCaseBranches
      where
        go :: Type -> Type -> [CaseBranch] -> Sem r Type
        go ity ty = \case
          [] -> return ty
          CaseBranch {..} : brs -> do
            let bl' = if _caseBranchSave then BL.cons ity bl else bl
            ty' <- goInfer bl' _caseBranchBody
            ty'' <- unifyTypes' (_nodeCaseInfo ^. nodeInfoLocation) md ty ty'
            go ity ty'' brs

    goSave :: BinderList Type -> NodeSave -> Sem r Type
    goSave bl NodeSave {..} = do
      ty <- goInfer bl _nodeSaveArg
      goInfer (BL.cons ty bl) _nodeSaveBody

    checkType :: BinderList Type -> Node -> Type -> Sem r ()
    checkType bl node ty = do
      ty' <- goInfer bl node
      void $ unifyTypes' (getNodeLocation node) md ty ty'

validateFunction :: (Member (Error TreeError) r) => Module -> FunctionInfo -> Sem r FunctionInfo
validateFunction md funInfo = do
  ty <- inferType md funInfo (funInfo ^. functionCode)
  let ty' = if funInfo ^. functionArgsNum == 0 then funInfo ^. functionType else typeTarget (funInfo ^. functionType)
  void $ unifyTypes' (funInfo ^. functionLocation) md ty ty'
  return funInfo

validate :: (Member (Error TreeError) r) => Module -> Sem r Module
validate md = mapFunctionsM (validateFunction md) md
