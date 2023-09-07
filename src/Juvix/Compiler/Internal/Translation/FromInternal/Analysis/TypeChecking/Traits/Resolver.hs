module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.TypedHole
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

type SubsI = HashMap VarName InstanceParam

subsIToE :: SubsI -> SubsE
subsIToE = fmap paramToExpression

isTrait :: InfoTable -> InductiveName -> Bool
isTrait tab name = fromJust (HashMap.lookup name (tab ^. infoInductives)) ^. inductiveInfoDef . inductiveTrait

resolveTraitInstance ::
  (Members '[Error TypeCheckerError, NameIdGen, Inference, Reader LocalVars, Reader InfoTable] r) =>
  TypedHole ->
  Sem r Expression
resolveTraitInstance TypedHole {..} = do
  vars <- ask
  tbl <- ask
  let tab = foldr (flip updateInstanceTable) (tbl ^. infoInstances) (varsToInstances tbl vars)
  ty <- strongNormalize _typedHoleType
  is <- lookupInstance tab ty
  case is of
    [(ii, subs)] -> expandArity loc (subsIToE subs) (ii ^. instanceInfoArgs) (ii ^. instanceInfoResult)
    [] -> throw (ErrNoInstance (NoInstance ty loc))
    _ -> throw (ErrAmbiguousInstances (AmbiguousInstances ty (map fst is) loc))
  where
    loc = getLoc _typedHoleHole

varsToInstances :: InfoTable -> LocalVars -> [InstanceInfo]
varsToInstances tbl LocalVars {..} =
  mapMaybe
    (instanceFromTypedExpression' tbl . mkTyped)
    (HashMap.toList _localTypes)
  where
    mkTyped :: (VarName, Expression) -> TypedExpression
    mkTyped (v, ty) =
      TypedExpression
        { _typedType = ty,
          _typedExpression = ExpressionIden (IdenVar v)
        }

expandArity ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  SubsE ->
  [FunctionParameter] ->
  Expression ->
  Sem r Expression
expandArity loc subs params e = case params of
  [] ->
    return e
  fp@FunctionParameter {..} : params'
    | Just (Just t) <- flip HashMap.lookup subs <$> _paramName ->
        expandArity loc subs params' (ExpressionApplication (Application e t _paramImplicit))
    | _paramImplicit == Implicit -> do
        h <- newHole
        expandArity loc subs params' (ExpressionApplication (Application e (ExpressionHole h) Implicit))
    | _paramImplicit == ImplicitInstance -> do
        h <- newHole
        expandArity loc subs params' (ExpressionApplication (Application e (ExpressionInstanceHole h) ImplicitInstance))
    | otherwise ->
        throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument fp))
  where
    newHole :: (Member NameIdGen r) => Sem r Hole
    newHole = mkHole loc <$> freshNameId

lookupInstance' ::
  forall r.
  (Member Inference r) =>
  InstanceTable ->
  Name ->
  [InstanceParam] ->
  Sem r [(InstanceInfo, SubsI)]
lookupInstance' tab name params = do
  let is = fromMaybe [] $ HashMap.lookup name tab
  mapMaybeM matchInstance is
  where
    matchInstance :: InstanceInfo -> Sem r (Maybe (InstanceInfo, SubsI))
    matchInstance ii@InstanceInfo {..}
      | length params /= length _instanceInfoParams =
          return Nothing
      | otherwise = do
          (si, b) <-
            runState mempty $
              and <$> sequence (zipWithExact goMatch _instanceInfoParams params)
          if
              | b -> do
                  return $ Just (ii, si)
              | otherwise -> return Nothing

    goMatch :: InstanceParam -> InstanceParam -> Sem (State SubsI ': r) Bool
    goMatch pat t = case (pat, t) of
      (InstanceParamMeta v, _) -> do
        m <- gets (HashMap.lookup v)
        case m of
          Just t'
            | t' == t ->
                return True
            | otherwise ->
                return False
          Nothing -> do
            modify (HashMap.insert v t)
            return True
      (InstanceParamVar v1, InstanceParamVar v2)
        | v1 == v2 ->
            return True
      (InstanceParamHole h, _) -> do
        m <- matchTypes (ExpressionHole h) (paramToExpression t)
        case m of
          Just {} -> return False
          Nothing -> return True
      (_, InstanceParamHole h)
        | checkNoMeta pat -> do
            m <- matchTypes (paramToExpression pat) (ExpressionHole h)
            case m of
              Just {} -> return False
              Nothing -> return True
      (InstanceParamApp app1, InstanceParamApp app2)
        | app1 ^. instanceAppHead == app2 ^. instanceAppHead -> do
            and <$> sequence (zipWithExact goMatch (app1 ^. instanceAppArgs) (app2 ^. instanceAppArgs))
      _ ->
        return False

lookupInstance ::
  forall r.
  (Members '[Error TypeCheckerError, Inference] r) =>
  InstanceTable ->
  Expression ->
  Sem r [(InstanceInfo, SubsI)]
lookupInstance tab ty = do
  case traitFromExpression mempty ty of
    Just InstanceApp {..} ->
      lookupInstance' tab _instanceAppHead _instanceAppArgs
    _ ->
      throw (ErrNotATrait (NotATrait ty))

instanceFromTypedExpression' :: InfoTable -> TypedExpression -> Maybe InstanceInfo
instanceFromTypedExpression' tbl e = case instanceFromTypedExpression e of
  Just ii@InstanceInfo {..}
    | isTrait tbl _instanceInfoInductive ->
        Just ii
  _ ->
    Nothing
