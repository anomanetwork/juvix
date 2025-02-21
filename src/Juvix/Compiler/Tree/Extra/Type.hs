{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Tree.Extra.Type
  ( module Juvix.Compiler.Tree.Extra.Type,
    module Juvix.Compiler.Tree.Extra.Type.Base,
  )
where

import Juvix.Compiler.Tree.Data.Module.Base
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Type.Base
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Pretty

isConcreteAtomType :: Type -> Bool
isConcreteAtomType = \case
  TyConstr {} -> True
  TyInductive {} -> True
  TyInteger {} -> True
  TyBool {} -> True
  TyString -> True
  TyField -> True
  TyByteArray -> True
  TyRandomGenerator -> True
  TyUnit -> True
  TyVoid -> True
  TyDynamic -> False
  TyFun {} -> False

unfoldType :: Type -> ([Type], Type)
unfoldType ty = (typeArgs ty, typeTarget ty)

-- | Converts e.g. `A -> B -> C -> D` to `(A, B, C) -> D` where `D` is an atom
uncurryType :: Type -> Type
uncurryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = uncurryType (typeTarget ty)
     in mkTypeFun (tyargs ++ typeArgs ty') (typeTarget ty')

-- | Converts e.g. `(A, B, C) -> (D, E) -> F` to `A -> B -> C -> D -> E -> F`
-- where `F` is an atom
curryType :: Type -> Type
curryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    foldr (\tyarg ty'' -> mkTypeFun [tyarg] ty'') (curryType (typeTarget ty)) tyargs

isSubtype :: Type -> Type -> Bool
isSubtype ty1 ty2 =
  let (ty1', ty2') = (curryType ty1, curryType ty2)
   in case (ty1', ty2') of
        (TyDynamic, _) -> True
        (_, TyDynamic) -> True
        (TyConstr TypeConstr {..}, TyInductive TypeInductive {..}) ->
          _typeConstrInductive == _typeInductiveSymbol
        (TyConstr c1, TyConstr c2) ->
          c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
            && c1 ^. typeConstrTag == c2 ^. typeConstrTag
            && all (uncurry isSubtype) (zip (c1 ^. typeConstrFields) (c2 ^. typeConstrFields))
        (TyFun t1, TyFun t2) ->
          let l1 = toList (t1 ^. typeFunArgs)
              l2 = toList (t2 ^. typeFunArgs)
              r1 = t1 ^. typeFunTarget
              r2 = t2 ^. typeFunTarget
           in length l1 == length l2 && all (uncurry isSubtype) (zip l2 l1) && isSubtype r1 r2
        (TyInteger (TypeInteger l1 u1), TyInteger (TypeInteger l2 u2)) ->
          checkBounds (>=) l1 l2 && checkBounds (<=) u1 u2
          where
            checkBounds :: (Integer -> Integer -> Bool) -> Maybe Integer -> Maybe Integer -> Bool
            checkBounds _ Nothing Nothing = True
            checkBounds _ Nothing (Just _) = False
            checkBounds _ (Just _) Nothing = True
            checkBounds cmp (Just x) (Just y) = cmp x y
        (TyBool {}, TyBool {}) -> True
        (TyString, TyString) -> True
        (TyField, TyField) -> True
        (TyByteArray, TyByteArray) -> True
        (TyRandomGenerator, TyRandomGenerator) -> True
        (TyUnit, TyUnit) -> True
        (TyVoid, TyVoid) -> True
        (TyInductive {}, TyInductive {}) -> ty1 == ty2
        (TyUnit, _) -> False
        (_, TyUnit) -> False
        (TyVoid, _) -> False
        (_, TyVoid) -> False
        (TyInteger {}, _) -> False
        (_, TyInteger {}) -> False
        (TyString, _) -> False
        (_, TyString) -> False
        (TyField, _) -> False
        (_, TyField) -> False
        (TyByteArray, _) -> False
        (_, TyByteArray) -> False
        (TyRandomGenerator, _) -> False
        (_, TyRandomGenerator) -> False
        (TyBool {}, _) -> False
        (_, TyBool {}) -> False
        (TyFun {}, _) -> False
        (_, TyFun {}) -> False
        (_, TyConstr {}) -> False

unifyTypes :: forall t e r. (Members '[Error TreeError, Reader (Maybe Location), Reader (Module'' t e)] r) => Type -> Type -> Sem r Type
unifyTypes ty1 ty2 =
  let (ty1', ty2') = (curryType ty1, curryType ty2)
   in case (ty1', ty2') of
        (TyDynamic, x) -> return x
        (x, TyDynamic) -> return x
        (TyInductive TypeInductive {..}, TyConstr TypeConstr {..})
          | _typeInductiveSymbol == _typeConstrInductive ->
              return ty1
        (TyConstr {}, TyInductive {}) -> unifyTypes @t @e ty2 ty1
        (TyConstr c1, TyConstr c2)
          | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
              && c1 ^. typeConstrTag == c2 ^. typeConstrTag -> do
              flds <- zipWithM (unifyTypes @t @e) (c1 ^. typeConstrFields) (c2 ^. typeConstrFields)
              return $ TyConstr (set typeConstrFields flds c1)
        (TyConstr c1, TyConstr c2)
          | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive ->
              return $ TyInductive (TypeInductive (c1 ^. typeConstrInductive))
        (TyFun t1, TyFun t2)
          | length (t1 ^. typeFunArgs) == length (t2 ^. typeFunArgs) -> do
              let args1 = toList (t1 ^. typeFunArgs)
                  args2 = toList (t2 ^. typeFunArgs)
                  tgt1 = t1 ^. typeFunTarget
                  tgt2 = t2 ^. typeFunTarget
              args <- zipWithM (unifyTypes @t @e) args1 args2
              tgt <- unifyTypes @t @e tgt1 tgt2
              return $ TyFun (TypeFun (nonEmpty' args) tgt)
        (TyInteger (TypeInteger l1 u1), TyInteger (TypeInteger l2 u2)) ->
          return $ TyInteger (TypeInteger (unifyBounds min l1 l2) (unifyBounds max u1 u2))
          where
            unifyBounds :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
            unifyBounds _ Nothing _ = Nothing
            unifyBounds _ _ Nothing = Nothing
            unifyBounds f (Just x) (Just y) = Just (f x y)
        (TyBool {}, TyBool {})
          | ty1 == ty2 -> return ty1
        (TyString, TyString) -> return TyString
        (TyField, TyField) -> return TyField
        (TyByteArray, TyByteArray) -> return TyByteArray
        (TyRandomGenerator, TyRandomGenerator) -> return TyRandomGenerator
        (TyUnit, TyUnit) -> return TyUnit
        (TyVoid, TyVoid) -> return TyVoid
        (TyInductive {}, TyInductive {})
          | ty1 == ty2 -> return ty1
        (TyUnit, _) -> err
        (_, TyUnit) -> err
        (TyVoid, _) -> err
        (_, TyVoid) -> err
        (TyInteger {}, _) -> err
        (_, TyInteger {}) -> err
        (TyString, _) -> err
        (_, TyString) -> err
        (TyField, _) -> err
        (_, TyField) -> err
        (TyByteArray, _) -> err
        (_, TyByteArray) -> err
        (TyRandomGenerator, _) -> err
        (_, TyRandomGenerator) -> err
        (TyBool {}, _) -> err
        (_, TyBool {}) -> err
        (TyFun {}, _) -> err
        (_, TyFun {}) -> err
        (TyInductive {}, _) -> err
        (_, TyConstr {}) -> err
  where
    err :: Sem r a
    err = do
      loc <- ask
      md <- ask @(Module'' t e)
      throw $ TreeError loc ("not unifiable: " <> ppTrace' (defaultOptions md) ty1 <> ", " <> ppTrace' (defaultOptions md) ty2)

unifyTypes' :: forall t e r. (Member (Error TreeError) r) => Maybe Location -> Module'' t e -> Type -> Type -> Sem r Type
unifyTypes' loc md ty1 ty2 =
  runReader loc $
    runReader md $
      unifyTypes @t @e ty1 ty2
