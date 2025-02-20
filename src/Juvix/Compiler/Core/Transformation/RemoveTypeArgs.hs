module Juvix.Compiler.Core.Transformation.RemoveTypeArgs
  ( removeTypeArgs,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = convert mempty
  where
    unsupported :: (HasCallStack) => forall a. Node -> a
    unsupported node = error ("remove type arguments: unsupported node\n\t" <> ppTrace node)

    convert :: BinderList Binder -> Node -> Node
    convert vars = dmapLR' (vars, go)

    go :: BinderList Binder -> Node -> Recur
    go vars node = case node of
      NVar v@(Var {..}) ->
        let ty = BL.lookup _varIndex vars ^. binderType
         in if
                | isTypeConstr md ty -> End (mkDynamic _varInfo)
                | otherwise -> End (NVar (shiftVar (-k) v))
        where
          k = length (filter (isTypeConstr md . (^. binderType)) (take _varIndex (toList vars)))
      NIdt Ident {..} ->
        let fi = lookupIdentifierInfo md _identSymbol
         in if
                | isTypeConstr md (fi ^. identifierType) ->
                    Recur (lookupIdentifierNode md _identSymbol)
                | otherwise ->
                    Recur node
      NApp App {..} ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar Var {..} ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt Ident {..} ->
                  let fi = lookupIdentifierInfo md _identSymbol
                   in fi ^. identifierType
                NBot Bottom {..} -> _bottomType
                _ -> unsupported node
            args' = filterArgs snd ty args
         in if
                | isTypeConstr md ty ->
                    End (mkDynamic _appInfo)
                | null args' ->
                    End (convert vars h)
                | otherwise ->
                    End (mkApps (convert vars h) (map (second (convert vars)) args'))
      NCtr (Constr {..}) ->
        let ci = lookupConstructorInfo md _constrTag
            ty = ci ^. constructorType
            args' = filterArgs id ty _constrArgs
         in End (mkConstr _constrInfo _constrTag (map (convert vars) args'))
      NCase (Case {..}) ->
        End (mkCase _caseInfo _caseInductive (convert vars _caseValue) (map convertBranch _caseBranches) (fmap (convert vars) _caseDefault))
        where
          nParams :: Int
          nParams = maybe 0 (length . (^. inductiveParams)) (lookupInductiveInfo' md _caseInductive)
          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let paramBinders = map (set binderType mkSmallUniv) (take nParams _caseBranchBinders)
                argBinders = drop nParams _caseBranchBinders
                tyargs = drop nParams (typeArgs (lookupConstructorInfo md _caseBranchTag ^. constructorType))
                argBinders' = zipWith (\b ty -> if isDynamic (b ^. binderType) && isTypeConstr md ty then set binderType ty b else b) argBinders (tyargs ++ repeat mkDynamic')
                binders' =
                  filterBinders
                    (BL.prependRev paramBinders vars)
                    argBinders'
                body' =
                  convert
                    (BL.prependRev argBinders' (BL.prependRev paramBinders vars))
                    _caseBranchBody
             in br
                  { _caseBranchBinders = binders',
                    _caseBranchBindersNum = length binders',
                    _caseBranchBody = body'
                  }
          filterBinders :: BinderList Binder -> [Binder] -> [Binder]
          filterBinders _ [] = []
          filterBinders vars' (b : bs)
            | isTypeConstr md (b ^. binderType) =
                filterBinders (BL.cons b vars') bs
          filterBinders vars' (b : bs) =
            over binderType (convert vars') b : filterBinders (BL.cons b vars') bs
      NLam (Lambda {..})
        | isTypeConstr md (_lambdaBinder ^. binderType) ->
            End (convert (BL.cons _lambdaBinder vars) _lambdaBody)
      NLet (Let {..})
        | isTypeConstr md (_letItem ^. letItemBinder . binderType) ->
            End (convert (BL.cons (_letItem ^. letItemBinder) vars) _letBody)
      NPi (Pi {..})
        | isTypeConstr md (_piBinder ^. binderType) && not (isTypeConstr md _piBody) ->
            End (convert (BL.cons _piBinder vars) _piBody)
      _ -> Recur node
      where
        filterArgs :: (a -> Node) -> Type -> [a] -> [a]
        filterArgs getNode ty args = case (ty, args) of
          (NPi Pi {..}, arg : args') ->
            let ty' = subst (getNode arg) _piBody
                args'' = filterArgs getNode ty' args'
             in if
                    | isTypeConstr md (_piBinder ^. binderType) ->
                        args''
                    | otherwise ->
                        arg : args''
          _ ->
            args

convertIdent :: Module -> IdentifierInfo -> IdentifierInfo
convertIdent md ii =
  ii
    { _identifierType = ty',
      _identifierArgsNum = length tyargs',
      _identifierArgNames = filterArgNames (ii ^. identifierType) (ii ^. identifierArgNames)
    }
  where
    ty' = convertNode md (ii ^. identifierType)
    tyargs' = typeArgs ty'

    filterArgNames :: Type -> [Maybe Text] -> [Maybe Text]
    filterArgNames ty argnames = case (ty, argnames) of
      (NPi Pi {..}, name : argnames')
        | isTypeConstr md (_piBinder ^. binderType) ->
            filterArgNames _piBody argnames'
        | otherwise ->
            name : filterArgNames _piBody argnames'
      _ ->
        argnames

convertConstructor :: Module -> ConstructorInfo -> ConstructorInfo
convertConstructor md ci =
  ci
    { _constructorType = ty',
      _constructorArgsNum = length (typeArgs ty')
    }
  where
    ty' = convertNode md (ci ^. constructorType)

convertInductive :: Module -> InductiveInfo -> InductiveInfo
convertInductive md ii =
  ii
    { _inductiveKind = ty',
      _inductiveParams = map (over paramKind (convertNode md) . fst) $ filter (not . isTypeConstr md . snd) (zipExact (ii ^. inductiveParams) tyargs)
    }
  where
    tyargs = typeArgs (ii ^. inductiveKind)
    ty' = convertNode md (ii ^. inductiveKind)

-- | Remove type arguments and type abstractions.
--
-- Also adjusts the types, removing quantification over types and replacing all
-- type variables with the dynamic type.
--
-- For example,
--
-- `\\A : Type \\f : ((B : Type) -> A -> B -> Pair A B) \\x : A { f A x x }`
--
-- is transformed to
--
-- `\\f : (Any -> Any -> Pair Any Any) \\x : Any { f x x }`
--
-- References:
--  - https://github.com/anoma/juvix/issues/1512
--  - https://github.com/anoma/juvix/pull/1655
--  - https://github.com/anoma/juvix/issues/1930
--  - https://github.com/anoma/juvix/pull/1954
removeTypeArgs :: Module -> Module
removeTypeArgs md =
  filterOutTypeSynonyms
    . mapInductives (convertInductive md)
    . mapConstructors (convertConstructor md)
    . mapIdents (convertIdent md)
    $ mapT (const (convertNode md)) md
