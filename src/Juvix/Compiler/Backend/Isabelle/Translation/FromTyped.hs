module Juvix.Compiler.Backend.Isabelle.Translation.FromTyped where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as T
import Data.Text qualified as Text
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Extra
import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language

newtype NameSet = NameSet
  { _nameSet :: HashSet Text
  }

newtype NameMap = NameMap
  { _nameMap :: HashMap Name Expression
  }

makeLenses ''NameSet
makeLenses ''NameMap

data Nested a = Nested
  { _nestedElem :: a,
    _nestedPatterns :: NestedPatterns
  }
  deriving stock (Functor)

type NestedPatterns = [(Expression, Nested Pattern)]

makeLenses ''Nested

fromInternal ::
  forall r.
  (Members '[Error JuvixError, Reader EntryPoint, Reader ModuleTable, NameIdGen] r) =>
  Internal.InternalTypedResult ->
  Sem r Result
fromInternal res@Internal.InternalTypedResult {..} = do
  onlyTypes <- (^. entryPointIsabelleOnlyTypes) <$> ask
  itab <- getInternalModuleTable <$> ask
  let md :: Internal.InternalModule
      md = _resultInternalModule
      itab' :: Internal.InternalModuleTable
      itab' = Internal.insertInternalModule itab md
      table :: Internal.InfoTable
      table = Internal.computeCombinedInfoTable itab'
      comments :: [Comment]
      comments = allComments (Internal.getInternalTypedResultComments res)
  go onlyTypes comments table _resultModule
  where
    go :: Bool -> [Comment] -> Internal.InfoTable -> Internal.Module -> Sem r Result
    go onlyTypes comments tab md =
      return $
        Result
          { _resultTheory = goModule onlyTypes tab md,
            _resultModuleId = md ^. Internal.moduleId,
            _resultComments = filter (\c -> c ^. commentInterval . intervalFile == file) comments
          }
      where
        file = getLoc md ^. intervalFile

goModule :: Bool -> Internal.InfoTable -> Internal.Module -> Theory
goModule onlyTypes infoTable Internal.Module {..} =
  Theory
    { _theoryName = overNameText toIsabelleTheoryName _moduleName,
      _theoryImports =
        map
          (overNameText toIsabelleTheoryName . (^. Internal.importModuleName))
          (_moduleBody ^. Internal.moduleImports),
      _theoryStatements = case _modulePragmas ^. pragmasIsabelleIgnore of
        Just (PragmaIsabelleIgnore True) -> []
        _ -> concatMap goMutualBlock (_moduleBody ^. Internal.moduleStatements)
    }
  where
    toIsabelleTheoryName :: Text -> Text
    toIsabelleTheoryName name =
      quote . Text.intercalate "_" $ filter (/= "") $ T.splitOn "." name

    isTypeDef :: Statement -> Bool
    isTypeDef = \case
      StmtDefinition {} -> False
      StmtFunction {} -> False
      StmtSynonym {} -> True
      StmtDatatype {} -> True
      StmtRecord {} -> True

    mkExprCase :: Case -> Expression
    mkExprCase c@Case {..} = case _caseValue of
      ExprIden v ->
        case _caseBranches of
          CaseBranch {..} :| [] ->
            case _caseBranchPattern of
              PatVar v' -> substVar v' v _caseBranchBody
              _ -> ExprCase c
          _ -> ExprCase c
      ExprTuple (Tuple (ExprIden v :| [])) ->
        case _caseBranches of
          CaseBranch {..} :| [] ->
            case _caseBranchPattern of
              PatTuple (Tuple (PatVar v' :| [])) -> substVar v' v _caseBranchBody
              _ -> ExprCase c
          _ -> ExprCase c
      _ -> ExprCase c

    goMutualBlock :: Internal.MutualBlock -> [Statement]
    goMutualBlock Internal.MutualBlock {..} =
      filter (\stmt -> not onlyTypes || isTypeDef stmt) $
        mapMaybe goMutualStatement (toList _mutualStatements)

    checkNotIgnored :: Pragmas -> a -> Maybe a
    checkNotIgnored pragmas x = case pragmas ^. pragmasIsabelleIgnore of
      Just (PragmaIsabelleIgnore True) -> Nothing
      _ -> Just x

    goMutualStatement :: Internal.MutualStatement -> Maybe Statement
    goMutualStatement = \case
      Internal.StatementInductive x -> checkNotIgnored (x ^. Internal.inductivePragmas) $ goInductiveDef x
      Internal.StatementFunction x -> checkNotIgnored (x ^. Internal.funDefPragmas) $ goFunctionDef x
      Internal.StatementAxiom x -> checkNotIgnored (x ^. Internal.axiomPragmas) $ goAxiomDef x

    goInductiveDef :: Internal.InductiveDef -> Statement
    goInductiveDef Internal.InductiveDef {..}
      | length _inductiveConstructors == 1
          && head' _inductiveConstructors ^. Internal.inductiveConstructorIsRecord =
          let tyargs = fst $ Internal.unfoldFunType $ head' _inductiveConstructors ^. Internal.inductiveConstructorType
           in StmtRecord
                RecordDef
                  { _recordDefName = _inductiveName,
                    _recordDefParams = params,
                    _recordDefFields = map goRecordField tyargs,
                    _recordDefDocComment = _inductiveDocComment
                  }
      | otherwise =
          StmtDatatype
            Datatype
              { _datatypeName = _inductiveName,
                _datatypeParams = params,
                _datatypeConstructors = map goConstructorDef _inductiveConstructors,
                _datatypeDocComment = _inductiveDocComment
              }
      where
        params = map goInductiveParameter _inductiveParameters

    goInductiveParameter :: Internal.InductiveParameter -> TypeVar
    goInductiveParameter Internal.InductiveParameter {..} = TypeVar _inductiveParamName

    goRecordField :: Internal.FunctionParameter -> RecordField
    goRecordField param@Internal.FunctionParameter {..} =
      RecordField
        { _recordFieldName = fromMaybe (defaultName (getLoc param) "_") _paramName,
          _recordFieldType = goType _paramType,
          _recordFieldDocComment = Nothing
        }

    goConstructorDef :: Internal.ConstructorDef -> Constructor
    goConstructorDef Internal.ConstructorDef {..} =
      Constructor
        { _constructorName = _inductiveConstructorName,
          _constructorArgTypes = tyargs,
          _constructorDocComment = _inductiveConstructorDocComment
        }
      where
        tyargs = map (goType . (^. Internal.paramType)) (fst $ Internal.unfoldFunType _inductiveConstructorType)

    goDef :: Name -> Internal.Expression -> [Internal.ArgInfo] -> Maybe Internal.Expression -> Maybe Text -> Statement
    goDef name ty argsInfo body comment = case ty of
      Internal.ExpressionUniverse {} ->
        StmtSynonym
          Synonym
            { _synonymName = name',
              _synonymType = goType $ fromMaybe (error "unsupported axiomatic type") body,
              _synonymDocComment = comment
            }
      _
        | isFunction argnames ty body ->
            StmtFunction
              Function
                { _functionName = name',
                  _functionType = goType ty,
                  _functionClauses = goBody loc argnames ty body,
                  _functionDocComment = comment
                }
        | otherwise ->
            StmtDefinition
              Definition
                { _definitionName = name',
                  _definitionType = goType ty,
                  _definitionBody = maybe (ExprUndefined loc) goExpression' body,
                  _definitionDocComment = comment
                }
      where
        argnames =
          map quoteName $ filterTypeArgs 0 ty $ map (fromMaybe (defaultName (getLoc name) "_") . (^. Internal.argInfoName)) argsInfo
        name' = quoteName name
        loc = getLoc name

    isFunction :: [Name] -> Internal.Expression -> Maybe Internal.Expression -> Bool
    isFunction argnames ty = \case
      Just (Internal.ExpressionLambda Internal.Lambda {..})
        | not $ null $ filterTypeArgs 0 ty $ toList $ head _lambdaClauses ^. Internal.lambdaPatterns ->
            True
      _ -> not (null argnames)

    goBody :: Interval -> [Name] -> Internal.Expression -> Maybe Internal.Expression -> NonEmpty Clause
    goBody defLoc argnames ty = \case
      Nothing -> oneClause (ExprUndefined defLoc)
      -- We assume here that all clauses have the same number of patterns
      Just (Internal.ExpressionLambda Internal.Lambda {..})
        | not $ null $ filterTypeArgs 0 ty $ toList $ head _lambdaClauses ^. Internal.lambdaPatterns ->
            nonEmpty' $ goClauses $ toList _lambdaClauses
        | otherwise ->
            oneClause (goExpression'' nset (NameMap mempty) (head _lambdaClauses ^. Internal.lambdaBody))
      Just body -> oneClause (goExpression'' nset (NameMap mempty) body)
      where
        nset :: NameSet
        nset = NameSet $ HashSet.fromList $ map (^. namePretty) argnames

        oneClause :: Expression -> NonEmpty Clause
        oneClause expr =
          nonEmpty'
            [ Clause
                { _clausePatterns = nonEmpty' (map PatVar argnames),
                  _clauseBody = expr
                }
            ]

        goClauses :: [Internal.LambdaClause] -> [Clause]
        goClauses = \case
          cl@Internal.LambdaClause {..} : cls ->
            case npats0 of
              Nested pats [] ->
                Clause
                  { _clausePatterns = nonEmpty' pats,
                    _clauseBody = goExpression'' nset' nmap' _lambdaBody
                  }
                  : goClauses cls
              Nested pats npats ->
                let rhs = goExpression'' nset' nmap' _lambdaBody
                    argnames' = fmap getPatternArgName _lambdaPatterns
                    vnames =
                      fmap
                        ( \(idx :: Int, mname) ->
                            maybe
                              ( defaultName
                                  (getLoc cl)
                                  ( disambiguate
                                      (nset' ^. nameSet)
                                      ("v_" <> show idx)
                                  )
                              )
                              (overNameText (disambiguate (nset' ^. nameSet)))
                              mname
                        )
                        (NonEmpty.zip (nonEmpty' [0 ..]) argnames')
                    nset'' = foldl' (flip (over nameSet . HashSet.insert . (^. namePretty))) nset' vnames
                    remainingBranches = goLambdaClauses'' nset'' nmap' cls
                    valTuple = ExprTuple (Tuple (fmap ExprIden vnames))
                    patTuple = PatTuple (Tuple (nonEmpty' pats))
                    brs = goNestedBranches (getLoc cl) valTuple rhs remainingBranches patTuple (nonEmpty' npats)
                 in [ Clause
                        { _clausePatterns = fmap PatVar vnames,
                          _clauseBody =
                            mkExprCase
                              Case
                                { _caseValue = valTuple,
                                  _caseBranches = brs
                                }
                        }
                    ]
            where
              (npats0, nset', nmap') = goPatternArgsTop (filterTypeArgs 0 ty (toList _lambdaPatterns))
          [] -> []

    goNestedBranches :: Interval -> Expression -> Expression -> [CaseBranch] -> Pattern -> NonEmpty (Expression, Nested Pattern) -> NonEmpty CaseBranch
    goNestedBranches loc caseVal rhs remainingBranches pat npats =
      let val = ExprTuple (Tuple (fmap fst npats))
          pat' = PatTuple (Tuple (fmap ((^. nestedElem) . snd) npats))
          npats' = concatMap ((^. nestedPatterns) . snd) npats
          brs = goNestedBranches' rhs (mkDefaultBranch loc caseVal remainingBranches) (Nested pat' npats')
          remainingBranches' = filter (not . subsumesPattern pat . (^. caseBranchPattern)) remainingBranches
       in CaseBranch
            { _caseBranchPattern = pat,
              _caseBranchBody =
                mkExprCase
                  Case
                    { _caseValue = val,
                      _caseBranches = brs
                    }
            }
            :| remainingBranches'

    mkDefaultBranch :: Interval -> Expression -> [CaseBranch] -> Maybe CaseBranch
    mkDefaultBranch loc val remainingBranches = case remainingBranches of
      [] -> Nothing
      _ ->
        Just $
          CaseBranch
            { _caseBranchPattern = PatVar (defaultName loc "_"),
              _caseBranchBody =
                mkExprCase
                  Case
                    { _caseValue = val,
                      _caseBranches = nonEmpty' remainingBranches
                    }
            }

    goNestedBranches' :: Expression -> Maybe CaseBranch -> Nested Pattern -> NonEmpty CaseBranch
    goNestedBranches' rhs defaultBranch = \case
      Nested pat [] ->
        CaseBranch
          { _caseBranchPattern = pat,
            _caseBranchBody = rhs
          }
          :| toList defaultBranch
      Nested pat npats -> do
        let val = ExprTuple (Tuple (nonEmpty' (map fst npats)))
            pat' = PatTuple (Tuple (nonEmpty' (map ((^. nestedElem) . snd) npats)))
            npats' = concatMap ((^. nestedPatterns) . snd) npats
            brs = goNestedBranches' rhs defaultBranch (Nested pat' npats')
         in CaseBranch
              { _caseBranchPattern = pat,
                _caseBranchBody =
                  mkExprCase
                    Case
                      { _caseValue = val,
                        _caseBranches = brs
                      }
              }
              :| toList defaultBranch

    goFunctionDef :: Internal.FunctionDef -> Statement
    goFunctionDef Internal.FunctionDef {..} = goDef _funDefName _funDefType _funDefArgsInfo (Just _funDefBody) _funDefDocComment

    goAxiomDef :: Internal.AxiomDef -> Statement
    goAxiomDef Internal.AxiomDef {..} = goDef _axiomName _axiomType [] Nothing _axiomDocComment

    goType :: Internal.Expression -> Type
    goType ty = case ty of
      Internal.ExpressionIden x -> goTypeIden x
      Internal.ExpressionApplication x -> goTypeApp x
      Internal.ExpressionFunction x -> goTypeFun x
      Internal.ExpressionLiteral {} -> unsupportedType ty
      Internal.ExpressionHole {} -> unsupportedType ty
      Internal.ExpressionInstanceHole {} -> unsupportedType ty
      Internal.ExpressionLet {} -> unsupportedType ty
      Internal.ExpressionUniverse {} -> unsupportedType ty
      Internal.ExpressionSimpleLambda {} -> unsupportedType ty
      Internal.ExpressionLambda {} -> unsupportedType ty
      Internal.ExpressionCase {} -> unsupportedType ty
      where
        unsupportedType :: Internal.Expression -> a
        unsupportedType e = error ("unsupported type: " <> Internal.ppTrace e)

    mkIndType :: Name -> [Type] -> Type
    mkIndType name params = TyInd $ IndApp ind params
      where
        ind = case HashMap.lookup name (infoTable ^. Internal.infoInductives) of
          Just ii -> case ii ^. Internal.inductiveInfoBuiltin of
            Just Internal.BuiltinBool -> IndBool
            Just Internal.BuiltinNat -> IndNat
            Just Internal.BuiltinInt -> IndInt
            Just Internal.BuiltinList -> IndList
            Just Internal.BuiltinMaybe -> IndOption
            Just Internal.BuiltinPair -> IndTuple
            _ -> IndUser name
          Nothing -> case HashMap.lookup name (infoTable ^. Internal.infoAxioms) of
            Just ai -> case ai ^. Internal.axiomInfoDef . Internal.axiomBuiltin of
              Just Internal.BuiltinString -> IndString
              _ -> IndUser name
            Nothing -> IndUser name

    goTypeIden :: Internal.Iden -> Type
    goTypeIden = \case
      Internal.IdenFunction name -> mkIndType (quoteName name) []
      Internal.IdenConstructor name -> error ("unsupported type: constructor " <> Internal.ppTrace name)
      Internal.IdenVar name -> TyVar $ TypeVar (quoteName name)
      Internal.IdenAxiom name -> mkIndType (quoteName name) []
      Internal.IdenInductive name -> mkIndType (quoteName name) []

    goTypeApp :: Internal.Application -> Type
    goTypeApp app = mkIndType name params
      where
        (ind, args) = Internal.unfoldApplication app
        params = map goType (toList args)
        name = quoteName $ case ind of
          Internal.ExpressionIden (Internal.IdenFunction n) -> n
          Internal.ExpressionIden (Internal.IdenAxiom n) -> n
          Internal.ExpressionIden (Internal.IdenInductive n) -> n
          _ -> error ("unsupported type: " <> Internal.ppTrace app)

    goTypeFun :: Internal.Function -> Type
    goTypeFun Internal.Function {..}
      | Internal.isTypeConstructor lty = goType _functionRight
      | otherwise =
          TyFun $ FunType (goType lty) (goType _functionRight)
      where
        lty = _functionLeft ^. Internal.paramType

    goConstrName :: Name -> Name
    goConstrName name =
      case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
        Just ctrInfo ->
          case ctrInfo ^. Internal.constructorInfoBuiltin of
            Just Internal.BuiltinNatSuc ->
              setNameText "Suc" name
            Just Internal.BuiltinBoolTrue ->
              setNameText "True" name
            Just Internal.BuiltinBoolFalse ->
              setNameText "False" name
            Just Internal.BuiltinMaybeNothing ->
              setNameText "None" name
            Just Internal.BuiltinMaybeJust ->
              setNameText "Some" name
            _ -> quoteName name
        Nothing -> quoteName name

    getArgtys :: Internal.ConstructorInfo -> [Internal.FunctionParameter]
    getArgtys ctrInfo = fst $ Internal.unfoldFunType $ ctrInfo ^. Internal.constructorInfoType

    goFunName :: Expression -> Expression
    goFunName = \case
      ExprIden name ->
        ExprIden $
          case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
            Just funInfo ->
              case funInfo ^. Internal.functionInfoPragmas . pragmasIsabelleFunction of
                Just PragmaIsabelleFunction {..} -> setNameText _pragmaIsabelleFunctionName name
                Nothing -> quoteName name
            Nothing -> quoteName name
      x -> x

    lookupName :: forall r. (Member (Reader NameMap) r) => Name -> Sem r Expression
    lookupName name = do
      nmap <- asks (^. nameMap)
      return $ fromMaybe (ExprIden name) $ HashMap.lookup name nmap

    localName :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => Name -> Name -> Sem r a -> Sem r a
    localName v v' =
      local (over nameSet (HashSet.insert (v' ^. namePretty)))
        . local (over nameMap (HashMap.insert v (ExprIden v')))

    localNames :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => [(Name, Name)] -> Sem r a -> Sem r a
    localNames vs e = foldl' (flip (uncurry localName)) e vs

    withLocalNames :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => NameSet -> NameMap -> Sem r a -> Sem r a
    withLocalNames nset nmap =
      local (const nset) . local (const nmap)

    goRecordFields :: [Internal.FunctionParameter] -> [a] -> [(Name, a)]
    goRecordFields argtys args = case (argtys, args) of
      (ty : argtys', arg' : args') -> (fromMaybe (defaultName (getLoc ty) "_") (ty ^. Internal.paramName), arg') : goRecordFields argtys' args'
      _ -> []

    goExpression' :: Internal.Expression -> Expression
    goExpression' = goExpression'' (NameSet mempty) (NameMap mempty)

    goExpression'' :: NameSet -> NameMap -> Internal.Expression -> Expression
    goExpression'' nset nmap e =
      run $ runReader nset $ runReader nmap $ goExpression e

    goExpression :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => Internal.Expression -> Sem r Expression
    goExpression = \case
      Internal.ExpressionIden x -> goIden x
      Internal.ExpressionApplication x -> goApplication x
      Internal.ExpressionFunction x -> goFunType x
      Internal.ExpressionLiteral x -> goLiteral x
      Internal.ExpressionHole x -> goHole x
      Internal.ExpressionInstanceHole x -> goInstanceHole x
      Internal.ExpressionLet x -> goLet x
      Internal.ExpressionUniverse x -> goUniverse x
      Internal.ExpressionSimpleLambda x -> goSimpleLambda x
      Internal.ExpressionLambda x -> goLambda x
      Internal.ExpressionCase x -> goCase x
      where
        goIden :: Internal.Iden -> Sem r Expression
        goIden iden = case iden of
          Internal.IdenFunction name -> do
            goFunName <$> lookupName name
          Internal.IdenConstructor name ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinNatZero -> return $ ExprLiteral (WithLoc (getLoc name) (LitNumeric 0))
                  _ -> return $ ExprIden (goConstrName name)
              Nothing -> return $ ExprIden (goConstrName name)
          Internal.IdenVar name -> do
            lookupName name
          Internal.IdenAxiom name -> return $ ExprIden (quoteName name)
          Internal.IdenInductive name -> return $ ExprIden (quoteName name)

        goApplication :: Internal.Application -> Sem r Expression
        goApplication app@Internal.Application {..}
          | Just (pragmas, arg1, arg2) <- getIsabelleOperator app =
              mkIsabelleOperator (getLoc app) pragmas arg1 arg2
          | Just x <- getLiteral app =
              return $ ExprLiteral $ WithLoc (getLoc app) (LitNumeric x)
          | Just xs <- getList app = do
              xs' <- mapM goExpression xs
              return $ ExprList (List (getLoc app) xs')
          | Just (arg1, arg2) <- getCons app = do
              arg1' <- goExpression arg1
              arg2' <- goExpression arg2
              return $ ExprCons $ Cons arg1' arg2'
          | Just (val, br1, br2) <- getIf app = do
              val' <- goExpression val
              br1' <- goExpression br1
              br2' <- goExpression br2
              return $ ExprIf $ If val' br1' br2'
          | Just (op, fixity, arg1, arg2) <- getBoolOperator app = do
              arg1' <- goExpression arg1
              arg2' <- goExpression arg2
              return $
                ExprBinop
                  Binop
                    { _binopOperator = op,
                      _binopLeft = arg1',
                      _binopRight = arg2',
                      _binopFixity = fixity
                    }
          | Just (x, y) <- getPair app = do
              x' <- goExpression x
              y' <- goExpression y
              return $ ExprTuple (Tuple (x' :| [y']))
          | Just (name, fields) <- getRecordCreation app = do
              fields' <- mapM (secondM goExpression) fields
              return $ ExprRecord (Record name fields')
          | Just (indName, names, record, fields) <- getRecordUpdate app = do
              record' <- goExpression record
              let names' = map (qualifyRecordProjection indName) names
              nset <- ask @NameSet
              nmap <- ask @NameMap
              let nset' = foldl' (flip (over nameSet . HashSet.insert . (^. namePretty))) nset names'
                  exprs = map (\n -> ExprApp (Application (ExprIden n) record')) names'
                  nmap' = foldl' (flip (over nameMap . uncurry HashMap.insert)) nmap (zipExact names exprs)
              fields' <- mapM (secondM (withLocalNames nset' nmap' . goExpression)) fields
              return $ ExprRecordUpdate (RecordUpdate record' (Record indName fields'))
          | Just (fn, args) <- getIdentApp app = do
              fn' <- goExpression fn
              args' <- mapM goExpression args
              return $ mkApp fn' args'
          | otherwise = do
              l <- goExpression _appLeft
              r <- goExpression _appRight
              return $ ExprApp (Application l r)

        mkIsabelleOperator :: Interval -> PragmaIsabelleOperator -> Internal.Expression -> Internal.Expression -> Sem r Expression
        mkIsabelleOperator loc PragmaIsabelleOperator {..} arg1 arg2 = do
          arg1' <- goExpression arg1
          arg2' <- goExpression arg2
          return $
            ExprBinop
              Binop
                { _binopOperator = defaultName loc _pragmaIsabelleOperatorName,
                  _binopLeft = arg1',
                  _binopRight = arg2',
                  _binopFixity =
                    Fixity
                      { _fixityPrecedence = PrecNat (fromMaybe 0 _pragmaIsabelleOperatorPrec),
                        _fixityArity = OpBinary (fromMaybe AssocNone _pragmaIsabelleOperatorAssoc),
                        _fixityId = Nothing
                      }
                }

        getIsabelleOperator :: Internal.Application -> Maybe (PragmaIsabelleOperator, Internal.Expression, Internal.Expression)
        getIsabelleOperator app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoPragmas . pragmasIsabelleOperator of
                  Just pragma ->
                    case args of
                      Internal.ExpressionIden (Internal.IdenInductive tyname) :| [_, arg1, arg2] ->
                        case HashMap.lookup tyname (infoTable ^. Internal.infoInductives) of
                          Just Internal.InductiveInfo {..} ->
                            case _inductiveInfoBuiltin of
                              Just Internal.BuiltinNat -> Just (pragma, arg1, arg2)
                              Just Internal.BuiltinInt -> Just (pragma, arg1, arg2)
                              _ -> Nothing
                          Nothing -> Nothing
                      _ -> Nothing
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getLiteral :: Internal.Application -> Maybe Integer
        getLiteral app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinFromNat -> lit
                  Just Internal.BuiltinFromInt -> lit
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

            lit :: Maybe Integer
            lit = case args of
              _ :| [_, Internal.ExpressionLiteral l] ->
                case l ^. Internal.withLocParam of
                  Internal.LitString {} -> Nothing
                  Internal.LitNumeric x -> Just x
                  Internal.LitInteger x -> Just x
                  Internal.LitNatural x -> Just x
              _ -> Nothing

        getList :: Internal.Application -> Maybe [Internal.Expression]
        getList app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinListNil -> Just []
                  Just Internal.BuiltinListCons
                    | (_ :| [arg1, Internal.ExpressionApplication app2]) <- args,
                      Just lst <- getList app2 ->
                        Just (arg1 : lst)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getCons :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression)
        getCons app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinListCons
                    | (_ :| [arg1, arg2]) <- args ->
                        Just (arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getIf :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression, Internal.Expression)
        getIf app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinBoolIf
                    | (_ :| [val, br1, br2]) <- args ->
                        Just (val, br1, br2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getBoolOperator :: Internal.Application -> Maybe (Name, Fixity, Internal.Expression, Internal.Expression)
        getBoolOperator app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinBoolAnd
                    | (arg1 :| [arg2]) <- args ->
                        Just (defaultName (getLoc name) "\\<and>", andFixity, arg1, arg2)
                  Just Internal.BuiltinBoolOr
                    | (arg1 :| [arg2]) <- args ->
                        Just (defaultName (getLoc name) "\\<or>", orFixity, arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getPair :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression)
        getPair app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinPairConstr
                    | (_ :| [_, arg1, arg2]) <- args ->
                        Just (arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getRecordCreation :: Internal.Application -> Maybe (Name, [(Name, Internal.Expression)])
        getRecordCreation app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo
                | ctrInfo ^. Internal.constructorInfoRecord ->
                    Just (indName, goRecordFields (getArgtys ctrInfo) (toList args))
                where
                  indName = ctrInfo ^. Internal.constructorInfoInductive
              _ -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getRecordUpdate :: Internal.Application -> Maybe (Name, [Name], Internal.Expression, [(Name, Internal.Expression)])
        getRecordUpdate Internal.Application {..} = case _appLeft of
          Internal.ExpressionLambda Internal.Lambda {..} -> case _lambdaClauses of
            Internal.LambdaClause {..} :| [] -> case fmap (^. Internal.patternArgPattern) _lambdaPatterns of
              Internal.PatternConstructorApp Internal.ConstructorApp {..} :| []
                | all isPatternArgVar _constrAppParameters ->
                    case _lambdaBody of
                      Internal.ExpressionApplication app -> case fn of
                        Internal.ExpressionIden (Internal.IdenConstructor name')
                          | name' == _constrAppConstructor ->
                              case HashMap.lookup name' (infoTable ^. Internal.infoConstructors) of
                                Just ctrInfo
                                  | ctrInfo ^. Internal.constructorInfoRecord ->
                                      let names = map (fromJust . getPatternArgName) _constrAppParameters
                                          fields = goRecordFields (getArgtys ctrInfo) (toList args)
                                          fields' = zipWithExact (\n (n', e) -> (setNameText (n' ^. namePretty) n, e)) names fields
                                          fields'' = filter (\(n, e) -> e /= Internal.ExpressionIden (Internal.IdenVar n)) fields'
                                       in Just (ctrInfo ^. Internal.constructorInfoInductive, map fst fields', _appRight, fields'')
                                _ -> Nothing
                        _ -> Nothing
                        where
                          (fn, args) = Internal.unfoldApplication app
                      _ -> Nothing
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing

        getIdentApp :: Internal.Application -> Maybe (Internal.Expression, [Internal.Expression])
        getIdentApp app = case mty of
          Just (ty, paramsNum) -> Just (fn, args')
            where
              args' = filterTypeArgs paramsNum ty (toList args)
          Nothing -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app
            mty = case fn of
              Internal.ExpressionIden (Internal.IdenFunction name) ->
                case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
                  Just funInfo -> Just (funInfo ^. Internal.functionInfoType, 0)
                  Nothing -> Nothing
              Internal.ExpressionIden (Internal.IdenConstructor name) ->
                case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
                  Just ctrInfo ->
                    Just
                      ( ctrInfo ^. Internal.constructorInfoType,
                        length (ctrInfo ^. Internal.constructorInfoInductiveParameters)
                      )
                  Nothing -> Nothing
              _ -> Nothing

        goFunType :: Internal.Function -> Sem r Expression
        goFunType f = return (ExprUndefined (getLoc f))

        goLiteral :: Internal.LiteralLoc -> Sem r Expression
        goLiteral lit = return $ ExprLiteral $ WithLoc (lit ^. withLocInt) $ case lit ^. withLocParam of
          Internal.LitString s -> LitString s
          Internal.LitNumeric n -> LitNumeric n
          Internal.LitInteger n -> LitNumeric n
          Internal.LitNatural n -> LitNumeric n

        goHole :: Internal.Hole -> Sem r Expression
        goHole h = return (ExprUndefined (getLoc h))

        goInstanceHole :: Internal.InstanceHole -> Sem r Expression
        goInstanceHole h = return (ExprUndefined (getLoc h))

        goLet :: Internal.Let -> Sem r Expression
        goLet Internal.Let {..} = do
          let fdefs = concatMap toFunDefs (toList _letClauses)
          cls <- mapM goFunDef fdefs
          let ns = zipExact (map (^. Internal.funDefName) fdefs) (map (^. letClauseName) cls)
          expr <- localNames ns $ goExpression _letExpression
          return $
            ExprLet
              Let
                { _letClauses = nonEmpty' cls,
                  _letBody = expr
                }
          where
            toFunDefs :: Internal.LetClause -> [Internal.FunctionDef]
            toFunDefs = \case
              Internal.LetFunDef d -> [d]
              Internal.LetMutualBlock Internal.MutualBlockLet {..} -> toList _mutualLet

            goFunDef :: Internal.FunctionDef -> Sem r LetClause
            goFunDef Internal.FunctionDef {..} = do
              nset <- asks (^. nameSet)
              let name' = overNameText (disambiguate nset) _funDefName
              val <- localName _funDefName name' $ goExpression _funDefBody
              return $
                LetClause
                  { _letClauseName = name',
                    _letClauseValue = val
                  }

        goUniverse :: Internal.SmallUniverse -> Sem r Expression
        goUniverse u = return (ExprUndefined (getLoc u))

        goSimpleLambda :: Internal.SimpleLambda -> Sem r Expression
        goSimpleLambda Internal.SimpleLambda {..} = do
          nset <- asks (^. nameSet)
          let v = _slambdaBinder ^. Internal.sbinderVar
              v' = overNameText (disambiguate nset) v
          body <-
            localName v v' $
              goExpression _slambdaBody
          return $
            ExprLambda
              Lambda
                { _lambdaVar = v',
                  _lambdaType = Just $ goType $ _slambdaBinder ^. Internal.sbinderType,
                  _lambdaBody = body
                }

        goLambda :: Internal.Lambda -> Sem r Expression
        goLambda lam@Internal.Lambda {..}
          | patsNum == 0 = goExpression (head _lambdaClauses ^. Internal.lambdaBody)
          | otherwise = goLams vars
          where
            patsNum =
              case _lambdaType of
                Just ty ->
                  length
                    . filterTypeArgs 0 ty
                    . toList
                    $ head _lambdaClauses ^. Internal.lambdaPatterns
                Nothing ->
                  length
                    . filter ((/= Internal.Implicit) . (^. Internal.patternArgIsImplicit))
                    . toList
                    $ head _lambdaClauses ^. Internal.lambdaPatterns
            vars = map (\i -> defaultName (getLoc lam) ("x" <> show i)) [0 .. patsNum - 1]

            goLams :: [Name] -> Sem r Expression
            goLams = \case
              v : vs -> do
                nset <- asks (^. nameSet)
                let v' = overNameText (disambiguate nset) v
                body <-
                  localName v v' $
                    goLams vs
                return $
                  ExprLambda
                    Lambda
                      { _lambdaType = Nothing,
                        _lambdaVar = v',
                        _lambdaBody = body
                      }
              [] -> do
                val <-
                  case vars of
                    [v] -> do
                      lookupName v
                    _ -> do
                      vars' <- mapM lookupName vars
                      return $
                        ExprTuple
                          Tuple
                            { _tupleComponents = nonEmpty' vars'
                            }
                brs <- goLambdaClauses (toList _lambdaClauses)
                return $
                  mkExprCase
                    Case
                      { _caseValue = val,
                        _caseBranches = nonEmpty' brs
                      }

        goCase :: Internal.Case -> Sem r Expression
        goCase Internal.Case {..} = do
          val <- goExpression _caseExpression
          brs <- goCaseBranches (toList _caseBranches)
          return $
            mkExprCase
              Case
                { _caseValue = val,
                  _caseBranches = nonEmpty' brs
                }

        goCaseBranches :: [Internal.CaseBranch] -> Sem r [CaseBranch]
        goCaseBranches = \case
          br@Internal.CaseBranch {..} : brs -> do
            (npat, nset, nmap) <- goPatternArgCase _caseBranchPattern
            case npat of
              Nested pat [] -> do
                rhs <- withLocalNames nset nmap $ goCaseBranchRhs _caseBranchRhs
                brs' <- goCaseBranches brs
                return $
                  CaseBranch
                    { _caseBranchPattern = pat,
                      _caseBranchBody = rhs
                    }
                    : brs'
              Nested pat npats -> do
                let vname = defaultName (getLoc br) (disambiguate (nset ^. nameSet) "v")
                    nset' = over nameSet (HashSet.insert (vname ^. namePretty)) nset
                rhs <- withLocalNames nset' nmap $ goCaseBranchRhs _caseBranchRhs
                remainingBranches <- withLocalNames nset' nmap $ goCaseBranches brs
                let brs' = goNestedBranches (getLoc vname) (ExprIden vname) rhs remainingBranches pat (nonEmpty' npats)
                return
                  [ CaseBranch
                      { _caseBranchPattern = PatVar vname,
                        _caseBranchBody =
                          mkExprCase
                            Case
                              { _caseValue = ExprIden vname,
                                _caseBranches = brs'
                              }
                      }
                  ]
          [] -> return []

        goCaseBranchRhs :: Internal.CaseBranchRhs -> Sem r Expression
        goCaseBranchRhs = \case
          Internal.CaseBranchRhsExpression e -> goExpression e
          Internal.CaseBranchRhsIf {} -> error "unsupported: side conditions"

    goLambdaClauses'' :: NameSet -> NameMap -> [Internal.LambdaClause] -> [CaseBranch]
    goLambdaClauses'' nset nmap cls =
      run $ runReader nset $ runReader nmap $ goLambdaClauses cls

    goLambdaClauses :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => [Internal.LambdaClause] -> Sem r [CaseBranch]
    goLambdaClauses = \case
      cl@Internal.LambdaClause {..} : cls -> do
        (npat, nset, nmap) <- case _lambdaPatterns of
          p :| [] -> goPatternArgCase p
          _ -> do
            (npats, nset, nmap) <- goPatternArgsCase (toList _lambdaPatterns)
            let npat =
                  fmap
                    ( \pats ->
                        PatTuple
                          Tuple
                            { _tupleComponents = nonEmpty' pats
                            }
                    )
                    npats
            return (npat, nset, nmap)
        case npat of
          Nested pat [] -> do
            body <- withLocalNames nset nmap $ goExpression _lambdaBody
            brs <- goLambdaClauses cls
            return $
              CaseBranch
                { _caseBranchPattern = pat,
                  _caseBranchBody = body
                }
                : brs
          Nested pat npats -> do
            let vname = defaultName (getLoc cl) (disambiguate (nset ^. nameSet) "v")
                nset' = over nameSet (HashSet.insert (vname ^. namePretty)) nset
            rhs <- withLocalNames nset' nmap $ goExpression _lambdaBody
            remainingBranches <- withLocalNames nset' nmap $ goLambdaClauses cls
            let brs' = goNestedBranches (getLoc vname) (ExprIden vname) rhs remainingBranches pat (nonEmpty' npats)
            return
              [ CaseBranch
                  { _caseBranchPattern = PatVar vname,
                    _caseBranchBody =
                      mkExprCase
                        Case
                          { _caseValue = ExprIden vname,
                            _caseBranches = brs'
                          }
                  }
              ]
      [] -> return []

    isPatternArgVar :: Internal.PatternArg -> Bool
    isPatternArgVar Internal.PatternArg {..} =
      case _patternArgPattern of
        Internal.PatternVariable {} -> True
        _ -> False

    getPatternArgName :: Internal.PatternArg -> Maybe Name
    getPatternArgName Internal.PatternArg {..} =
      case _patternArgPattern of
        Internal.PatternVariable name -> Just name
        _ -> _patternArgName

    goPatternArgsTop :: [Internal.PatternArg] -> (Nested [Pattern], NameSet, NameMap)
    goPatternArgsTop pats =
      (Nested pats' npats, nset, nmap)
      where
        (npats, (nset, (nmap, pats'))) = run $ runOutputList $ runState (NameSet mempty) $ runState (NameMap mempty) $ goPatternArgs True pats

    goPatternArgCase :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => Internal.PatternArg -> Sem r (Nested Pattern, NameSet, NameMap)
    goPatternArgCase pat = do
      nset <- ask @NameSet
      nmap <- ask @NameMap
      let (npats, (nmap', (nset', pat'))) = run $ runOutputList $ runState nmap $ runState nset $ goPatternArg False pat
      return (Nested pat' npats, nset', nmap')

    goPatternArgsCase :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => [Internal.PatternArg] -> Sem r (Nested [Pattern], NameSet, NameMap)
    goPatternArgsCase pats = do
      nset <- ask @NameSet
      nmap <- ask @NameMap
      let (npats, (nmap', (nset', pats'))) = run $ runOutputList $ runState nmap $ runState nset $ goPatternArgs False pats
      return (Nested pats' npats, nset', nmap')

    goPatternArgs :: forall r. (Members '[State NameSet, State NameMap, Output (Expression, Nested Pattern)] r) => Bool -> [Internal.PatternArg] -> Sem r [Pattern]
    goPatternArgs isTop = mapM (goPatternArg isTop)

    goPatternArg :: forall r. (Members '[State NameSet, State NameMap, Output (Expression, Nested Pattern)] r) => Bool -> Internal.PatternArg -> Sem r Pattern
    goPatternArg isTop Internal.PatternArg {..}
      | Just name <- _patternArgName = do
          binders <- gets (^. nameSet)
          let name' = overNameText (disambiguate binders) name
          modify' (over nameSet (HashSet.insert (name' ^. namePretty)))
          modify' (over nameMap (HashMap.insert name (ExprIden name')))
          npat <- goNestedPattern _patternArgPattern
          output (ExprIden name', npat)
          return $ PatVar name'
      | otherwise =
          goPattern isTop _patternArgPattern

    goNestedPatternArg :: forall r. (Members '[State NameSet, State NameMap] r) => Internal.PatternArg -> Sem r (Nested Pattern)
    goNestedPatternArg Internal.PatternArg {..}
      | Just name <- _patternArgName = do
          binders <- gets (^. nameSet)
          let name' = overNameText (disambiguate binders) name
          modify' (over nameSet (HashSet.insert (name' ^. namePretty)))
          modify' (over nameMap (HashMap.insert name (ExprIden name')))
          npat <- goNestedPattern _patternArgPattern
          return $ Nested (PatVar name') [(ExprIden name', npat)]
      | otherwise =
          goNestedPattern _patternArgPattern

    goNestedPattern :: forall r. (Members '[State NameSet, State NameMap] r) => Internal.Pattern -> Sem r (Nested Pattern)
    goNestedPattern pat = do
      (npats, pat') <- runOutputList $ goPattern False pat
      return $ Nested pat' npats

    goPattern :: forall r. (Members '[State NameSet, State NameMap, Output (Expression, Nested Pattern)] r) => Bool -> Internal.Pattern -> Sem r Pattern
    goPattern isTop = \case
      Internal.PatternVariable name -> do
        binders <- gets (^. nameSet)
        let name' = overNameText (disambiguate binders) name
        modify' (over nameSet (HashSet.insert (name' ^. namePretty)))
        modify' (over nameMap (HashMap.insert name (ExprIden name')))
        return $ PatVar name'
      Internal.PatternConstructorApp x -> goPatternConstructorApp x
      Internal.PatternWildcardConstructor {} -> impossible
      where
        goPatternConstructorApp :: Internal.ConstructorApp -> Sem r Pattern
        goPatternConstructorApp app@Internal.ConstructorApp {..}
          | Just lst <- getListPat _constrAppConstructor _constrAppParameters = do
              pats <- goPatternArgs False lst
              return $ PatList (List (getLoc app) pats)
          | Just (x, y) <- getConsPat _constrAppConstructor _constrAppParameters = do
              x' <- goPatternArg False x
              y' <- goPatternArg False y
              return $ PatCons (Cons x' y')
          | Just (indName, fields) <- getRecordPat _constrAppConstructor _constrAppParameters =
              if
                  | isTop -> do
                      fields' <- mapM (secondM (goPatternArg False)) fields
                      return $ PatRecord (Record indName fields')
                  | otherwise -> do
                      binders <- gets (^. nameSet)
                      let adjustName :: Name -> Expression
                          adjustName name =
                            let name' = qualifyRecordProjection indName name
                             in ExprApp (Application (ExprIden name') (ExprIden vname))
                          vname = defaultName (getLoc app) (disambiguate binders "v")
                          fieldsVars = map (second (fromJust . getPatternArgName)) $ map (first adjustName) $ filter (isPatternArgVar . snd) fields
                          fieldsNonVars = map (first adjustName) $ filter (not . isPatternArgVar . snd) fields
                      modify' (over nameSet (HashSet.insert (vname ^. namePretty)))
                      forM fieldsVars $ \(e, fname) -> do
                        modify' (over nameSet (HashSet.insert (fname ^. namePretty)))
                        modify' (over nameMap (HashMap.insert fname e))
                      fieldsNonVars' <- mapM (secondM goNestedPatternArg) fieldsNonVars
                      forM fieldsNonVars' output
                      return (PatVar vname)
          | Just (x, y) <- getPairPat _constrAppConstructor _constrAppParameters = do
              x' <- goPatternArg False x
              y' <- goPatternArg False y
              return $ PatTuple (Tuple (x' :| [y']))
          | Just p <- getNatPat _constrAppConstructor _constrAppParameters =
              case p of
                Left zero -> return zero
                Right arg -> do
                  arg' <- goPatternArg False arg
                  return (PatConstrApp (ConstrApp (goConstrName _constrAppConstructor) [arg']))
          | otherwise = do
              args <- mapM (goPatternArg False) _constrAppParameters
              return $
                PatConstrApp
                  ConstrApp
                    { _constrAppConstructor = goConstrName _constrAppConstructor,
                      _constrAppArgs = args
                    }

        -- This function cannot be simply merged with `getList` because in patterns
        -- the constructors don't get the type arguments.
        getListPat :: Name -> [Internal.PatternArg] -> Maybe [Internal.PatternArg]
        getListPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinListNil -> Just []
                Just Internal.BuiltinListCons
                  | [arg1, Internal.PatternArg {..}] <- args,
                    Internal.PatternConstructorApp Internal.ConstructorApp {..} <- _patternArgPattern,
                    Just lst <- getListPat _constrAppConstructor _constrAppParameters ->
                      Just (arg1 : lst)
                _ -> Nothing
            Nothing -> Nothing

        getConsPat :: Name -> [Internal.PatternArg] -> Maybe (Internal.PatternArg, Internal.PatternArg)
        getConsPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinListCons
                  | [arg1, arg2] <- args ->
                      Just (arg1, arg2)
                _ -> Nothing
            Nothing -> Nothing

        getRecordPat :: Name -> [Internal.PatternArg] -> Maybe (Name, [(Name, Internal.PatternArg)])
        getRecordPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just ctrInfo
              | ctrInfo ^. Internal.constructorInfoRecord ->
                  Just (indName, goRecordFields (getArgtys ctrInfo) args)
              where
                indName = ctrInfo ^. Internal.constructorInfoInductive
            _ -> Nothing

        getNatPat :: Name -> [Internal.PatternArg] -> Maybe (Either Pattern Internal.PatternArg)
        getNatPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinNatZero
                  | null args ->
                      Just $ Left $ PatZero (getLoc name)
                Just Internal.BuiltinNatSuc
                  | [arg] <- args ->
                      Just $ Right arg
                _ -> Nothing
            Nothing -> Nothing

        getPairPat :: Name -> [Internal.PatternArg] -> Maybe (Internal.PatternArg, Internal.PatternArg)
        getPairPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinPairConstr
                  | [arg1, arg2] <- args ->
                      Just (arg1, arg2)
                _ -> Nothing
            Nothing -> Nothing

    defaultName :: Interval -> Text -> Name
    defaultName loc n =
      Name
        { _nameText = n,
          _nameId = defaultId,
          _nameKind = KNameLocal,
          _nameKindPretty = KNameLocal,
          _namePretty = n,
          _nameLoc = loc,
          _nameFixity = Nothing
        }
      where
        defaultId =
          NameId
            { _nameIdUid = 0,
              _nameIdModuleId = defaultModuleId
            }

    qualifyRecordProjection :: Name -> Name -> Name
    qualifyRecordProjection indName name =
      setNameText (indName ^. namePretty <> "." <> name ^. namePretty) name

    setNameText :: Text -> Name -> Name
    setNameText txt name =
      set namePretty txt
        . set nameText txt
        $ name

    overNameText :: (Text -> Text) -> Name -> Name
    overNameText f name =
      over namePretty f
        . over nameText f
        $ name

    disambiguate :: HashSet Text -> Text -> Text
    disambiguate binders = disambiguate' . quote
      where
        disambiguate' :: Text -> Text
        disambiguate' name
          | name == "?" || name == "" || name == "_" =
              disambiguate' "X"
          | HashSet.member name binders
              || HashSet.member name names =
              disambiguate' (prime name)
          | otherwise =
              name

        names :: HashSet Text
        names =
          HashSet.fromList $
            map quote $
              map (^. Internal.functionInfoName . namePretty) (filter (not . (^. Internal.functionInfoIsLocal)) (HashMap.elems (infoTable ^. Internal.infoFunctions)))
                ++ map (^. Internal.constructorInfoName . namePretty) (HashMap.elems (infoTable ^. Internal.infoConstructors))
                ++ map (^. Internal.inductiveInfoName . namePretty) (HashMap.elems (infoTable ^. Internal.infoInductives))
                ++ map (^. Internal.axiomInfoDef . Internal.axiomName . namePretty) (HashMap.elems (infoTable ^. Internal.infoAxioms))

    quoteName :: Name -> Name
    quoteName name = overNameText goNameText name
      where
        goNameText :: Text -> Text
        goNameText txt
          | Text.elem '.' txt =
              let idenName = snd $ Text.breakOnEnd "." txt
                  modulePath = name ^. nameId . nameIdModuleId . moduleIdPath
                  modulePathText = Text.intercalate "." (modulePath ^. modulePathKeyDir ++ [modulePath ^. modulePathKeyName])
                  moduleName' = toIsabelleTheoryName modulePathText
                  idenName' = quote idenName
               in moduleName' <> "." <> idenName'
          | otherwise = quote txt

    quote :: Text -> Text
    quote txt0
      | Text.elem '.' txt0 = moduleName' <> "." <> idenName'
      | otherwise = quote'' txt0
      where
        (moduleName, idenName) = Text.breakOnEnd "." txt0
        moduleName' = toIsabelleTheoryName (removeLastDot moduleName)
        idenName' = quote'' idenName

        removeLastDot :: Text -> Text
        removeLastDot txt
          | Text.last txt == '.' = Text.init txt
          | otherwise = txt

        quote'' :: Text -> Text
        quote'' =
          quote'
            . Text.filter isLatin1
            . Text.filter (isLetter .||. isDigit .||. (== '_') .||. (== '\''))

        quote' :: Text -> Text
        quote' txt
          | HashSet.member txt reservedNames = quote' (prime txt)
          | txt == "_" = "v"
          | otherwise = case Text.uncons txt of
              Just (c, _) | not (isLetter c) -> quote' ("v_" <> txt)
              _ -> case Text.unsnoc txt of
                Just (_, c) | not (isAlphaNum c || c == '\'') -> quote' (txt <> "'")
                _ -> txt

    reservedNames :: HashSet Text
    reservedNames =
      HashSet.fromList
        [ "begin",
          "bool",
          "case",
          "else",
          "end",
          "if",
          "imports",
          "in",
          "int",
          "let",
          "list",
          "nat",
          "O",
          "OO",
          "of",
          "option",
          "theory",
          "then"
        ]

    filterTypeArgs :: Int -> Internal.Expression -> [a] -> [a]
    filterTypeArgs paramsNum ty args =
      map fst $
        filter (not . snd) $
          zip (drop paramsNum args) (argtys ++ repeat False)
      where
        argtys =
          map Internal.isTypeConstructor
            . map (^. Internal.paramType)
            . fst
            . Internal.unfoldFunType
            $ ty
