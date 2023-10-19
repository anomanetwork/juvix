module Juvix.Compiler.Internal.Extra.Clonable where

import Juvix.Compiler.Internal.Extra.Base
-- import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

type FreshBindersContext = HashMap NameId NameId

type HolesState = HashMap Hole Hole

class Clonable a where
  freshNameIds :: (Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) => a -> Sem r a

instance Clonable Name where
  freshNameIds n = do
    ctx <- ask @FreshBindersContext
    return $ case ctx ^. at (n ^. nameId) of
      Nothing -> n
      Just uid' -> set nameId uid' n

instance Clonable Iden where
  freshNameIds = traverseOf idenName freshNameIds

instance Clonable Application where
  freshNameIds Application {..} = do
    l' <- freshNameIds _appLeft
    r' <- freshNameIds _appRight
    return
      Application
        { _appLeft = l',
          _appRight = r',
          ..
        }

instance (Clonable a) => Clonable (WithLoc a) where
  freshNameIds = traverseOf withLocParam freshNameIds

instance Clonable Literal where
  freshNameIds = return

instance Clonable Hole where
  freshNameIds h = do
    tbl <- get @HolesState
    case tbl ^. at h of
      Just h' -> return h'
      Nothing -> do
        uid' <- freshNameId
        let h' = set holeId uid' h
        modify' @HolesState (set (at h) (Just h'))
        return h'

instance Clonable SmallUniverse where
  freshNameIds = return

instance (Clonable a) => Clonable (Maybe a) where
  freshNameIds = mapM freshNameIds

underBinder ::
  forall r a binding.
  (HasBinders binding, Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) =>
  binding ->
  (binding -> Sem r a) ->
  Sem r a
underBinder p f = underBinders [p] (f . headDef impossible)

underBindersNonEmpty ::
  forall r a binding.
  (HasBinders binding, Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) =>
  NonEmpty binding ->
  (NonEmpty binding -> Sem r a) ->
  Sem r a
underBindersNonEmpty p f = underBinders (toList p) (f . nonEmpty')

underBinders :: forall r a binding. (HasBinders binding, Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) => [binding] -> ([binding] -> Sem r a) -> Sem r a
underBinders ps f = do
  ctx <- ask @FreshBindersContext
  (ctx', ps') <- runState ctx (mapM goBinders ps)
  local (const ctx') (f ps')
  where
    goBinders :: forall r'. (Members '[State FreshBindersContext, NameIdGen] r') => binding -> Sem r' binding
    goBinders pat = do
      forOf bindersTraversal pat addVar
      where
        addVar :: VarName -> Sem r' VarName
        addVar v = do
          uid' <- freshNameId
          modify' @FreshBindersContext (set (at (v ^. nameId)) (Just uid'))
          return (set nameId uid' v)

instance Clonable CaseBranch where
  freshNameIds CaseBranch {..} =
    underBinder _caseBranchPattern $ \pat' -> do
      body' <- freshNameIds _caseBranchExpression
      return
        CaseBranch
          { _caseBranchPattern = pat',
            _caseBranchExpression = body'
          }

instance Clonable Case where
  freshNameIds Case {..} = do
    e' <- freshNameIds _caseExpression
    ety' <- freshNameIds _caseExpressionType
    wholetype' <- freshNameIds _caseExpressionWholeType
    branches' <- mapM freshNameIds _caseBranches
    return
      Case
        { _caseExpression = e',
          _caseExpressionType = ety',
          _caseExpressionWholeType = wholetype',
          _caseBranches = branches',
          _caseParens
        }

instance Clonable Function where
  freshNameIds Function {..} =
    underBinder _functionLeft $ \l' -> do
      r' <- freshNameIds _functionRight
      return
        Function
          { _functionLeft = l',
            _functionRight = r'
          }

instance (Clonable a) => Clonable (NonEmpty a) where
  freshNameIds = mapM freshNameIds

instance Clonable MutualBlockLet where
  freshNameIds MutualBlockLet {..} =
    underBindersNonEmpty _mutualLet $ \funs -> do
      funs' <- mapM freshNameIds funs
      return
        MutualBlockLet
          { _mutualLet = funs'
          }

instance Clonable LetClause where
  freshNameIds = \case
    LetFunDef f -> LetFunDef <$> freshNameIds f
    LetMutualBlock m -> LetMutualBlock <$> freshNameIds m

instance Clonable Let where
  freshNameIds :: (Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) => Let -> Sem r Let
  freshNameIds Let {..} = do
    underBindersNonEmpty _letClauses $ \clauses -> do
      -- FIXME at this point the functions in the clauses have already been renamed
      -- but we are cloning them again!
      clauses' <- freshNameIds clauses
      e' <- freshNameIds _letExpression
      -- TODO this is wrong!!!!
      return
        Let
          { _letClauses = clauses',
            _letExpression = e'
          }

instance Clonable SimpleBinder where
  freshNameIds SimpleBinder {..} = do
    ty' <- freshNameIds _sbinderType
    return
      SimpleBinder
        { _sbinderType = ty',
          ..
        }

instance Clonable SimpleLambda where
  freshNameIds SimpleLambda {..} =
    underBinder _slambdaBinder $ \bi -> do
      bi' <- freshNameIds bi
      body' <- freshNameIds _slambdaBody
      return
        SimpleLambda
          { _slambdaBinder = bi',
            _slambdaBody = body'
          }

instance Clonable LambdaClause where
  freshNameIds LambdaClause {..} =
    underBindersNonEmpty _lambdaPatterns $ \ps' -> do
      body' <- freshNameIds _lambdaBody
      return
        LambdaClause
          { _lambdaPatterns = ps',
            _lambdaBody = body'
          }

instance Clonable Lambda where
  freshNameIds Lambda {..} = do
    ty' <- freshNameIds _lambdaType
    clauses' <- freshNameIds _lambdaClauses
    return
      Lambda
        { _lambdaType = ty',
          _lambdaClauses = clauses'
        }

instance Clonable Expression where
  freshNameIds :: (Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) => Expression -> Sem r Expression
  freshNameIds = \case
    ExpressionIden i -> ExpressionIden <$> freshNameIds i
    ExpressionApplication a -> ExpressionApplication <$> freshNameIds a
    ExpressionLiteral a -> ExpressionLiteral <$> freshNameIds a
    ExpressionHole a -> ExpressionHole <$> freshNameIds a
    ExpressionUniverse a -> ExpressionUniverse <$> freshNameIds a
    ExpressionCase a -> ExpressionCase <$> freshNameIds a
    ExpressionFunction f -> ExpressionFunction <$> freshNameIds f
    ExpressionInstanceHole h -> ExpressionInstanceHole <$> freshNameIds h
    ExpressionLet l -> ExpressionLet <$> freshNameIds l
    ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> freshNameIds l
    ExpressionLambda l -> ExpressionLambda <$> freshNameIds l

instance Clonable FunctionDef where
  freshNameIds :: (Members '[State HolesState, Reader FreshBindersContext, NameIdGen] r) => FunctionDef -> Sem r FunctionDef
  freshNameIds fun@FunctionDef {..} = do
    ty' <- freshNameIds _funDefType
    underBinder fun $ \fun' -> do
      body' <- freshNameIds _funDefBody
      return
        FunctionDef
          { _funDefName = fun' ^. funDefName,
            _funDefType = ty',
            _funDefBody = body',
            ..
          }
