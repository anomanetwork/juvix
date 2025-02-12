module Juvix.Compiler.Tree.Evaluator where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import GHC.IO (unsafePerformIO)
import GHC.Show qualified as S
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Evaluator.Builtins
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty

data EvalError = EvalError
  { _evalErrorLocation :: Maybe Location,
    _evalErrorMsg :: Text
  }

makeLenses ''EvalError

instance Show EvalError where
  show :: EvalError -> String
  show EvalError {..} =
    "evaluation error: "
      ++ fromText _evalErrorMsg

instance Exception.Exception EvalError

eval :: Module -> Node -> Value
eval = hEval stdout

hEval :: Handle -> Module -> Node -> Value
hEval hout md = eval' [] mempty
  where
    eval' :: [Value] -> BL.BinderList Value -> Node -> Value
    eval' args temps node = case node of
      Binop x -> goBinop x
      Unop x -> goUnop x
      ByteArray x -> goByteArrayOp x
      Anoma {} -> evalError "unsupported: Anoma builtin"
      Cairo {} -> evalError "unsupported: Cairo builtin"
      Constant c -> goConstant c
      MemRef x -> goMemRef x
      AllocConstr x -> goAllocConstr x
      AllocClosure x -> goAllocClosure x
      ExtendClosure x -> goExtendClosure x
      Call x -> goCall x
      CallClosures x -> goCallClosures x
      Branch x -> goBranch x
      Case x -> goCase x
      Save x -> goSave x
      where
        evalError :: Text -> a
        evalError msg =
          Exception.throw (EvalError (getNodeLocation node) msg)

        eitherToError :: Either Text Value -> Value
        eitherToError = \case
          Left err -> evalError err
          Right v -> v

        goBinop :: NodeBinop -> Value
        goBinop NodeBinop {..} =
          -- keeping the lets separate ensures that `arg1` is evaluated before `arg2`
          let !arg1 = eval' args temps _nodeBinopArg1
           in let !arg2 = eval' args temps _nodeBinopArg2
               in case _nodeBinopOpcode of
                    PrimBinop op -> eitherToError $ evalBinop op arg1 arg2
                    OpSeq -> arg2

        goUnop :: NodeUnop -> Value
        goUnop NodeUnop {..} =
          let !v = eval' args temps _nodeUnopArg
           in case _nodeUnopOpcode of
                PrimUnop op -> eitherToError $ evalUnop md op v
                OpAssert -> goAssert v
                OpTrace -> goTrace v
                OpFail -> goFail v

        goByteArrayOp :: NodeByteArray -> Value
        goByteArrayOp NodeByteArray {..} =
          case _nodeByteArrayOpcode of
            OpByteArrayLength -> case _nodeByteArrayArgs of
              [nodeArg] ->
                let !arg = eval' args temps nodeArg
                 in case arg of
                      (ValByteArray bs) -> ValInteger (fromIntegral (BS.length bs))
                      _ -> evalError "expected argument to be a ByteString"
              _ -> evalError "expected exactly one argument"
            OpByteArrayFromListUInt8 -> case _nodeByteArrayArgs of
              [nodeArg] ->
                let !arg = eval' args temps nodeArg
                    !listUInt8 :: [Word8] = checkListUInt8 arg
                 in ValByteArray (BS.pack listUInt8)
              _ -> evalError "expected exactly one argument"
              where
                checkListUInt8 :: Value -> [Word8]
                checkListUInt8 = \case
                  ValConstr c -> case c ^. constrArgs of
                    -- is nil
                    [] -> []
                    -- is cons
                    [ValUInt8 w, t] -> w : checkListUInt8 t
                    _ -> evalError "expected either a nullary or a binary constructor"
                  _ -> evalError "expected a constructor"

        goAssert :: Value -> Value
        goAssert = \case
          ValBool True -> ValBool True
          _ -> evalError "assertion failed"

        goFail :: Value -> Value
        goFail v = evalError ("failure: " <> printValue md v)

        goTrace :: Value -> Value
        goTrace v = unsafePerformIO (hPutStrLn hout (printValue md v) >> return v)

        goConstant :: NodeConstant -> Value
        goConstant NodeConstant {..} = constantToValue _nodeConstant

        goMemRef :: NodeMemRef -> Value
        goMemRef NodeMemRef {..} = case _nodeMemRef of
          DRef r -> goDirectRef r
          ConstrRef r -> goField r

        goDirectRef :: DirectRef -> Value
        goDirectRef = \case
          ArgRef OffsetRef {..} ->
            args !! _offsetRefOffset
          TempRef RefTemp {_refTempOffsetRef = OffsetRef {..}} ->
            BL.lookupLevel _offsetRefOffset temps

        goField :: Field -> Value
        goField Field {..} = case goDirectRef _fieldRef of
          ValConstr Constr {..} -> _constrArgs !! _fieldOffset
          _ -> evalError "expected a constructor"

        goAllocConstr :: NodeAllocConstr -> Value
        goAllocConstr NodeAllocConstr {..} =
          let !vs = map' (eval' args temps) _nodeAllocConstrArgs
           in ValConstr
                Constr
                  { _constrTag = _nodeAllocConstrTag,
                    _constrArgs = vs
                  }

        goAllocClosure :: NodeAllocClosure -> Value
        goAllocClosure NodeAllocClosure {..} =
          let !vs = map' (eval' args temps) _nodeAllocClosureArgs
           in ValClosure
                Closure
                  { _closureSymbol = _nodeAllocClosureFunSymbol,
                    _closureArgs = vs
                  }

        goExtendClosure :: NodeExtendClosure -> Value
        goExtendClosure NodeExtendClosure {..} =
          case eval' args temps _nodeExtendClosureFun of
            ValClosure Closure {..} ->
              let !vs = map' (eval' args temps) (toList _nodeExtendClosureArgs)
               in ValClosure
                    Closure
                      { _closureSymbol,
                        _closureArgs = _closureArgs ++ vs
                      }
            _ -> evalError "expected a closure"

        goCall :: NodeCall -> Value
        goCall NodeCall {..} = case _nodeCallType of
          CallFun sym -> doCall sym [] _nodeCallArgs
          CallClosure cl -> doCallClosure cl _nodeCallArgs

        doCall :: Symbol -> [Value] -> [Node] -> Value
        doCall sym vs0 as =
          let !vs = map' (eval' args temps) as
              fi = lookupFunInfo md sym
              vs' = vs0 ++ vs
           in if
                  | length vs' == fi ^. functionArgsNum ->
                      eval' vs' mempty (fi ^. functionCode)
                  | otherwise ->
                      evalError "wrong number of arguments"

        doCallClosure :: Node -> [Node] -> Value
        doCallClosure cl cargs = case eval' args temps cl of
          ValClosure Closure {..} ->
            doCall _closureSymbol _closureArgs cargs
          _ ->
            evalError "expected a closure"

        goCallClosures :: NodeCallClosures -> Value
        goCallClosures NodeCallClosures {..} =
          let !vs = map' (eval' args temps) (toList _nodeCallClosuresArgs)
           in go (eval' args temps _nodeCallClosuresFun) vs
          where
            go :: Value -> [Value] -> Value
            go cl vs = case cl of
              ValClosure Closure {..}
                | argsNum == n ->
                    eval' vs' mempty body
                | argsNum < n ->
                    go (eval' (take argsNum vs') mempty body) (drop argsNum vs')
                | otherwise ->
                    ValClosure
                      Closure
                        { _closureSymbol,
                          _closureArgs = vs'
                        }
                where
                  fi = lookupFunInfo md _closureSymbol
                  argsNum = fi ^. functionArgsNum
                  vs' = _closureArgs ++ vs
                  n = length vs'
                  body = fi ^. functionCode
              _ ->
                evalError "expected a closure"

        goBranch :: NodeBranch -> Value
        goBranch NodeBranch {..} =
          case eval' args temps _nodeBranchArg of
            ValBool True -> eval' args temps _nodeBranchTrue
            ValBool False -> eval' args temps _nodeBranchFalse
            _ -> evalError "expected a boolean"

        goCase :: NodeCase -> Value
        goCase NodeCase {..} =
          case eval' args temps _nodeCaseArg of
            v@(ValConstr Constr {..}) ->
              case find (\CaseBranch {..} -> _caseBranchTag == _constrTag) _nodeCaseBranches of
                Just CaseBranch {..} -> goCaseBranch v _caseBranchSave _caseBranchBody
                Nothing -> goCaseBranch v False (fromMaybe (evalError "no matching branch") _nodeCaseDefault)
            _ ->
              evalError "expected a constructor"

        goCaseBranch :: Value -> Bool -> Node -> Value
        goCaseBranch v bSave body
          | bSave = eval' args (BL.cons v temps) body
          | otherwise = eval' args temps body

        goSave :: NodeSave -> Value
        goSave NodeSave {..} =
          let !v = eval' args temps _nodeSaveArg
           in eval' args (BL.cons v temps) _nodeSaveBody

valueToNode :: Value -> Node
valueToNode = \case
  ValInteger i -> mkConst $ ConstInt i
  ValField f -> mkConst $ ConstField f
  ValBool b -> mkConst $ ConstBool b
  ValString s -> mkConst $ ConstString s
  ValUnit -> mkConst ConstUnit
  ValVoid -> mkConst ConstVoid
  ValConstr Constr {..} ->
    AllocConstr
      NodeAllocConstr
        { _nodeAllocConstrInfo = mempty,
          _nodeAllocConstrTag = _constrTag,
          _nodeAllocConstrArgs = map valueToNode _constrArgs
        }
  ValClosure Closure {..} ->
    AllocClosure
      NodeAllocClosure
        { _nodeAllocClosureInfo = mempty,
          _nodeAllocClosureFunSymbol = _closureSymbol,
          _nodeAllocClosureArgs = map valueToNode _closureArgs
        }
  ValUInt8 i -> mkConst $ ConstUInt8 i
  ValByteArray b -> mkConst $ ConstByteArray b

hEvalIO :: (MonadIO m) => Handle -> Handle -> Module -> FunctionInfo -> m Value
hEvalIO hin hout md funInfo = do
  let !v = hEval hout md (funInfo ^. functionCode)
  hRunIO hin hout md v

-- | Interpret IO actions.
hRunIO :: (MonadIO m) => Handle -> Handle -> Module -> Value -> m Value
hRunIO hin hout md = \case
  ValConstr (Constr (BuiltinTag TagReturn) [x]) -> return x
  ValConstr (Constr (BuiltinTag TagBind) [x, f]) -> do
    x' <- hRunIO hin hout md x
    let code =
          CallClosures
            NodeCallClosures
              { _nodeCallClosuresInfo = mempty,
                _nodeCallClosuresFun = valueToNode f,
                _nodeCallClosuresArgs = valueToNode x' :| []
              }
        !x'' = hEval hout md code
    hRunIO hin hout md x''
  ValConstr (Constr (BuiltinTag TagWrite) [ValString s]) -> do
    hPutStr hout s
    return ValVoid
  ValConstr (Constr (BuiltinTag TagWrite) [arg]) -> do
    hPutStr hout (ppPrint md arg)
    return ValVoid
  ValConstr (Constr (BuiltinTag TagReadLn) []) -> do
    hFlush hout
    s <- hGetLine hin
    return (ValString s)
  val ->
    return val

-- | Catch EvalError and convert it to TreeError.
catchEvalErrorIO :: IO a -> IO (Either TreeError a)
catchEvalErrorIO ma =
  Exception.catch
    (Exception.evaluate ma >>= \ma' -> Right <$> ma')
    (\(ex :: EvalError) -> return (Left (toTreeError ex)))

toTreeError :: EvalError -> TreeError
toTreeError EvalError {..} =
  TreeError
    { _treeErrorMsg = "evaluation error: " <> _evalErrorMsg,
      _treeErrorLoc = _evalErrorLocation
    }
