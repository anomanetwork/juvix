module Juvix.Compiler.Reg.Interpreter
  ( module Juvix.Compiler.Reg.Interpreter,
    module Juvix.Compiler.Reg.Interpreter.Base,
    module Juvix.Compiler.Reg.Error,
  )
where

import Control.Monad.ST
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MV
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Error
import Juvix.Compiler.Reg.Extra.Info
import Juvix.Compiler.Reg.Interpreter.Base
import Juvix.Compiler.Reg.Interpreter.Error
import Juvix.Compiler.Reg.Pretty
import Juvix.Compiler.Tree.Evaluator.Builtins
import System.IO.Unsafe (unsafePerformIO)

type Vars s = MV.MVector s (Maybe Val)

type Args = Vec.Vector Val

runFunction :: forall r. (Members '[Error RegError, EmbedIO] r) => Handle -> Module -> [Val] -> FunctionInfo -> Sem r Val
runFunction hout md args0 info0 = do
  r <- catchRunError (runST (goFun args0 info0))
  case r of
    Left err -> throw err
    Right v -> return v
  where
    localVarsNum :: HashMap Symbol Int
    localVarsNum = HashMap.map (computeLocalVarsNum . (^. functionCode)) (md ^. moduleInfoTable . infoFunctions)

    goFun :: [Val] -> FunctionInfo -> ST s Val
    goFun args info = do
      tmps <- MV.replicate localsNum Nothing
      go (Vec.fromList args) tmps (info ^. functionCode)
      where
        localsNum :: Int
        localsNum = fromJust $ HashMap.lookup (info ^. functionSymbol) localVarsNum

    go :: Args -> Vars s -> Code -> ST s Val
    go args tmps = \case
      instr : instrs -> case instr of
        Nop -> go args tmps instrs
        Binop x -> goBinop args tmps instrs x
        Unop x -> goUnop args tmps instrs x
        Cairo {} -> throwRunError "unsupported: Cairo builtin" Nothing
        Assign x -> goAssign args tmps instrs x
        Assert x -> goAssert args tmps instrs x
        Trace x -> goTrace args tmps instrs x
        Dump -> goDump args tmps instrs
        Failure x -> goFailure args tmps instrs x
        Prealloc x -> goPrealloc args tmps instrs x
        Alloc x -> goAlloc args tmps instrs x
        AllocClosure x -> goAllocClosure args tmps instrs x
        ExtendClosure x -> goExtendClosure args tmps instrs x
        Call x -> goCall args tmps instrs x
        TailCall x -> goTailCall args tmps instrs x
        CallClosures x -> goCallClosures args tmps instrs x
        TailCallClosures x -> goTailCallClosures args tmps instrs x
        Return x -> goReturn args tmps instrs x
        If x -> goIf args tmps instrs x
        Branch x -> goBranch args tmps instrs x
        Case x -> goCase args tmps instrs x
        Block x -> goBlock args tmps instrs x
      [] ->
        return $ ValVoid

    readVarRef :: Args -> Vars s -> VarRef -> ST s Val
    readVarRef args tmps VarRef {..} = case _varRefGroup of
      VarGroupArgs -> return $ args Vec.! _varRefIndex
      VarGroupLocal -> do
        mv <- MV.read tmps _varRefIndex
        case mv of
          Just v -> return v
          Nothing -> throwRunError ("reading uninitialized temporary variable tmp[" <> show _varRefIndex <> "]") Nothing

    readConstrRef :: Args -> Vars s -> ConstrField -> ST s Val
    readConstrRef args tmps ConstrField {..} = do
      v <- readVarRef args tmps _constrFieldRef
      case v of
        ValConstr Constr {..}
          | _constrFieldIndex < length _constrArgs ->
              return $ _constrArgs !! _constrFieldIndex
          | otherwise ->
              throwRunError "wrong number of constructor arguments" Nothing
        _ ->
          throwRunError "expected a constructor" Nothing

    readValue :: Args -> Vars s -> Value -> ST s Val
    readValue args tmps = \case
      ValConst c -> return $ constantToValue c
      CRef r -> readConstrRef args tmps r
      VRef r -> readVarRef args tmps r

    writeVarRef :: Args -> Vars s -> VarRef -> Val -> ST s ()
    writeVarRef _ tmps VarRef {..} val = case _varRefGroup of
      VarGroupLocal ->
        MV.write tmps _varRefIndex (Just val)
      VarGroupArgs ->
        throwRunError "function arguments are not writable" Nothing

    goBinop :: Args -> Vars s -> Code -> InstrBinop -> ST s Val
    goBinop args tmps instrs InstrBinop {..} = do
      v1 <- readValue args tmps _instrBinopArg1
      v2 <- readValue args tmps _instrBinopArg2
      writeVarRef args tmps _instrBinopResult (binop _instrBinopOpcode v1 v2)
      go args tmps instrs

    binop :: BinaryOp -> Val -> Val -> Val
    binop op v1 v2 = case evalBinop op v1 v2 of
      Left err -> throwRunError err Nothing
      Right v -> v

    goUnop :: Args -> Vars s -> Code -> InstrUnop -> ST s Val
    goUnop args tmps instrs InstrUnop {..} = do
      val <- readValue args tmps _instrUnopArg
      writeVarRef args tmps _instrUnopResult (unop _instrUnopOpcode val)
      go args tmps instrs

    unop :: UnaryOp -> Val -> Val
    unop op v = case evalUnop md op v of
      Left err -> throwRunError err Nothing
      Right v' -> v'

    goAssign :: Args -> Vars s -> Code -> InstrAssign -> ST s Val
    goAssign args tmps instrs InstrAssign {..} = do
      val <- readValue args tmps _instrAssignValue
      writeVarRef args tmps _instrAssignResult val
      go args tmps instrs

    goAssert :: Args -> Vars s -> Code -> InstrAssert -> ST s Val
    goAssert args tmps instrs InstrAssert {..} = do
      val <- readValue args tmps _instrAssertValue
      case val of
        ValBool True ->
          go args tmps instrs
        _ ->
          throwRunError "assertion failed" Nothing

    goTrace :: Args -> Vars s -> Code -> InstrTrace -> ST s Val
    goTrace args tmps instrs InstrTrace {..} = do
      val <- readValue args tmps _instrTraceValue
      void $ unsafePerformIO $ do
        hPutStrLn hout (printVal val)
        return (pure ValVoid)
      go args tmps instrs

    goDump :: Args -> Vars s -> Code -> ST s Val
    goDump args tmps instrs = do
      void $ unsafePerformIO $ do
        hPutStrLn hout "<dump>"
        return (pure ValVoid)
      go args tmps instrs

    goFailure :: Args -> Vars s -> Code -> InstrFailure -> ST s Val
    goFailure args tmps _ InstrFailure {..} = do
      val <- readValue args tmps _instrFailureValue
      throwRunError ("failure: " <> printVal val) Nothing

    goPrealloc :: Args -> Vars s -> Code -> InstrPrealloc -> ST s Val
    goPrealloc args tmps instrs InstrPrealloc {} =
      go args tmps instrs

    goAlloc :: Args -> Vars s -> Code -> InstrAlloc -> ST s Val
    goAlloc args tmps instrs InstrAlloc {..} = do
      vals <- mapM (readValue args tmps) _instrAllocArgs
      let val = ValConstr (Constr _instrAllocTag vals)
      writeVarRef args tmps _instrAllocResult val
      go args tmps instrs

    goAllocClosure :: Args -> Vars s -> Code -> InstrAllocClosure -> ST s Val
    goAllocClosure args tmps instrs InstrAllocClosure {..} = do
      vals <- mapM (readValue args tmps) _instrAllocClosureArgs
      let val = ValClosure (Closure _instrAllocClosureSymbol vals)
      writeVarRef args tmps _instrAllocClosureResult val
      go args tmps instrs

    goExtendClosure :: Args -> Vars s -> Code -> InstrExtendClosure -> ST s Val
    goExtendClosure args tmps instrs InstrExtendClosure {..} = do
      vals <- mapM (readValue args tmps) _instrExtendClosureArgs
      cl <- readVarRef args tmps _instrExtendClosureValue
      case cl of
        ValClosure (Closure sym vs) -> do
          let val = ValClosure (Closure sym (vs ++ vals))
          writeVarRef args tmps _instrExtendClosureResult val
          go args tmps instrs
        _ ->
          throwRunError "expected a closure" Nothing

    goCall :: Args -> Vars s -> Code -> InstrCall -> ST s Val
    goCall args tmps instrs InstrCall {..} = do
      vals <- mapM (readValue args tmps) _instrCallArgs
      case _instrCallType of
        CallFun sym -> do
          val <- goFun vals fi
          writeVarRef args tmps _instrCallResult val
          go args tmps instrs
          where
            fi = lookupFunInfo md sym
        CallClosure r -> do
          cl <- readVarRef args tmps r
          case cl of
            ValClosure (Closure sym vs) -> do
              val <- goFun (vs ++ vals) fi
              writeVarRef args tmps _instrCallResult val
              go args tmps instrs
              where
                fi = lookupFunInfo md sym
            _ ->
              throwRunError "expected a closure" Nothing

    goTailCall :: Args -> Vars s -> Code -> InstrTailCall -> ST s Val
    goTailCall args tmps instrs InstrTailCall {..}
      | null instrs = do
          vals <- mapM (readValue args tmps) _instrTailCallArgs
          case _instrTailCallType of
            CallFun sym -> goFun vals fi
              where
                fi = lookupFunInfo md sym
            CallClosure r -> do
              cl <- readVarRef args tmps r
              case cl of
                ValClosure (Closure sym vs) -> goFun (vs ++ vals) fi
                  where
                    fi = lookupFunInfo md sym
                _ ->
                  throwRunError "expected a closure" Nothing
      | otherwise =
          throwRunError "not a tail call" Nothing

    goCallClosures :: Args -> Vars s -> Code -> InstrCallClosures -> ST s Val
    goCallClosures args tmps instrs InstrCallClosures {..} = do
      cl <- readVarRef args tmps _instrCallClosuresValue
      vals <- mapM (readValue args tmps) _instrCallClosuresArgs
      val <- goClosures cl vals
      writeVarRef args tmps _instrCallClosuresResult val
      go args tmps instrs

    goTailCallClosures :: Args -> Vars s -> Code -> InstrTailCallClosures -> ST s Val
    goTailCallClosures args tmps instrs InstrTailCallClosures {..}
      | null instrs = do
          cl <- readVarRef args tmps _instrTailCallClosuresValue
          vals <- mapM (readValue args tmps) _instrTailCallClosuresArgs
          goClosures cl vals
      | otherwise =
          throwRunError "not a tail call" Nothing

    goClosures :: Val -> [Val] -> ST s Val
    goClosures cl vals = case cl of
      ValClosure (Closure sym vs)
        | fi ^. functionArgsNum == n ->
            goFun (vs ++ vals) fi
        | fi ^. functionArgsNum > n ->
            return $ ValClosure (Closure sym (vs ++ vals))
        | otherwise -> do
            let vs' = vs ++ vals
            cl' <- goFun (take (fi ^. functionArgsNum) vs') fi
            goClosures cl' (drop (fi ^. functionArgsNum) vs')
        where
          fi = lookupFunInfo md sym
          n = length vs + length vals
      _ ->
        throwRunError "expected a closure" Nothing

    goReturn :: Args -> Vars s -> Code -> InstrReturn -> ST s Val
    goReturn args tmps instrs InstrReturn {..}
      | null instrs =
          readValue args tmps _instrReturnValue
      | otherwise =
          throwRunError "return not in tail position" Nothing

    goIf :: Args -> Vars s -> Code -> InstrIf -> ST s Val
    goIf args tmps instrs InstrIf {..} = do
      arg1 <- readValue args tmps _instrIfArg1
      arg2 <- readValue args tmps _instrIfArg2
      let val = binop (OpBool _instrIfOp) arg1 arg2
      r <- case val of
        ValBool True -> go args tmps _instrIfTrue
        ValBool False -> go args tmps _instrIfFalse
        _ -> throwRunError "expected a boolean" Nothing
      goNext args tmps r instrs

    goBranch :: Args -> Vars s -> Code -> InstrBranch -> ST s Val
    goBranch args tmps instrs InstrBranch {..} = do
      val <- readValue args tmps _instrBranchValue
      r <- case val of
        ValBool True -> go args tmps _instrBranchTrue
        ValBool False -> go args tmps _instrBranchFalse
        _ -> throwRunError "expected a boolean" Nothing
      goNext args tmps r instrs

    goCase :: Args -> Vars s -> Code -> InstrCase -> ST s Val
    goCase args tmps instrs InstrCase {..} = do
      val <- readValue args tmps _instrCaseValue
      r <-
        case val of
          ValConstr (Constr tag _) -> do
            let mb = find (\CaseBranch {..} -> _caseBranchTag == tag) _instrCaseBranches
            case mb of
              Just CaseBranch {..} ->
                go args tmps _caseBranchCode
              Nothing -> case _instrCaseDefault of
                Just instrs' ->
                  go args tmps instrs'
                Nothing ->
                  throwRunError "no matching case branch" Nothing
          _ ->
            throwRunError "expected a constructor" Nothing
      goNext args tmps r instrs

    goBlock :: Args -> Vars s -> Code -> InstrBlock -> ST s Val
    goBlock args tmps instrs InstrBlock {..} = do
      val <- go args tmps _instrBlockCode
      goNext args tmps val instrs

    goNext :: Args -> Vars s -> Val -> Code -> ST s Val
    goNext args tmps val instrs
      | null instrs = return val
      | otherwise = case val of
          ValVoid -> go args tmps instrs
          _ -> throwRunError "return not in tail position" Nothing

    printVal :: Val -> Text
    printVal = \case
      ValString s -> s
      v -> ppPrint md v

runIO :: forall r. (Members '[Error RegError, EmbedIO] r) => Handle -> Handle -> Module -> Val -> Sem r Val
runIO hin hout md = \case
  ValConstr (Constr (BuiltinTag TagReturn) [x]) ->
    return x
  ValConstr (Constr (BuiltinTag TagBind) [x, f]) -> do
    x' <- runIO hin hout md x
    case f of
      ValClosure (Closure sym args) -> do
        let fi = lookupFunInfo md sym
        x'' <- runFunction hout md (args ++ [x']) fi
        runIO hin hout md x''
      _ ->
        throw $
          RegError
            { _regErrorMsg = "expected a closure",
              _regErrorLoc = Nothing
            }
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

runFunctionIO :: forall r. (Members '[Error RegError, EmbedIO] r) => Handle -> Handle -> Module -> [Val] -> FunctionInfo -> Sem r Val
runFunctionIO hin hout md args funInfo = do
  val <- runFunction hout md args funInfo
  runIO hin hout md val
