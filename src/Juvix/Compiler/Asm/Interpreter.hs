module Juvix.Compiler.Asm.Interpreter
  ( module Juvix.Compiler.Asm.Interpreter,
    module Juvix.Compiler.Asm.Interpreter.Base,
  )
where

import Control.Exception qualified as Exception
import Control.Monad
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Interpreter.Base
import Juvix.Compiler.Asm.Interpreter.Extra
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Compiler.Asm.Pretty
import Juvix.Compiler.Tree.Evaluator.Builtins

-- | Interpret JuvixAsm code for a single function. The returned Val is the
-- value on top of the value stack at exit, i.e., when executing a toplevel
-- Return. Throws a runtime runtimeError if at exit the value stack has more than one
-- element.
runCode :: Module -> FunctionInfo -> IO Val
runCode = hRunCode stdout

hRunCode :: Handle -> Module -> FunctionInfo -> IO Val
hRunCode h md = runM . hEvalRuntime h md . runCodeR md

runCodeR :: (Member Runtime r) => Module -> FunctionInfo -> Sem r Val
runCodeR md funInfo = goCode (funInfo ^. functionCode) >> popLastValueStack
  where
    goCode :: (Member Runtime r) => Code -> Sem r ()
    goCode = \case
      cmd : cont -> goCommand cmd cont
      [] -> return ()

    goCommand :: (Member Runtime r) => Command -> Code -> Sem r ()
    goCommand cmd cont = case cmd of
      Instr CmdInstr {..} -> do
        registerLocation (_cmdInstrInfo ^. commandInfoLocation)
        goInstr (_cmdInstrInfo ^. commandInfoLocation) _cmdInstrInstruction cont
      Branch CmdBranch {..} -> do
        registerLocation (_cmdBranchInfo ^. commandInfoLocation)
        v <- popValueStack
        case v of
          ValBool True -> goCode _cmdBranchTrue
          ValBool False -> goCode _cmdBranchFalse
          _ -> runtimeError "branch on non-boolean"
        goCode cont
      Case CmdCase {..} -> do
        registerLocation (_cmdCaseInfo ^. commandInfoLocation)
        v <- topValueStack
        case v of
          ValConstr c -> branch (c ^. constrTag) _cmdCaseBranches _cmdCaseDefault
          _ -> runtimeError "case on non-data"
        goCode cont
        where
          branch :: (Member Runtime r) => Tag -> [CaseBranch] -> Maybe Code -> Sem r ()
          branch tag bs def = case bs of
            (CaseBranch {..}) : _ | _caseBranchTag == tag -> goCode _caseBranchCode
            _ : bs' -> branch tag bs' def
            _ -> case def of
              Just x -> goCode x
              Nothing -> runtimeError "no matching branch"
      Save CmdSave {..} -> do
        registerLocation (_cmdSaveInfo ^. commandInfoLocation)
        v <- popValueStack
        pushTempStack v
        if
            | _cmdSaveIsTail ->
                goCode _cmdSaveCode
            | otherwise ->
                goCode _cmdSaveCode >> popTempStack >> goCode cont

    goInstr :: (Member Runtime r) => Maybe Location -> Instruction -> Code -> Sem r ()
    goInstr loc instr cont = case instr of
      Binop op -> goBinOp (evalBinop op) >> goCode cont
      Unop op -> goUnop (evalUnop md op) >> goCode cont
      Cairo {} -> runtimeError "unsupported: Cairo builtin"
      Push ref -> do
        v <- getVal ref
        pushValueStack v
        goCode cont
      Pop ->
        popValueStack >> goCode cont
      Assert -> do
        v <- topValueStack
        unless (v == ValBool True) $
          runtimeError "assertion failed"
        goCode cont
      Trace -> do
        v <- topValueStack
        logMessage (printValue md v)
        goCode cont
      Dump -> do
        dumpState
        goCode cont
      Failure -> do
        v <- topValueStack
        runtimeError $ mappend "failure: " (printValue md v)
      Prealloc {} ->
        goCode cont
      AllocConstr tag -> do
        let ci = lookupConstrInfo md tag
        args <- replicateM (ci ^. constructorArgsNum) popValueStack
        pushValueStack (ValConstr (Constr tag args))
        goCode cont
      AllocClosure InstrAllocClosure {..} -> do
        args <- replicateM _allocClosureArgsNum popValueStack
        pushValueStack (ValClosure (Closure _allocClosureFunSymbol args))
        goCode cont
      ExtendClosure InstrExtendClosure {..} -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            unless (_extendClosureArgsNum > 0) $
              runtimeError "invalid closure extension: the number of supplied arguments must be greater than 0"
            extendClosure cl _extendClosureArgsNum
            goCode cont
          _ -> runtimeError "invalid closure extension: expected closure on top of value stack"
      Call ic -> do
        (code, frm) <- getCallDetails loc ic
        pushCallStack cont
        replaceFrame frm
        goCode code
      TailCall ic -> do
        unless (null cont) (runtimeError "invalid tail call")
        (code, frm) <- getCallDetails loc ic
        replaceTailFrame frm
        goCode code
      CallClosures (InstrCallClosures {..}) ->
        callClosures loc False _callClosuresArgsNum cont
      TailCallClosures (InstrCallClosures {..}) ->
        callClosures loc True _callClosuresArgsNum cont
      Return -> do
        unless (null cont) (runtimeError "invalid return")
        isToplevel <- fmap not hasCaller
        if
            | isToplevel -> return ()
            | otherwise -> do
                v <- popLastValueStack
                cont' <- popCallStack
                replaceFrame (cont' ^. contFrame)
                pushValueStack v
                goCode (cont' ^. contCode)

    goBinOp' :: (Member Runtime r) => (Val -> Val -> Sem r Val) -> Sem r ()
    goBinOp' op = do
      v1 <- popValueStack
      v2 <- popValueStack
      v <- op v1 v2
      pushValueStack v

    goBinOp :: (Member Runtime r) => (Val -> Val -> Either Text Val) -> Sem r ()
    goBinOp op = goBinOp' (\x y -> eitherToError (op x y))

    goUnop' :: (Member Runtime r) => (Val -> Sem r Val) -> Sem r ()
    goUnop' op = do
      v <- popValueStack
      v' <- op v
      pushValueStack v'

    goUnop :: (Member Runtime r) => (Val -> Either Text Val) -> Sem r ()
    goUnop op = goUnop' (eitherToError . op)

    getVal :: (Member Runtime r) => Value -> Sem r Val
    getVal = \case
      Constant c -> return (constantToValue c)
      Ref r -> getMemVal r

    getMemVal :: forall r. (Member Runtime r) => MemRef -> Sem r Val
    getMemVal = \case
      DRef dr -> getDirectRef dr
      ConstrRef cr -> do
        ctr <- getDirectRef (cr ^. fieldRef) >>= getConstr
        if
            | cr ^. fieldOffset < length (ctr ^. constrArgs) ->
                return $ (ctr ^. constrArgs) !! (cr ^. fieldOffset)
            | otherwise ->
                runtimeError "invalid constructor field access"
        where
          getConstr :: Val -> Sem r Constr
          getConstr = \case
            ValConstr ctr -> return ctr
            _ -> runtimeError "invalid memory access: expected a constructor"

    getDirectRef :: (Member Runtime r) => DirectRef -> Sem r Val
    getDirectRef = \case
      ArgRef OffsetRef {..} -> readArg _offsetRefOffset
      TempRef r -> readTemp r

    popLastValueStack :: (Member Runtime r) => Sem r Val
    popLastValueStack = do
      v <- popValueStack
      isNull <- nullValueStack
      unless
        isNull
        (runtimeError "value stack not empty on function return")
      return v

    getCallDetails :: forall r. (Member Runtime r) => Maybe Location -> InstrCall -> Sem r (Code, Frame)
    getCallDetails loc InstrCall {..} = case _callType of
      CallFun sym -> do
        let fi = lookupFunInfo md sym
        unless
          (_callArgsNum == fi ^. functionArgsNum)
          (runtimeError "invalid direct call: supplied arguments number not equal to expected arguments number")
        args <- replicateM (fi ^. functionArgsNum) popValueStack
        return (fi ^. functionCode, frameFromFunctionInfo loc fi args)
      CallClosure -> do
        cl <- popValueStack >>= closureFromValue
        let fi = lookupFunInfo md (cl ^. closureSymbol)
            clArgs = length (cl ^. closureArgs)
        unless
          (clArgs < fi ^. functionArgsNum)
          (runtimeError "invalid closure: too many arguments")
        unless
          (clArgs + _callArgsNum == fi ^. functionArgsNum)
          (runtimeError "invalid indirect call: supplied arguments number not equal to expected arguments number")
        frm <- getCallFrame loc cl fi _callArgsNum
        return (fi ^. functionCode, frm)
        where
          closureFromValue :: Val -> Sem r Closure
          closureFromValue = \case
            ValClosure cl -> return cl
            _ -> runtimeError "invalid indirect call: expected closure on top of value stack"

    getCallFrame :: (Member Runtime r) => Maybe Location -> Closure -> FunctionInfo -> Int -> Sem r Frame
    getCallFrame loc cl fi argsNum = do
      args <- replicateM argsNum popValueStack
      return $ frameFromFunctionInfo loc fi ((cl ^. closureArgs) ++ args)

    extendClosure :: (Member Runtime r) => Closure -> Int -> Sem r ()
    extendClosure cl n = do
      args <- replicateM n popValueStack
      pushValueStack
        ( ValClosure
            ( Closure
                (cl ^. closureSymbol)
                (cl ^. closureArgs ++ args)
            )
        )

    callClosures :: (Member Runtime r) => Maybe Location -> Bool -> Int -> Code -> Sem r ()
    callClosures loc isTail argsNum cont = do
      v <- popValueStack
      case v of
        ValClosure cl -> do
          let fi = lookupFunInfo md (cl ^. closureSymbol)
          let n = fi ^. functionArgsNum - length (cl ^. closureArgs)
          when
            (n < 0)
            (runtimeError "invalid closure: too many arguments")
          if
              | n > argsNum -> do
                  extendClosure cl argsNum
                  if
                      | isTail -> goInstr loc Return cont
                      | otherwise -> goCode cont
              | n == argsNum -> do
                  frm <- getCallFrame loc cl fi n
                  if
                      | isTail -> do
                          unless (null cont) $
                            runtimeError "invalid tail call"
                          replaceTailFrame frm
                      | otherwise -> do
                          pushCallStack cont
                          replaceFrame frm
                  goCode (fi ^. functionCode)
              | otherwise -> do
                  let instr = mkInstr ((if isTail then TailCallClosures else CallClosures) (InstrCallClosures (argsNum - n)))
                  frm <- getCallFrame loc cl fi n
                  pushCallStack (instr : cont)
                  replaceFrame frm
                  goCode (fi ^. functionCode)
        _ -> runtimeError "invalid indirect call: expected closure on top of value stack"

    eitherToError :: (Member Runtime r) => Either Text Val -> Sem r Val
    eitherToError = \case
      Left err -> runtimeError err
      Right v -> return v

-- | Interpret JuvixAsm code and the resulting IO actions.
runCodeIO :: Module -> FunctionInfo -> IO Val
runCodeIO = hRunCodeIO stdin stdout

hRunCodeIO :: Handle -> Handle -> Module -> FunctionInfo -> IO Val
hRunCodeIO hin hout md funInfo = do
  v <- hRunCode hout md funInfo
  hRunIO hin hout md funInfo v

-- | Interpret IO actions.
hRunIO :: Handle -> Handle -> Module -> FunctionInfo -> Val -> IO Val
hRunIO hin hout md funInfo = \case
  ValConstr (Constr (BuiltinTag TagReturn) [x]) -> return x
  ValConstr (Constr (BuiltinTag TagBind) [x, f]) -> do
    x' <- hRunIO hin hout md funInfo x
    let code = [Instr (CmdInstr (CommandInfo Nothing) (Call (InstrCall CallClosure 1)))]
    let r =
          pushValueStack x'
            >> pushValueStack f
            >> runCodeR md funInfo {_functionCode = code}
    x'' <- runM (hEvalRuntime hout md r)
    hRunIO hin hout md funInfo x''
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

catchRunErrorIO :: IO a -> IO (Either AsmError a)
catchRunErrorIO ma =
  Exception.catch
    (Exception.evaluate ma >>= \ma' -> ma' <&> Right)
    (\(ex :: RunError) -> return (Left (toAsmError ex)))

toAsmError :: RunError -> AsmError
toAsmError (RunError {..}) =
  AsmError
    { _asmErrorMsg =
        "runtime error: "
          <> _runErrorMsg
          <> "\n\nStacktrace\n----------\n\n"
          <> ppTrace (_runErrorState ^. runtimeModule) _runErrorState,
      _asmErrorLoc = _runErrorState ^. runtimeLocation
    }
