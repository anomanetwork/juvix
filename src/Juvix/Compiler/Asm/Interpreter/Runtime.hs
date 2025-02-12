module Juvix.Compiler.Asm.Interpreter.Runtime
  ( module Juvix.Compiler.Asm.Interpreter.Runtime,
    module Juvix.Compiler.Asm.Interpreter.RuntimeState,
    module Juvix.Compiler.Asm.Interpreter.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Debug.Trace qualified as Debug
import GHC.Base qualified as GHC
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Interpreter.Error
import Juvix.Compiler.Asm.Interpreter.RuntimeState
import Juvix.Compiler.Asm.Pretty

data Runtime :: Effect where
  HasCaller :: Runtime m Bool -- is the call stack non-empty?
  PushCallStack :: Code -> Runtime m ()
  PopCallStack :: Runtime m Continuation
  PushValueStack :: Val -> Runtime m ()
  PopValueStack :: Runtime m Val
  TopValueStack :: Runtime m Val
  NullValueStack :: Runtime m Bool
  ReplaceFrame :: Frame -> Runtime m ()
  ReplaceTailFrame :: Frame -> Runtime m ()
  ReadArg :: Offset -> Runtime m Val
  ReadTemp :: RefTemp -> Runtime m Val
  PushTempStack :: Val -> Runtime m ()
  PopTempStack :: Runtime m ()
  LogMessage :: Text -> Runtime m ()
  FlushLogs :: Runtime m ()
  DumpState :: Runtime m ()
  RegisterLocation :: Maybe Location -> Runtime m ()
  RuntimeError :: Text -> Runtime m a

makeSem ''Runtime

runRuntime :: forall r a. Module -> Sem (Runtime ': r) a -> Sem r (RuntimeState, a)
runRuntime md = interp
  where
    iniState =
      RuntimeState
        { _runtimeCallStack = AsmCallStack [],
          _runtimeFrame = emptyFrame,
          _runtimeMessages = [],
          _runtimeLocation = Nothing,
          _runtimeModule = md
        }

    interp :: Sem (Runtime ': r) a -> Sem r (RuntimeState, a)
    interp = reinterpret (runState iniState) $ \case
      HasCaller ->
        not . null . (^. runtimeCallStack . callStack) <$> get
      PushCallStack code -> do
        frm <- (^. runtimeFrame) <$> get
        modify' (over (runtimeCallStack . callStack) (Continuation frm code :))
      PopCallStack -> do
        s <- get
        case s ^. runtimeCallStack . callStack of
          h : t -> do
            modify' (over runtimeCallStack (set callStack t))
            return h
          [] -> throwRuntimeError s "popping empty call stack"
      PushValueStack val ->
        modify' (over (runtimeFrame . frameStack) (over valueStack (val :)))
      PopValueStack -> do
        s <- get
        case s ^. runtimeFrame . frameStack . valueStack of
          v : vs -> do
            modify' (over (runtimeFrame . frameStack) (set valueStack vs))
            return v
          [] -> throwRuntimeError s "popping empty value stack"
      TopValueStack -> do
        s <- get
        case s ^. runtimeFrame . frameStack . valueStack of
          v : _ -> return v
          [] -> throwRuntimeError s "accessing top of empty value stack"
      NullValueStack ->
        get >>= \s -> return $ null $ s ^. runtimeFrame . frameStack . valueStack
      ReplaceFrame frm ->
        modify' (set runtimeFrame frm)
      ReplaceTailFrame frm -> do
        s <- get
        modify' (set runtimeFrame frm {_frameCallLocation = s ^. runtimeFrame . frameCallLocation})
      ReadArg off -> do
        s <- get
        return $
          fromMaybe
            (throwRuntimeError s "invalid argument area read")
            (HashMap.lookup off (s ^. runtimeFrame . frameArgs . argumentArea))
      ReadTemp r -> do
        s <- get
        return $
          fromMaybe
            (throwRuntimeError s "invalid temporary stack read")
            (Stack.nthFromBottom (r ^. refTempOffsetRef . offsetRefOffset) (s ^. runtimeFrame . frameTemp . temporaryStack))
      PushTempStack val ->
        modify' (over (runtimeFrame . frameTemp) (over temporaryStack (Stack.push val)))
      PopTempStack ->
        modify' (over (runtimeFrame . frameTemp) (over temporaryStack Stack.pop))
      LogMessage msg ->
        modify' (over runtimeMessages (msg :))
      FlushLogs ->
        doFlushLogs <$> get
      DumpState -> do
        s :: RuntimeState <- get
        Debug.trace (fromText $ ppTrace (s ^. runtimeModule) s) $ return ()
      RegisterLocation loc ->
        modify' (set runtimeLocation loc)
      RuntimeError msg -> do
        s <- get
        throwRuntimeError s msg

    throwRuntimeError :: forall b. RuntimeState -> Text -> b
    throwRuntimeError s msg =
      doFlushLogs s `GHC.seq`
        throwRunError s msg

    doFlushLogs :: RuntimeState -> ()
    doFlushLogs s =
      let logs = reverse (s ^. runtimeMessages)
       in map' (\x -> Debug.trace (fromText x) ()) logs `GHC.seq` ()

hEvalRuntime :: forall r a. (Member EmbedIO r) => Handle -> Module -> Sem (Runtime ': r) a -> Sem r a
hEvalRuntime h md r = do
  (s, a) <- runRuntime md r
  mapM_ (hPutStrLn h) (reverse (s ^. runtimeMessages))
  return a

evalRuntime :: forall r a. (Member EmbedIO r) => Module -> Sem (Runtime ': r) a -> Sem r a
evalRuntime = hEvalRuntime stdout
