module RegInterpreter where

import App
import CommonOptions
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Interpreter qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg

runReg :: forall r. (Members '[EmbedIO, App] r) => Reg.Module -> Sem r ()
runReg md =
  case md ^. Reg.moduleInfoTable . Reg.infoMainFunction of
    Just sym -> do
      r <- doRun md (Reg.lookupFunInfo md sym)
      case r of
        Left err ->
          exitJuvixError (JuvixError err)
        Right Reg.ValVoid ->
          return ()
        Right val -> do
          renderStdOut (Reg.ppOut (Reg.defaultOptions md) val)
          putStrLn ""
    Nothing ->
      exitMsg (ExitFailure 1) "no 'main' function"
  where
    doRun ::
      Reg.Module ->
      Reg.FunctionInfo ->
      Sem r (Either Reg.RegError Reg.Val)
    doRun md' funInfo = runError $ Reg.runFunctionIO stdin stdout md' [] funInfo
