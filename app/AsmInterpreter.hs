module AsmInterpreter where

import App
import CommonOptions
import Juvix.Compiler.Asm.Data.Module qualified as Asm
import Juvix.Compiler.Asm.Extra qualified as Asm
import Juvix.Compiler.Asm.Interpreter qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Transformation.Validate qualified as Asm

runAsm :: forall r. (Members '[EmbedIO, App] r) => Bool -> Asm.Module -> Sem r ()
runAsm bValidate md =
  let v = if bValidate then Asm.validate' md else Nothing
   in case v of
        Just err ->
          exitJuvixError (JuvixError err)
        Nothing ->
          case md ^. Asm.moduleInfoTable . Asm.infoMainFunction of
            Just sym -> do
              r <- doRun md (Asm.lookupFunInfo md sym)
              case r of
                Left err ->
                  exitJuvixError (JuvixError err)
                Right Asm.ValVoid ->
                  return ()
                Right val -> do
                  renderStdOut (Asm.ppOut (Asm.defaultOptions md) val)
                  putStrLn ""
            Nothing ->
              exitMsg (ExitFailure 1) "no 'main' function"
  where
    doRun ::
      Asm.Module ->
      Asm.FunctionInfo ->
      Sem r (Either Asm.AsmError Asm.Val)
    doRun md' funInfo =
      liftIO $ Asm.catchRunErrorIO (Asm.runCodeIO md' funInfo)
