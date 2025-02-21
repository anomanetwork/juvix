module Commands.Dev.Asm.Compile where

import Commands.Base
import Commands.Dev.Asm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => AsmCompileOptions -> Sem r ()
runCommand opts = do
  file <- getMainFile (Just (opts ^. compileInputFile))
  s <- readFile file
  md <- fromRightGenericError (Asm.runParser file s)
  ep <- getEntryPoint (Just (opts ^. compileInputFile))
  tgt <- getTarget (opts ^. compileTarget)
  let entryPoint :: EntryPoint
      entryPoint =
        ep
          { _entryPointTarget = Just tgt,
            _entryPointDebug = opts ^. compileDebug
          }
  case opts ^. compileTarget of
    AppTargetReg -> do
      regFile <- Compile.outputFile opts
      r <-
        runReader entryPoint
          . runError @JuvixError
          . asmToReg
          $ md
      md' <- getRight r
      let code = Reg.ppPrint md' (Reg.computeCombinedInfoTable md')
      writeFileEnsureLn regFile code
    AppTargetCasm -> do
      casmFile <- Compile.outputFile opts
      r <-
        runReader entryPoint
          . runError @JuvixError
          . asmToCasm
          $ md
      Casm.Result {..} <- getRight r
      writeFileEnsureLn casmFile (toPlainText $ Casm.ppProgram _resultCode)
    AppTargetCairo -> do
      cairoFile <- Compile.outputFile opts
      r <-
        runReader entryPoint
          . runError @JuvixError
          . asmToCairo
          $ md
      res <- getRight r
      liftIO $ JSON.encodeFile (toFilePath cairoFile) res
    _ -> do
      C.MiniCResult {..} <-
        getRight @JuvixError
          . run
          . runReader entryPoint
          . runError
          $ asmToMiniC md
      buildDir <- askBuildDir
      ensureDir buildDir
      cFile <- inputCFile file
      writeFileEnsureLn cFile _resultCCode
      outfile <- Compile.outputFile opts
      Compile.runCommand
        opts
          { _compileInputFile = AppPath (preFileFromAbs cFile) False,
            _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
          }
  where
    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      AppTargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      AppTargetNative64 -> return Backend.TargetCNative64
      AppTargetReg -> return Backend.TargetReg
      AppTargetCasm -> return Backend.TargetCairo
      AppTargetCairo -> return Backend.TargetCairo
      AppTargetRiscZeroRust -> err "RISC0 Rust"
      AppTargetAnoma -> err "Anoma"
      AppTargetTree -> err "JuvixTree"
      AppTargetCore -> err "JuvixCore"
      AppTargetAsm -> err "JuvixAsm"
      where
        err :: Text -> Sem r a
        err tgt = exitMsg (ExitFailure 1) ("error: " <> tgt <> " target not supported for JuvixAsm")

inputCFile :: (Members '[App] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
