module Casm.Reg.Base where

import Base
import Casm.Run.Base qualified as Run
import Data.Aeson
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Data.PPOutput
import Reg.Run.Base qualified as Reg

compileAssertion' :: EntryPoint -> Maybe (Path Abs File) -> Path Abs Dir -> Path Abs File -> Symbol -> Reg.Module -> (String -> IO ()) -> Assertion
compileAssertion' entryPoint inputFile _ outputFile _ md step = do
  step "Translate to CASM"
  case run . runError @JuvixError . runReader entryPoint $ regToCasm md of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right Result {..} -> do
      step "Interpret"
      hout <- openFile (toFilePath outputFile) WriteMode
      rv <- Run.doRun hout _resultLabelInfo _resultCode inputFile
      case rv of
        Left e -> do
          hClose hout
          assertFailure (show (pretty (fromJuvixError @GenericError (JuvixError @CasmError e))))
        Right v -> do
          hPrint hout v
          hClose hout

cairoAssertion' :: EntryPoint -> Maybe (Path Abs File) -> Path Abs Dir -> Path Abs File -> Symbol -> Reg.Module -> (String -> IO ()) -> Assertion
cairoAssertion' entryPoint inputFile dirPath outputFile _ md step = do
  step "Translate to Cairo"
  case run . runError @JuvixError . runReader entryPoint $ regToCairo md of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right res -> do
      step "Serialize to Cairo bytecode"
      encodeFile (toFilePath outputFile) res
      step "Execute in Cairo VM"
      actualOutput <- Run.casmRunVM' dirPath outputFile inputFile
      writeFileEnsureLn outputFile actualOutput

regToCasmAssertion :: Path Abs Dir -> Path Abs File -> Maybe (Path Abs File) -> Path Abs File -> (String -> IO ()) -> Assertion
regToCasmAssertion root mainFile inputFile expectedFile step = do
  entryPoint <- testDefaultEntryPointIO root mainFile
  Reg.regRunAssertionParam (compileAssertion' entryPoint inputFile) mainFile expectedFile [] (const (return ())) step

regToCairoAssertion :: Path Abs Dir -> Path Abs File -> Maybe (Path Abs File) -> Path Abs File -> (String -> IO ()) -> Assertion
regToCairoAssertion root mainFile inputFile expectedFile step = do
  entryPoint <- testDefaultEntryPointIO root mainFile
  Reg.regRunAssertionParam (cairoAssertion' entryPoint inputFile) mainFile expectedFile [] (const (return ())) step
