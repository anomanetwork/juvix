module Asm.Run.Base where

import Base
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Interpreter
import Juvix.Compiler.Asm.Pretty
import Juvix.Compiler.Asm.Transformation.Validate
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Data.PPOutput

runAssertion :: Handle -> Symbol -> Module -> IO ()
runAssertion hout sym tab = do
  r' <- doRun hout tab (lookupFunInfo tab sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (prettyString err)
    Right value' -> do
      case value' of
        ValVoid -> return ()
        _ -> hPutStrLn hout (ppPrint tab value')

asmRunAssertion' :: Module -> Path Abs File -> (String -> IO ()) -> Assertion
asmRunAssertion' = asmRunAssertionParam' runAssertion

asmRunAssertionParam' :: (Handle -> Symbol -> Module -> IO ()) -> Module -> Path Abs File -> (String -> IO ()) -> Assertion
asmRunAssertionParam' interpretFun md expectedFile step = do
  step "Validate"
  case validate' md of
    Just err -> assertFailure (prettyString err)
    Nothing ->
      case md ^. moduleInfoTable . infoMainFunction of
        Just sym -> do
          withTempDir'
            ( \dirPath -> do
                let outputFile = dirPath <//> $(mkRelFile "out.out")
                hout <- openFile (toFilePath outputFile) WriteMode
                step "Interpret"
                interpretFun hout sym md
                hClose hout
                actualOutput <- readFile outputFile
                step "Compare expected and actual program output"
                expected <- readFile expectedFile
                assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
            )
        Nothing -> assertFailure "no 'main' function"

asmRunAssertion :: Path Abs File -> Path Abs File -> (Module -> Either AsmError Module) -> (Module -> Assertion) -> (String -> IO ()) -> Assertion
asmRunAssertion = asmRunAssertionParam runAssertion

asmRunAssertionParam :: (Handle -> Symbol -> Module -> IO ()) -> Path Abs File -> Path Abs File -> (Module -> Either AsmError Module) -> (Module -> Assertion) -> (String -> IO ()) -> Assertion
asmRunAssertionParam interpretFun mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right md -> do
      case trans md of
        Left err -> assertFailure (prettyString err)
        Right md' -> do
          testTrans md'
          asmRunAssertionParam' interpretFun md' expectedFile step

asmRunErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
asmRunErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right md -> do
      step "Validate"
      case validate' md of
        Just _ -> assertBool "" True
        Nothing ->
          case md ^. moduleInfoTable . infoMainFunction of
            Just sym -> do
              withTempDir'
                ( \dirPath -> do
                    let outputFile = dirPath <//> $(mkRelFile "out.out")
                    hout <- openFile (toFilePath outputFile) WriteMode
                    step "Interpret"
                    r' <- doRun hout md (lookupFunInfo md sym)
                    hClose hout
                    case r' of
                      Left _ -> assertBool "" True
                      Right _ -> assertFailure "no error"
                )
            Nothing -> assertBool "" True

parseFile :: Path Abs File -> IO (Either MegaparsecError Module)
parseFile f = do
  s <- readFile f
  return (runParser f s)

doRun ::
  Handle ->
  Module ->
  FunctionInfo ->
  IO (Either AsmError Val)
doRun hout md funInfo = catchRunErrorIO (hRunCodeIO stdin hout md funInfo)
