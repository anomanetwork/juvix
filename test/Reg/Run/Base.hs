module Reg.Run.Base where

import Base
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Error
import Juvix.Compiler.Reg.Interpreter
import Juvix.Compiler.Reg.Pretty
import Juvix.Compiler.Reg.Transformation as Reg
import Juvix.Compiler.Reg.Translation.FromSource
import Juvix.Data.PPOutput

runAssertion :: Path Abs Dir -> Path Abs File -> Symbol -> Module -> (String -> IO ()) -> Assertion
runAssertion _ outputFile sym md step = do
  hout <- openFile (toFilePath outputFile) WriteMode
  step "Interpret"
  r' <- doRun hout md (lookupFunInfo md sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (prettyString err)
    Right value' -> do
      case value' of
        ValVoid ->
          hClose hout
        _ -> do
          hPutStrLn hout (ppPrint md value')
          hClose hout

regRunAssertion' :: Module -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertion' = regRunAssertionParam' runAssertion

regRunAssertionParam' :: (Path Abs Dir -> Path Abs File -> Symbol -> Module -> (String -> IO ()) -> Assertion) -> Module -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertionParam' interpretFun md expectedFile step = do
  case md ^. moduleInfoTable . infoMainFunction of
    Just sym -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            interpretFun dirPath outputFile sym md step
            actualOutput <- readFile outputFile
            step "Compare expected and actual program output"
            expected <- readFile expectedFile
            assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
        )
    Nothing -> assertFailure "no 'main' function"

regRunAssertion :: Path Abs File -> Path Abs File -> [TransformationId] -> (Module -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertion = regRunAssertionParam runAssertion

regRunAssertionParam :: (Path Abs Dir -> Path Abs File -> Symbol -> Module -> (String -> IO ()) -> Assertion) -> Path Abs File -> Path Abs File -> [TransformationId] -> (Module -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertionParam interpretFun mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right md -> do
      unless (null trans) $
        step "Transform"
      case run $ runError @JuvixError $ runReader Reg.defaultOptions $ applyTransformations trans md of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right md' -> do
          testTrans md'
          regRunAssertionParam' interpretFun md' expectedFile step

regRunErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
regRunErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right md ->
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
  IO (Either RegError Val)
doRun hout md funInfo =
  runM
    . runError
    $ runFunctionIO stdin hout md [] funInfo
