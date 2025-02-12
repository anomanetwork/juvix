module Tree.Eval.Base where

import Base
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Data.TransformationId
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Evaluator
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Options qualified as Tree
import Juvix.Compiler.Tree.Pretty
import Juvix.Compiler.Tree.Transformation
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (Module -> Assertion) ->
  (String -> IO ()) ->
  Assertion
treeEvalAssertion = treeEvalAssertionParam evalAssertion

treeEvalAssertionParam ::
  (Handle -> Symbol -> Module -> IO ()) ->
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (Module -> Assertion) ->
  (String -> IO ()) ->
  Assertion
treeEvalAssertionParam evalParam mainFile expectedFile trans testTrans step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (prettyString err)
    Right md0 -> do
      step "Validate"
      let opts = Tree.defaultOptions
      case run $ runReader opts $ runError @JuvixError $ applyTransformations [Validate] md0 of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right md1 -> do
          unless (null trans) $
            step "Transform"
          case run $ runReader opts $ runError @JuvixError $ applyTransformations trans md1 of
            Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
            Right md -> do
              testTrans md
              case md ^. moduleInfoTable . infoMainFunction of
                Just sym -> do
                  withTempDir'
                    ( \dirPath -> do
                        let outputFile = dirPath <//> $(mkRelFile "out.out")
                        hout <- openFile (toFilePath outputFile) WriteMode
                        step "Evaluate"
                        evalParam hout sym md
                        hClose hout
                        actualOutput <- readFile outputFile
                        step "Compare expected and actual program output"
                        expected <- readFile expectedFile
                        assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
                    )
                Nothing -> assertFailure "no 'main' function"

evalAssertion :: Handle -> Symbol -> Module -> IO ()
evalAssertion hout sym md = do
  r' <- doEval hout md (lookupFunInfo md sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (prettyString err)
    Right value' -> do
      case value' of
        ValVoid -> return ()
        _ -> hPutStrLn hout (ppPrint md value')

doEval ::
  Handle ->
  Module ->
  FunctionInfo ->
  IO (Either TreeError Value)
doEval hout md funInfo = catchEvalErrorIO (hEvalIO stdin hout md funInfo)

treeEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
treeEvalErrorAssertion mainFile step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (prettyString err)
    Right md ->
      case md ^. moduleInfoTable . infoMainFunction of
        Just sym -> do
          withTempDir'
            ( \dirPath -> do
                let outputFile = dirPath <//> $(mkRelFile "out.out")
                hout <- openFile (toFilePath outputFile) WriteMode
                step "Evaluate"
                r' <- doEval hout md (lookupFunInfo md sym)
                hClose hout
                case r' of
                  Left _ -> assertBool "" True
                  Right _ -> assertFailure "no error"
            )
        Nothing -> assertFailure "no main function"
