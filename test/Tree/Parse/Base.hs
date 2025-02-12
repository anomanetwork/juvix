module Tree.Parse.Base where

import Base
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Pretty
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeParseAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
treeParseAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right md -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            step "Print"
            writeFileEnsureLn outputFile (ppPrint md (computeCombinedInfoTable md))
            step "Parse printed"
            r' <- parseFile outputFile
            case r' of
              Left err -> assertFailure (prettyString err)
              Right md' -> do
                assertBool
                  ("Check: print . parse = print . parse . print . parse")
                  (ppPrint md (computeCombinedInfoTable md) == ppPrint md' (computeCombinedInfoTable md'))
        )

parseFile :: Path Abs File -> IO (Either MegaparsecError Module)
parseFile f = do
  s <- readFile f
  return (runParser f s)
