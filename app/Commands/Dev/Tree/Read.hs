module Commands.Dev.Tree.Read where

import Commands.Base
import Commands.Dev.Tree.Read.Options
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Compiler.Tree.Options qualified as TreeOptions
import Juvix.Compiler.Tree.Pretty qualified as Tree
import Juvix.Compiler.Tree.Transformation qualified as Tree
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree
import TreeEvaluator qualified as Eval

runCommand :: forall r. (Members '[EmbedIO, App] r) => TreeReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Tree.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right md -> do
      r <- runReader TreeOptions.defaultOptions $ runError @JuvixError (Tree.applyTransformations (project opts ^. treeReadTransformations) md)
      case r of
        Left err -> exitJuvixError (JuvixError err)
        Right md' -> do
          unless (project opts ^. treeReadNoPrint) $
            renderStdOut (Tree.ppOutDefault md' (Tree.computeCombinedInfoTable md'))
          doEval md'
  where
    file :: AppPath File
    file = opts ^. treeReadInputFile

    doEval :: Tree.Module -> Sem r ()
    doEval md'
      | project opts ^. treeReadEval = do
          putStrLn "--------------------------------"
          putStrLn "|            Eval              |"
          putStrLn "--------------------------------"
          Eval.evalTree Eval.defaultEvaluator md'
      | otherwise = return ()
