module Commands.Dev.DevCompile.Tree where

import Commands.Base
import Commands.Dev.DevCompile.Tree.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Pretty

runCommand ::
  (Members AppEffects r) =>
  TreeOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. treeCompileCommonOptions . compileInputFile
      moutputFile = opts ^. treeCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixTree inputFile moutputFile
  res :: Module <- runPipeline opts inputFile upToTree
  let txt = ppPrint res (computeCombinedInfoTable res)
  writeFileEnsureLn outFile txt
