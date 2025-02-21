module Commands.Dev.DevCompile.Tree where

import Commands.Base
import Commands.Dev.DevCompile.Tree.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Pipeline.Modular (modularCoreToTree)
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Pretty
import Juvix.Compiler.Tree.Transformation.FilterUnreachable

runCommand ::
  (Members AppEffects r) =>
  TreeOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. treeCompileCommonOptions . compileInputFile
      moutputFile = opts ^. treeCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixTree inputFile moutputFile
  (mid, mtab) <- runPipelineModular opts inputFile Nothing modularCoreToTree
  let md = filterUnreachable (combineInfoTables (lookupModuleTable mtab mid))
      txt = ppPrint md (md ^. moduleInfoTable)
  writeFileEnsureLn outFile txt
