module Commands.Dev.DevCompile.Reg where

import Commands.Base
import Commands.Dev.DevCompile.Reg.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Pretty

runCommand ::
  (Members AppEffects r) =>
  RegOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. regCompileCommonOptions . compileInputFile
      moutputFile = opts ^. regCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixReg inputFile moutputFile
  res :: Module <- runPipeline opts inputFile upToReg
  let txt = ppPrint res (computeCombinedInfoTable res)
  writeFileEnsureLn outFile txt
