module Commands.Dev.Tree.Compile.Reg where

import Commands.Base
import Commands.Dev.DevCompile.Reg.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  RegOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  let opts' = opts ^. regCompileCommonOptions
      inputFile = Just (opts' ^. compileInputFile)
      moutputFile = opts' ^. compileOutputFile
  outFile <- getOutputFile FileExtJuvixReg inputFile moutputFile
  mainFile <- getMainFile inputFile
  md :: Module <- readFile mainFile >>= getRight . Tree.runParser mainFile
  entrypoint <-
    applyOptions opts
      <$> getEntryPoint inputFile
  res <-
    getRight
      . run
      . runError @JuvixError
      . runReader entrypoint
      $ treeToReg md
  writeFileEnsureLn outFile (Reg.ppPrint res (Reg.computeCombinedInfoTable res))
