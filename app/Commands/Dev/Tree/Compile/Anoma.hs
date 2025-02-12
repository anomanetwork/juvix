module Commands.Dev.Tree.Compile.Anoma where

import Commands.Base
import Commands.Compile.Anoma.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Nockma.Pretty qualified as Anoma
import Juvix.Compiler.Nockma.Translation.FromTree qualified as Anoma
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  AnomaOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  let opts' = opts ^. anomaCompileCommonOptions
      inputFile = Just (opts' ^. compileInputFile)
      moutputFile = opts' ^. compileOutputFile
  outFile <- getOutputFile FileExtNockma inputFile moutputFile
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
      $ treeToAnoma md
  writeFileEnsureLn outFile (Anoma.ppPrint (res ^. Anoma.anomaClosure))
