module Commands.Dev.Tree.Compile.Casm where

import Commands.Base
import Commands.Dev.DevCompile.Casm.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  CasmOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  let opts' = opts ^. casmCompileCommonOptions
      inputFile = Just (opts' ^. compileInputFile)
      moutputFile = opts' ^. compileOutputFile
  outFile <- getOutputFile FileExtCasm inputFile moutputFile
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
      $ treeToCasm md
  writeFileEnsureLn outFile (Casm.ppPrint res)
