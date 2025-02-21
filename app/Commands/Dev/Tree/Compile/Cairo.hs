module Commands.Dev.Tree.Compile.Cairo where

import Commands.Base
import Commands.Compile.Cairo.Options
import Commands.Extra.NewCompile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => CairoOptions ('InputExtension 'FileExtJuvixTree) -> Sem r ()
runCommand opts = do
  let inputFile = Just (opts ^. cairoCompileCommonOptions . compileInputFile)
      moutputFile = opts ^. cairoCompileCommonOptions . compileOutputFile
  outFile <- getOutputFile FileExtCasm inputFile moutputFile
  mainFile <- getMainFile inputFile
  md :: Module <- readFile mainFile >>= getRight . Tree.runParser mainFile
  entrypoint <-
    applyOptions opts
      <$> getEntryPoint inputFile
  res <-
    getRight
      . run
      . runReader entrypoint
      . runError @JuvixError
      $ treeToCairo md
  liftIO (JSON.encodeFile (toFilePath outFile) res)
