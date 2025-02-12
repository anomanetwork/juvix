module Commands.Dev.Tree.Compile.RiscZeroRust where

import Commands.Base
import Commands.Compile.RiscZeroRust.Options
import Commands.Compile.RiscZeroRust.Rust
import Juvix.Compiler.Backend.Rust.Data.Result
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  RiscZeroRustOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  let inputFile = Just $ opts ^. riscZeroRustCompileCommonOptions . compileInputFile
  mainFile <- getMainFile inputFile
  md :: Module <- readFile mainFile >>= getRight . Tree.runParser mainFile
  entrypoint <-
    applyOptions opts
      <$> getEntryPoint inputFile
  Result {..} <-
    getRight
      . run
      . runError @JuvixError
      . runReader entrypoint
      $ treeToRiscZeroRust md
  compileRustCode opts inputFile _resultRustCode
