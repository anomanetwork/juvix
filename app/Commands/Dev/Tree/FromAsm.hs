module Commands.Dev.Tree.FromAsm where

import Commands.Base
import Commands.Dev.Tree.FromAsm.Options
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Compiler.Tree.Error (TreeError)
import Juvix.Compiler.Tree.Pretty qualified as Tree
import Juvix.Compiler.Tree.Translation.FromAsm qualified as Tree

runCommand :: forall r. (Members '[EmbedIO, App] r) => TreeFromAsmOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Asm.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right md -> do
      r :: Either JuvixError Tree.Module <- runError $ mapError (JuvixError @TreeError) $ Tree.fromAsm md
      md' <- getRight r
      renderStdOut (Tree.ppOutDefault md' (Tree.computeCombinedInfoTable md'))
  where
    file :: AppPath File
    file = opts ^. treeFromAsmInputFile
