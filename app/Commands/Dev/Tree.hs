module Commands.Dev.Tree where

import Commands.Base
import Commands.Dev.Tree.Compile as Compile
import Commands.Dev.Tree.Eval as Eval
import Commands.Dev.Tree.FromAsm as FromAsm
import Commands.Dev.Tree.Options
import Commands.Dev.Tree.Read as Read
import Commands.Dev.Tree.Repl as Repl

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => TreeCommand -> Sem r ()
runCommand = \case
  Eval opts -> Eval.runCommand opts
  Compile opts -> Compile.runCommand opts
  Read opts -> Read.runCommand opts
  FromAsm opts -> FromAsm.runCommand opts
  Repl opts -> Repl.runCommand opts
