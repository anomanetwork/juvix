module Commands.Dev.Reg.Run where

import Commands.Base
import Commands.Dev.Reg.Run.Options
import Juvix.Compiler.Reg.Translation.FromSource qualified as Reg
import RegInterpreter

runCommand :: forall r. (Members '[EmbedIO, App] r) => RegRunOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Reg.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right md -> runReg md
  where
    file :: AppPath File
    file = opts ^. regRunInputFile
