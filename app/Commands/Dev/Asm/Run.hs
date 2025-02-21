module Commands.Dev.Asm.Run where

import AsmInterpreter
import Commands.Base
import Commands.Dev.Asm.Run.Options
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. (Members '[EmbedIO, App] r) => AsmRunOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Asm.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right md -> runAsm (not (opts ^. asmRunNoValidate)) md
  where
    file :: AppPath File
    file = opts ^. asmRunInputFile
