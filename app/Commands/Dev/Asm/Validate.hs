module Commands.Dev.Asm.Validate where

import Commands.Base
import Commands.Dev.Asm.Validate.Options
import Juvix.Compiler.Asm.Data.Module qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Transformation.Validate qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. (Members '[EmbedIO, App] r) => AsmValidateOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Asm.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right md -> do
      case Asm.validate' md of
        Just err ->
          exitJuvixError (JuvixError err)
        Nothing ->
          if
              | opts ^. asmValidateNoPrint ->
                  exitMsg ExitSuccess "validation succeeded"
              | otherwise -> do
                  renderStdOut (Asm.ppOutDefault md (Asm.computeCombinedInfoTable md))
                  exitMsg ExitSuccess ""
  where
    file :: AppPath File
    file = opts ^. asmValidateInputFile
