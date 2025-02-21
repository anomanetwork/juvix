module Commands.Dev.Reg.Read where

import Commands.Base
import Commands.Dev.Reg.Read.Options
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg hiding (defaultOptions)
import Juvix.Compiler.Reg.Transformation qualified as Reg
import Juvix.Compiler.Reg.Translation.FromSource qualified as Reg
import RegInterpreter

runCommand :: forall r. (Members '[EmbedIO, App] r) => RegReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Reg.runParser afile s of
    Left err ->
      exitJuvixError (JuvixError err)
    Right md -> do
      r <-
        runError @JuvixError
          . runReader Reg.defaultOptions
          $ (Reg.applyTransformations (project opts ^. regReadTransformations) md)
      case r of
        Left err -> exitJuvixError (JuvixError err)
        Right md' -> do
          unless (project opts ^. regReadNoPrint) $
            renderStdOut (Reg.ppOutDefault md' (Reg.computeCombinedInfoTable md'))
          doRun md'
  where
    file :: AppPath File
    file = opts ^. regReadInputFile

    doRun :: Reg.Module -> Sem r ()
    doRun md'
      | project opts ^. regReadRun = do
          putStrLn "--------------------------------"
          putStrLn "|            Run               |"
          putStrLn "--------------------------------"
          runReg md'
      | otherwise = return ()
