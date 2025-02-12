module Commands.Dev.Core.Strip where

import Commands.Base
import Commands.Dev.Core.Strip.Options
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[EmbedIO, TaggedLock, App] r, CanonicalProjection a Core.Options, CanonicalProjection a CoreStripOptions) => a -> Sem r ()
runCommand opts = do
  root <- askRoot
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- fromAppPathFile sinputFile
  ep <- entryPointFromGlobalOptions root (Just inputFile) gopts
  s' <- readFile inputFile
  (tab, _) <- getRight (Core.runParser inputFile defaultModuleId mempty s')
  let r =
        run
          . runReader ep
          . runError @JuvixError
          $ Core.toStripped Core.IdentityTrans (Core.moduleFromInfoTable tab)
  tab' <-
    getRight $
      mapRight (Stripped.fromCore' . Core.computeCombinedInfoTable) r
  unless (project opts ^. coreStripNoPrint) $ do
    renderStdOut (Core.ppOut opts tab')
  where
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreStripInputFile
