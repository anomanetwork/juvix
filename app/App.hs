module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Internal.Translation (InternalTypedResult)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Pipeline.Run
import Juvix.Data.Error qualified as Error
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Paths.Base hiding (rootBuildDir)
import Juvix.Parser.Error
import System.Console.ANSI qualified as Ansi

data App :: Effect where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitFailMsg :: Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  FromAppFile :: AppPath File -> App m (Path Abs File)
  AskRoot :: App m Root
  AskArgs :: App m RunAppIOArgs
  AskInvokeDir :: App m (Path Abs Dir)
  AskPkgDir :: App m (Path Abs Dir)
  AskBuildDir :: App m (Path Abs Dir)
  AskPackage :: App m Package
  AskPackageGlobal :: App m Bool
  AskGlobalOptions :: App m GlobalOptions
  FromAppPathFile :: AppPath File -> App m (Path Abs File)
  GetMainAppFile :: Maybe (AppPath File) -> App m (AppPath File)
  GetMainAppFileMaybe :: Maybe (AppPath File) -> App m (Maybe (AppPath File))
  GetMainFile :: Maybe (AppPath File) -> App m (Path Abs File)
  GetMainFileMaybe :: Maybe (AppPath File) -> App m (Maybe (Path Abs File))
  FromAppPathDir :: AppPath Dir -> App m (Path Abs Dir)
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RenderStdOutRaw :: ByteString -> App m ()

data RunAppIOArgs = RunAppIOArgs
  { _runAppIOArgsGlobalOptions :: GlobalOptions,
    _runAppIOArgsRoot :: Root
  }

makeSem ''App
makeLenses ''RunAppIOArgs

type AppEffects = '[Logger, TaggedLock, Files, App, EmbedIO]

runAppIO ::
  forall r a.
  (Members '[EmbedIO, Logger, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args a = do
  entry <- getEntryPointPackage' args
  evalSingletonCache (runReader entry $ readPackageRootIO root)
    . reAppIO args
    $ a
  where
    root = args ^. runAppIOArgsRoot

reAppIO ::
  forall r a.
  (Members '[EmbedIO, TaggedLock, Logger] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem (SCache Package ': r) a
reAppIO args@RunAppIOArgs {..} =
  interpretTop $ \case
    AskPackageGlobal -> return (_runAppIOArgsRoot ^. rootPackageType `elem` [GlobalStdlib, GlobalPackageDescription, GlobalPackageBase])
    FromAppPathFile p -> prepathToAbsFile invDir (p ^. pathPath)
    FromAppFile m -> fromAppFile' m
    GetMainAppFile m -> getMainAppFile' m
    GetMainAppFileMaybe m -> getMainAppFileMaybe' m
    GetMainFile m -> getMainFile' m
    GetMainFileMaybe m -> getMainFileMaybe' m
    FromAppPathDir p -> liftIO (prepathToAbsDir invDir (p ^. pathPath))
    RenderStdOut t -> do
      sup <- liftIO (Ansi.hSupportsANSIColor stdout)
      renderIO (not (_runAppIOArgsGlobalOptions ^. globalNoColors) && sup) t
    RenderStdOutRaw b -> liftIO (ByteString.putStr b)
    AskGlobalOptions -> return _runAppIOArgsGlobalOptions
    AskPackage -> getPkg
    AskArgs -> return args
    AskRoot -> return _runAppIOArgsRoot
    AskInvokeDir -> return invDir
    AskPkgDir -> return (_runAppIOArgsRoot ^. rootRootDir)
    AskBuildDir -> return (resolveAbsBuildDir (_runAppIOArgsRoot ^. rootRootDir) (_runAppIOArgsRoot ^. rootBuildDir))
    PrintJuvixError e -> printErr e
    ExitJuvixError e -> do
      printErr e
      exitFailure
    ExitMsg exitCode t -> exitMsg' (exitWith exitCode) t
    ExitFailMsg t -> exitMsg' exitFailure t
  where
    getPkg :: (Members '[SCache Package] r') => Sem r' Package
    getPkg = cacheSingletonGet

    exitMsg' :: forall r' x. (Members '[EmbedIO, Logger] r') => IO x -> Text -> Sem r' x
    exitMsg' onExit t = do
      logError (mkAnsiText t)
      liftIO (hFlush stderr >> onExit)

    fromAppFile' :: (Members '[EmbedIO] r') => AppPath File -> Sem r' (Path Abs File)
    fromAppFile' f = prepathToAbsFile invDir (f ^. pathPath)

    getMainFile' :: (Members '[Logger, SCache Package, EmbedIO] r') => Maybe (AppPath File) -> Sem r' (Path Abs File)
    getMainFile' = getMainAppFile' >=> fromAppFile'

    getMainFileMaybe' :: (Members '[SCache Package, EmbedIO] r') => Maybe (AppPath File) -> Sem r' (Maybe (Path Abs File))
    getMainFileMaybe' = getMainAppFileMaybe' >=> mapM fromAppFile'

    getMainAppFileMaybe' :: (Members '[SCache Package, EmbedIO] r') => Maybe (AppPath File) -> Sem r' (Maybe (AppPath File))
    getMainAppFileMaybe' = \case
      Just p -> return (Just p)
      Nothing -> do
        pkg <- getPkg
        return $ case pkg ^. packageMain of
          Just p ->
            return
              AppPath
                { _pathPath = p,
                  _pathIsInput = True
                }
          Nothing -> Nothing

    getMainAppFile' :: (Members '[SCache Package, EmbedIO, Logger] r') => Maybe (AppPath File) -> Sem r' (AppPath File)
    getMainAppFile' = fromMaybeM missingMainErr . getMainAppFileMaybe'

    missingMainErr :: (Members '[EmbedIO, Logger] r') => Sem r' x
    missingMainErr =
      exitMsg'
        exitFailure
        ( "A path to the main file must be given in the CLI or specified in the `main` field of the "
            <> pack (toFilePath juvixYamlFile)
            <> " file"
        )

    invDir = _runAppIOArgsRoot ^. rootInvokeDir

    g :: GlobalOptions
    g = _runAppIOArgsGlobalOptions

    printErr :: forall r'. (Members '[Logger] r') => JuvixError -> Sem r' ()
    printErr e =
      logError
        . mkAnsiText
        . run
        . runReader (project' @GenericOptions g)
        $ Error.render renderType (g ^. globalIdeEndErrorChar) e
      where
        renderType :: Error.RenderType
        renderType
          | g ^. globalVSCode = Error.RenderVSCode
          | g ^. globalNoColors = Error.RenderText
          | otherwise = Error.RenderAnsi

getEntryPoint' ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  RunAppIOArgs ->
  Maybe (AppPath File) ->
  Sem r EntryPoint
getEntryPoint' RunAppIOArgs {..} inputFile = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  mainFile <- getMainAppFileMaybe inputFile
  mainFile' <- maybe (return Nothing) (fmap Just . fromAppFile) mainFile
  sha256 <- maybe (return Nothing) (fmap Just . runFilesIO . SHA256.digestFile) mainFile'
  set entryPointSHA256 sha256
    . set entryPointStdin estdin
    <$> entryPointFromGlobalOptionsPre root ((^. pathPath) <$> mainFile) opts

runPipelineEither ::
  (Members '[EmbedIO, TaggedLock, Logger, App] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r (Either JuvixError (ResolverState, PipelineResult a))
runPipelineEither opts input_ p = runPipelineOptions $ do
  args <- askArgs
  entry <- applyOptions opts <$> getEntryPoint' args input_
  runIOEither entry (inject p)

getEntryPointStdin' :: (Members '[EmbedIO, TaggedLock] r) => RunAppIOArgs -> Sem r EntryPoint
getEntryPointStdin' RunAppIOArgs {..} = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  set entryPointStdin estdin <$> entryPointFromGlobalOptionsNoFile root opts

getEntryPointPackage' :: (Members '[EmbedIO, TaggedLock] r) => RunAppIOArgs -> Sem r EntryPoint
getEntryPointPackage' RunAppIOArgs {..} = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  entryPointFromGlobalOptionsNoFile root opts

askPackageDotJuvixPath :: (Members '[App] r) => Sem r (Path Abs File)
askPackageDotJuvixPath = mkPackagePath . (^. rootRootDir) <$> askRoot

fromRightGenericError :: (Members '[App] r, ToGenericError err, Typeable err) => Either err a -> Sem r a
fromRightGenericError = fromRightJuvixError . mapLeft JuvixError

fromRightJuvixError :: (Members '[App] r) => Either JuvixError a -> Sem r a
fromRightJuvixError = getRight

someBaseToAbs' :: (Members '[App] r) => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

fromAppPathFileOrDir ::
  (Members '[EmbedIO, App] r) =>
  AppPath FileOrDir ->
  Sem r (Either (Path Abs File) (Path Abs Dir))
fromAppPathFileOrDir = filePathToAbs . (^. pathPath)

filePathToAbs :: (Members '[EmbedIO, App] r) => Prepath FileOrDir -> Sem r (Either (Path Abs File) (Path Abs Dir))
filePathToAbs fp = do
  invokeDir <- askInvokeDir
  fromPreFileOrDir invokeDir fp

askGenericOptions :: (Members '[App] r) => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: (Members '[EmbedIO, App, TaggedLock] r) => Maybe (AppPath File) -> Sem r EntryPoint
getEntryPoint inputFile = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPoint' RunAppIOArgs {..} inputFile

getEntryPointStdin :: (Members '[EmbedIO, App, TaggedLock] r) => Sem r EntryPoint
getEntryPointStdin = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPointStdin' RunAppIOArgs {..}

getEntryPointPackage :: (Members '[EmbedIO, App, TaggedLock] r) => Sem r EntryPoint
getEntryPointPackage = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPointPackage' RunAppIOArgs {..}

runPipelineTermination ::
  (Members '[EmbedIO, App, Logger, TaggedLock] r) =>
  Maybe (AppPath File) ->
  Sem (Termination ': PipelineEff r) a ->
  Sem r (PipelineResult a)
runPipelineTermination input_ p = silenceProgressLog $ do
  r <- runPipelineEither () input_ (evalTermination iniTerminationState (inject p)) >>= fromRightJuvixError
  return (snd r)

runPipelineNoOptions ::
  (Members '[App, EmbedIO, Logger, TaggedLock] r) =>
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipelineNoOptions = runPipeline ()

runPipelineLogger ::
  (Members '[App, EmbedIO, Logger, TaggedLock] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipelineLogger opts input_ p = do
  r <- runPipelineEither opts input_ (inject p) >>= fromRightJuvixError
  return (snd r ^. pipelineResult)

runPipeline ::
  (Members '[App, EmbedIO, Logger, TaggedLock] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipeline opts input_ =
  runPipelineLogger opts input_
    . inject

runPipelineRecursive ::
  forall r.
  (Members '[App, EmbedIO, Logger, TaggedLock] r) =>
  Maybe (AppPath File) ->
  Sem r (InternalTypedResult, [InternalTypedResult])
runPipelineRecursive input_ = do
  args <- askArgs
  entry <- getEntryPoint' args input_
  runReader defaultPipelineOptions (runPipelineHtmlEither entry) >>= fromRightJuvixError

runPipelineHtml ::
  (Members '[App, EmbedIO, Logger, TaggedLock] r) =>
  Bool ->
  Maybe (AppPath File) ->
  Sem r (InternalTypedResult, [InternalTypedResult])
runPipelineHtml bNonRecursive input_
  | bNonRecursive = do
      r <- runPipelineNoOptions input_ upToInternalTyped
      return (r, [])
  | otherwise = do
      args <- askArgs
      entry <- getEntryPoint' args input_
      runReader defaultPipelineOptions (runPipelineHtmlEither entry) >>= fromRightJuvixError

runPipelineOptions :: (Members '[App] r) => Sem (Reader PipelineOptions ': r) a -> Sem r a
runPipelineOptions m = do
  g <- askGlobalOptions
  let opt =
        defaultPipelineOptions
          { _pipelineNumThreads = g ^. globalNumThreads,
            _pipelineShowThreadId = g ^. globalDevShowThreadIds
          }
  runReader opt m

runPipelineEntry :: (Members '[App, Logger, EmbedIO, TaggedLock] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r a
runPipelineEntry entry p = runPipelineOptions $ do
  r <- runIOEither entry (inject p) >>= fromRightJuvixError
  return (snd r ^. pipelineResult)

runPipelineSetup ::
  (Members '[App, EmbedIO, Logger, Reader PipelineOptions, TaggedLock] r) =>
  Sem (PipelineEff' r) a ->
  Sem r a
runPipelineSetup p = do
  args <- askArgs
  entry <- getEntryPointStdin' args
  r <- runIOEitherPipeline entry (inject p) >>= fromRightJuvixError
  return (snd r)

renderStdOutLn :: forall a r. (Member App r, HasAnsiBackend a, HasTextBackend a) => a -> Sem r ()
renderStdOutLn txt = renderStdOut txt >> newline

newline :: (Member App r) => Sem r ()
newline = renderStdOut @Text "\n"

printSuccessExit :: (Member App r) => Text -> Sem r a
printSuccessExit = exitMsg ExitSuccess

getRight :: forall e a r. (Members '[App] r, AppError e) => Either e a -> Sem r a
getRight = either appError return

runAppError :: forall e r a. (AppError e, Members '[App] r) => Sem (Error e ': r) a -> Sem r a
runAppError = runErrorNoCallStackWith appError

instance AppError MarkdownBackendError where
  appError = appError . JuvixError

instance AppError ParserError where
  appError = appError . JuvixError

instance AppError MegaparsecError where
  appError = appError . JuvixError

instance AppError Text where
  appError = exitFailMsg

instance AppError JuvixError where
  appError = exitJuvixError

instance AppError SimpleError where
  appError = exitFailMsg . toPlainText

class AppError e where
  appError :: (Members '[App] r) => e -> Sem r a
