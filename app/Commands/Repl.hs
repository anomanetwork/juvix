{-# LANGUAGE QuasiQuotes #-}

module Commands.Repl where

import Commands.Base hiding (command)
import Commands.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Data.String.Interpolate (i, __i)
import Evaluator
import Juvix.Compiler.Concrete.Data.Scope (scopePath)
import Juvix.Compiler.Concrete.Data.ScopedName (absTopModulePath)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames)
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib
import Juvix.Extra.Version
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline

type ReplS = State.StateT ReplState IO

type Repl a = HaskelineT ReplS a

data ReplContext = ReplContext
  { _replContextArtifacts :: Artifacts,
    _replContextEntryPoint :: EntryPoint
  }

data ReplState = ReplState
  { _replStateRoots :: Roots,
    _replStateContext :: Maybe ReplContext,
    _replStateGlobalOptions :: GlobalOptions
  }

makeLenses ''ReplState
makeLenses ''ReplContext

helpTxt :: MonadIO m => m ()
helpTxt =
  liftIO
    ( putStrLn
        [__i|
  EXPRESSION                Evaluate an expression in the context of the currently loaded module
  :help                     Print help text and describe options
  :load       FILE          Load a file into the REPL
  :reload                   Reload the currently loaded file
  :type       EXPRESSION    Infer the type of an expression
  :core       EXPRESSION    Translate the expression to JuvixCore
  :multiline                Start a multi-line input. Submit with <Ctrl-D>
  :root                     Print the current project root
  :version                  Display the Juvix version
  :quit                     Exit the REPL
  |]
    )

noFileLoadedMsg :: (MonadIO m) => m ()
noFileLoadedMsg = liftIO (putStrLn "No file loaded. Load a file using the `:load FILE` command.")

welcomeMsg :: (MonadIO m) => m ()
welcomeMsg = liftIO (putStrLn [i|Juvix REPL version #{versionTag}: https://juvix.org. Run :help for help|])

runCommand :: Members '[Embed IO, App] r => ReplOptions -> Sem r ()
runCommand opts = do
  root <- askPkgDir
  buildDir <- askBuildDir
  package <- askPackage
  global <- askPackageGlobal
  let getReplEntryPoint :: SomeBase File -> Repl EntryPoint
      getReplEntryPoint inputFile = do
        gopts <- State.gets (^. replStateGlobalOptions)
        absInputFile :: Path Abs File <- replMakeAbsolute inputFile
        return $
          (entryPointFromGlobalOptions (package, global) root absInputFile gopts)
            { _entryPointBuildDir = Abs buildDir
            }

      printHelpTxt :: String -> Repl ()
      printHelpTxt _ = helpTxt

      multilineCmd :: String
      multilineCmd = "multiline"

      quit :: String -> Repl ()
      quit _ = liftIO (throwIO Interrupt)

      loadEntryPoint :: EntryPoint -> Repl ()
      loadEntryPoint ep = do
        artif <- liftIO (corePipelineIO' ep)
        State.modify
          ( set
              replStateContext
              ( Just
                  ( ReplContext
                      { _replContextArtifacts = artif,
                        _replContextEntryPoint = ep
                      }
                  )
              )
          )
        let epPath :: Path Abs File = ep ^. entryPointModulePaths . _head1
        liftIO (putStrLn [i|OK loaded: #{toFilePath epPath}|])

      reloadFile :: String -> Repl ()
      reloadFile _ = do
        mentryPoint <- State.gets (fmap (^. replContextEntryPoint) . (^. replStateContext))
        case mentryPoint of
          Just entryPoint -> do
            loadEntryPoint entryPoint
          Nothing -> noFileLoadedMsg

      pSomeFile :: String -> SomeBase File
      pSomeFile = someFile . unpack . strip . pack

      loadFile :: SomeBase File -> Repl ()
      loadFile f = do
        entryPoint <- getReplEntryPoint f
        loadEntryPoint entryPoint

      loadPrelude :: Repl ()
      loadPrelude = loadDefaultPrelude

      loadDefaultPrelude :: Repl ()
      loadDefaultPrelude = whenJustM defaultPreludeEntryPoint loadEntryPoint

      printRoot :: String -> Repl ()
      printRoot _ = do
        r <- State.gets (^. replStateRoots . rootsRootDir)
        liftIO $ putStrLn (pack (toFilePath r))

      displayVersion :: String -> Repl ()
      displayVersion _ = liftIO (putStrLn versionTag)

      command :: String -> Repl ()
      command input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just ctx' -> do
            let tab = ctx' ^. replContextArtifacts . artifactCoreTable
            evalRes <- compileThenEval ctx' input
            case evalRes of
              Left err -> printError err
              Right n
                | Info.member Info.kNoDisplayInfo (Core.getInfo n) -> return ()
              Right n
                | opts ^. replPrintValues ->
                    renderOut (Core.ppOut opts (toValue tab n))
                | otherwise ->
                    renderOut (Core.ppOut opts n)
          Nothing -> noFileLoadedMsg
        where
          defaultLoc :: Interval
          defaultLoc = singletonInterval (mkInitialLoc replPath)

          compileThenEval :: ReplContext -> String -> Repl (Either JuvixError Core.Node)
          compileThenEval ctx s = bindEither compileString eval
            where
              artif :: Artifacts
              artif = ctx ^. replContextArtifacts

              shouldDisambiguate :: Bool
              shouldDisambiguate = not (opts ^. replNoDisambiguate)

              eval :: Core.Node -> Repl (Either JuvixError Core.Node)
              eval n = do
                ep <- getReplEntryPoint (Abs replPath)
                case run
                  . runReader ep
                  . runError @JuvixError
                  . runState artif
                  . runTransformations shouldDisambiguate (opts ^. replTransformations)
                  $ n of
                  Left err -> return $ Left err
                  Right (artif', n') -> liftIO (doEvalIO' artif' n')

              doEvalIO' :: Artifacts -> Core.Node -> IO (Either JuvixError Core.Node)
              doEvalIO' artif' n =
                mapLeft (JuvixError @Core.CoreError)
                  <$> doEvalIO False defaultLoc (artif' ^. artifactCoreTable) n

              compileString :: Repl (Either JuvixError Core.Node)
              compileString = liftIO $ compileExpressionIO' ctx (strip (pack s))

              bindEither :: (Monad m) => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
              bindEither x f = join <$> (x >>= mapM f)

      core :: String -> Repl ()
      core input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just ctx' -> do
            compileRes <- liftIO (compileExpressionIO' ctx' (strip (pack input)))
            case compileRes of
              Left err -> printError err
              Right n -> renderOut (Core.ppOut opts n)
          Nothing -> noFileLoadedMsg

      inferType :: String -> Repl ()
      inferType input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        gopts <- State.gets (^. replStateGlobalOptions)
        case ctx of
          Just ctx' -> do
            compileRes <- liftIO (inferExpressionIO' ctx' (strip (pack input)))
            case compileRes of
              Left err -> printError err
              Right n -> renderOut (Internal.ppOut (project' @GenericOptions gopts) n)
          Nothing -> noFileLoadedMsg

      options :: [(String, String -> Repl ())]
      options =
        [ ("help", Repline.dontCrash . printHelpTxt),
          -- `multiline` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          (multilineCmd, Repline.dontCrash . \_ -> return ()),
          ("quit", quit),
          ("load", Repline.dontCrash . loadFile . pSomeFile),
          ("reload", Repline.dontCrash . reloadFile),
          ("root", printRoot),
          ("type", inferType),
          ("version", displayVersion),
          ("core", core)
        ]

      defaultMatcher :: [(String, CompletionFunc ReplS)]
      defaultMatcher = [(":load", fileCompleter)]

      optsCompleter :: WordCompleter ReplS
      optsCompleter n = do
        let names = (":" <>) . fst <$> options
        return (filter (isPrefixOf n) names)

      banner :: MultiLine -> Repl String
      banner = \case
        MultiLine -> return "... "
        SingleLine -> do
          mmodulePath <-
            State.gets
              ( ^?
                  replStateContext
                    . _Just
                    . replContextArtifacts
                    . artifactMainModuleScope
                    . _Just
                    . scopePath
                    . absTopModulePath
              )
          case mmodulePath of
            Just path -> return [i|#{unpack (P.prettyText path)}> |]
            Nothing -> return "juvix> "

      prefix :: Maybe Char
      prefix = Just ':'

      multilineCommand :: Maybe String
      multilineCommand = Just multilineCmd

      initialiser :: Repl ()
      initialiser = do
        gopts <- State.gets (^. replStateGlobalOptions)
        welcomeMsg
        unless
          (opts ^. replNoPrelude || gopts ^. globalNoStdlib)
          (maybe loadPrelude (loadFile . (^. pathPath)) (opts ^. replInputFile))

      finaliser :: Repl ExitDecision
      finaliser = return Exit

      tabComplete :: CompleterStyle ReplS
      tabComplete = Prefix (wordCompleter optsCompleter) defaultMatcher

      replAction :: ReplS ()
      replAction = do
        evalReplOpts
          ReplOpts
            { prefix,
              multilineCommand,
              initialiser,
              finaliser,
              tabComplete,
              command,
              options,
              banner
            }
  roots <- askRoots
  globalOptions <- askGlobalOptions
  embed
    ( State.evalStateT
        replAction
        ( ReplState
            { _replStateRoots = roots,
              _replStateContext = Nothing,
              _replStateGlobalOptions = globalOptions
            }
        )
    )

-- | If the package contains the stdlib as a dependency, loads the Prelude
defaultPreludeEntryPoint :: Repl (Maybe EntryPoint)
defaultPreludeEntryPoint = do
  opts <- State.gets (^. replStateGlobalOptions)
  roots <- State.gets (^. replStateRoots)
  let buildDir = roots ^. rootsBuildDir
      root = roots ^. rootsRootDir
      pkg = roots ^. rootsPackage
      pkgGlobal = roots ^. rootsPackageGlobal
      mstdlibPath = firstJust (isStdLib root buildDir) (pkg ^. packageDependencies)
  return $ case mstdlibPath of
    Just stdlibPath ->
      Just
        (entryPointFromGlobalOptions (pkg, pkgGlobal) root (stdlibPath <//> preludePath) opts)
          { _entryPointResolverRoot = stdlibPath
          }
    Nothing -> Nothing

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateRoots . rootsInvokeDir)
    return (invokeDir <//> r)

inferExpressionIO' :: ReplContext -> Text -> IO (Either JuvixError Internal.Expression)
inferExpressionIO' ctx txt =
  runM
    . evalState (ctx ^. replContextArtifacts)
    $ inferExpressionIO replPath txt

compileExpressionIO' :: ReplContext -> Text -> IO (Either JuvixError Core.Node)
compileExpressionIO' ctx txt =
  runM
    . evalState (ctx ^. replContextArtifacts)
    $ compileExpressionIO replPath txt

render' :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
render' t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  liftIO (P.renderIO (not (opts ^. globalNoColors) && hasAnsi) t)

renderOut :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOut t = render' t >> liftIO (putStrLn "")

printError :: JuvixError -> Repl ()
printError e = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stderr)
  liftIO $ hPutStrLn stderr $ run (runReader (project' @GenericOptions opts) (Error.render (not (opts ^. globalNoColors) && hasAnsi) False e))

runTransformations ::
  forall r.
  Members '[State Artifacts, Error JuvixError, Reader EntryPoint] r =>
  Bool ->
  [Core.TransformationId] ->
  Core.Node ->
  Sem r Core.Node
runTransformations shouldDisambiguate ts n = runCoreInfoTableBuilderArtifacts $ do
  sym <- addNode n
  applyTransforms shouldDisambiguate ts
  getNode sym
  where
    addNode :: Core.Node -> Sem (Core.InfoTableBuilder ': r) Core.Symbol
    addNode node = do
      sym <- Core.freshSymbol
      Core.registerIdentNode sym node
      -- `n` will get filtered out by the transformations unless it has a
      -- corresponding entry in `infoIdentifiers`
      tab <- Core.getInfoTable
      let name = Core.freshIdentName tab "_repl"
          idenInfo =
            Core.IdentifierInfo
              { _identifierName = name,
                _identifierSymbol = sym,
                _identifierLocation = Nothing,
                _identifierArgsNum = 0,
                _identifierType = Core.mkDynamic',
                _identifierIsExported = False,
                _identifierBuiltin = Nothing
              }
      Core.registerIdent name idenInfo
      return sym

    applyTransforms :: Bool -> [Core.TransformationId] -> Sem (Core.InfoTableBuilder ': r) ()
    applyTransforms shouldDisambiguate' ts' = do
      tab <- Core.getInfoTable
      tab' <- mapReader Core.fromEntryPoint $ Core.applyTransformations ts' tab
      let tab'' =
            if
                | shouldDisambiguate' -> disambiguateNames tab'
                | otherwise -> tab'
      Core.setInfoTable tab''

    getNode :: Core.Symbol -> Sem (Core.InfoTableBuilder ': r) Core.Node
    getNode sym = fromMaybe impossible . flip Core.lookupIdentifierNode' sym <$> Core.getInfoTable
