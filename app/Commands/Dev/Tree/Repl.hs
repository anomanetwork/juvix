module Commands.Dev.Tree.Repl where

import Commands.Base hiding (Atom)
import Commands.Dev.Tree.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Juvix.Compiler.Tree.Data.InfoTableBuilder qualified as Tree
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Pretty (ppPrint)
import Juvix.Compiler.Tree.Translation.FromSource (parseNodeText', parseText')
import System.Console.Haskeline
import System.Console.Repline qualified as Repline
import TreeEvaluator qualified as Eval

type ReplS = State.StateT ReplState IO

data ReplState = ReplState
  { _replStateBuilderState :: Tree.BuilderState,
    _replStateLoadedFile :: Maybe (Path Abs File)
  }

type Repl a = Repline.HaskelineT ReplS a

makeLenses ''ReplState

printHelpTxt :: Repl ()
printHelpTxt = liftIO $ putStrLn helpTxt
  where
    helpTxt :: Text =
      [__i|
  EXPRESSION                      Evaluate a JuvixTree expression
  :load FILE                      Load a file containing JuvixTree function and type definitions
  :reload                         Reload the current file
  :help                           Print help text and describe options
  :quit                           Exit the REPL
          |]

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

loadFile :: Path Abs File -> Repl ()
loadFile s = Repline.dontCrash $ do
  State.modify (set replStateLoadedFile (Just s))
  readProgram s

reloadFile :: Repl ()
reloadFile = Repline.dontCrash $ do
  fp <- State.gets (^. replStateLoadedFile)
  case fp of
    Nothing -> error "no file loaded"
    Just f -> readProgram f

readProgram :: Path Abs File -> Repl ()
readProgram f = do
  bs <- State.gets (^. replStateBuilderState)
  txt <- readFile f
  case parseText' bs txt of
    Left e -> error (show e)
    Right bs' ->
      State.modify (set replStateBuilderState bs')

options :: [(String, String -> Repl ())]
options =
  [ ("help", Repline.dontCrash . const printHelpTxt),
    ("quit", quit),
    ("load", loadFile . absFile),
    ("reload", const reloadFile)
  ]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> return "tree> "

readNode :: String -> Repl Node
readNode s = do
  bs <- State.gets (^. replStateBuilderState)
  case parseNodeText' bs replFile (strip (pack s)) of
    Left e -> error (show e)
    Right (bs', n) -> do
      State.modify (set replStateBuilderState bs')
      return n
  where
    replFile :: Path Abs File
    replFile = $(mkAbsFile "/<repl>")

evalNode :: Node -> Repl ()
evalNode node = do
  sym <- State.gets (^. replStateBuilderState . Tree.stateNextSymbolId)
  State.modify' (over (replStateBuilderState . Tree.stateNextSymbolId) (+ 1))
  md <- State.gets (^. replStateBuilderState . Tree.stateModule)
  let fi =
        FunctionInfo
          { _functionName = "repl:main",
            _functionLocation = Nothing,
            _functionSymbol = Symbol defaultModuleId sym,
            _functionArgsNum = 0,
            _functionCode = node,
            _functionExtra = (),
            _functionArgNames = [],
            _functionType = TyDynamic
          }
  et <- Eval.doEvalDefault md fi
  case et of
    Left e -> error (show e)
    Right v ->
      liftIO $
        putStrLn (ppPrint md v)

replCommand :: String -> Repl ()
replCommand input_ = Repline.dontCrash $ do
  readNode input_ >>= evalNode

replAction :: ReplS ()
replAction =
  Repline.evalReplOpts
    Repline.ReplOpts
      { prefix = Just ':',
        command = replCommand,
        initialiser = return (),
        finaliser = return Repline.Exit,
        multilineCommand = Just "multiline",
        tabComplete = Repline.Word (\_ -> return []),
        options,
        banner
      }

runCommand :: forall r. (Members '[EmbedIO, App] r) => TreeReplOptions -> Sem r ()
runCommand _ = liftIO . (`State.evalStateT` iniState) $ replAction
  where
    iniState :: ReplState
    iniState =
      ReplState
        { _replStateBuilderState = Tree.mkBuilderState (emptyModule defaultModuleId),
          _replStateLoadedFile = Nothing
        }
