module TreeEvaluator
  ( module TreeEvaluator,
    module Commands.Dev.Tree.Eval.Options,
  )
where

import App
import Commands.Dev.Tree.Eval.Options
import CommonOptions
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Compiler.Tree.Error qualified as Tree
import Juvix.Compiler.Tree.Evaluator qualified as Tree
import Juvix.Compiler.Tree.EvaluatorEff qualified as Eff
import Juvix.Compiler.Tree.Language.Value qualified as Tree
import Juvix.Compiler.Tree.Pretty qualified as Tree

evalTree :: forall r. (Members '[EmbedIO, App] r) => Evaluator -> Tree.Module -> Sem r ()
evalTree ev md =
  case md ^. Tree.moduleInfoTable . Tree.infoMainFunction of
    Just sym -> do
      r <- doEval ev md (Tree.lookupFunInfo md sym)
      case r of
        Left err ->
          exitJuvixError (JuvixError err)
        Right Tree.ValVoid ->
          return ()
        Right val -> do
          renderStdOut (Tree.ppOutDefault md val)
          putStrLn ""
    Nothing ->
      exitMsg (ExitFailure 1) "no 'main' function"

doEvalDefault ::
  (MonadIO m) =>
  Tree.Module ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalDefault = doEval defaultEvaluator

doEval ::
  (MonadIO m) =>
  Evaluator ->
  Tree.Module ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEval = \case
  EvalEffectful -> doEvalEff
  EvalRaw -> doEvalRaw

doEvalRaw ::
  (MonadIO m) =>
  Tree.Module ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalRaw md' = liftIO . Tree.catchEvalErrorIO . liftIO . Tree.hEvalIO stdin stdout md'

doEvalEff ::
  (MonadIO m) =>
  Tree.Module ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalEff md' funInfo = Eff.hEvalIOEither stdin stdout md' funInfo
