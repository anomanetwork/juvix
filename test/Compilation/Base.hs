module Compilation.Base where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Pipeline.Modular (modularCoreToTree)
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Prelude.Pretty
import Tree.Compile.Base
import Tree.Eval.Base

data CompileAssertionMode
  = EvalOnly
  | -- | Specify text to be sent to stdin of the process under test
    CompileOnly Text
  | EvalAndCompile

compileAssertion ::
  Path Abs Dir ->
  Int ->
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion root' optLevel =
  compileAssertionEntry
    ( set entryPointTarget (Just TargetCNative64)
        . set entryPointPipeline (Just PipelineExec)
        . set entryPointOptimizationLevel optLevel
    )
    root'
    optLevel

compileAssertionEntry ::
  (EntryPoint -> EntryPoint) ->
  Path Abs Dir ->
  Int ->
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertionEntry adjustEntry root' optLevel mode mainFile expectedFile step = do
  step "Translate to JuvixTree"
  entryPoint <- adjustEntry <$> testDefaultEntryPointIO root' mainFile
  r <- testRunIOModular (Just Core.CheckExec) entryPoint modularCoreToTree
  case r of
    Left e -> do
      assertFailure (prettyString (fromJuvixError @GenericError e))
    Right (mid, mtab) -> do
      let md = fromJust $ HashMap.lookup mid (mtab ^. Core.moduleTable)
          tab' = Tree.computeCombinedInfoTable md
          evalAssertion = treeEvalAssertion' (Tree.moduleFromInfoTable tab') expectedFile step
          compileAssertion' stdinText = treeCompileAssertion' entryPoint optLevel tab' mainFile expectedFile stdinText step
      case mode of
        EvalOnly -> evalAssertion
        CompileOnly stdinText -> compileAssertion' stdinText
        EvalAndCompile -> evalAssertion >> compileAssertion' ""

compileErrorAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileErrorAssertion root' mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  res <- fmap snd <$> testRunIOEither entryPoint upToCore
  let noError :: Assertion = assertFailure "No error found"
  case res of
    Left {} -> return ()
    Right core -> do
      let res' =
            run
              . runError @JuvixError
              . runReader entryPoint
              $ Core.toStored (core ^. pipelineResult . Core.coreResultModule)
                >>= Core.toStripped Core.CheckExec
      case res' of
        Left {} -> return ()
        Right {} -> noError
