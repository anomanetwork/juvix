module Asm.Compile.Base where

import Base
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Pipeline qualified as Pipeline
import Runtime.Base qualified as Runtime

asmCompileAssertion' :: EntryPoint -> Int -> Module -> Path Abs File -> Path Abs File -> Text -> (String -> IO ()) -> Assertion
asmCompileAssertion' entryPoint optLevel md mainFile expectedFile stdinText step = do
  step "Generate C code"
  case run $ runReader entryPoint' $ runError @JuvixError $ Pipeline.asmToMiniC md of
    Left e -> do
      let err :: AsmError = fromJust (fromJuvixError e)
      assertFailure ("code generation failed:" <> "\n" <> unpack (err ^. asmErrorMsg))
    Right C.MiniCResult {..} ->
      withTempDir'
        ( \dirPath -> do
            let cFile = dirPath <//> replaceExtension' ".c" (filename mainFile)
            writeFileEnsureLn cFile _resultCCode
            Runtime.clangAssertion optLevel cFile expectedFile stdinText step
        )
  where
    -- TODO: In the future, the target supplied here might need to correspond to
    -- the actual target, and C code will then need to be re-generated for each
    -- target separately. Now this works only because those limits that
    -- Prealloc.hs uses are the same for the Native64 and Wasm32 targets.
    entryPoint' :: EntryPoint
    entryPoint' =
      entryPoint
        { _entryPointDebug = True,
          _entryPointTarget = Just TargetCNative64,
          _entryPointOptimizationLevel = optLevel
        }

asmCompileAssertion :: Path Abs Dir -> Path Abs File -> Path Abs File -> Text -> (String -> IO ()) -> Assertion
asmCompileAssertion root' mainFile expectedFile stdinText step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (show err)
    Right md -> do
      entryPoint <- testDefaultEntryPointIO root' mainFile
      asmCompileAssertion' entryPoint 3 md mainFile expectedFile stdinText step
