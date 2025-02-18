module Juvix.Compiler.Pipeline.Modular.Run where

import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular
import Juvix.Prelude

runIOEitherPipeline ::
  forall a r.
  (Members '[TaggedLock, EmbedIO] r) =>
  EntryPoint ->
  Sem (ModularEff r) a ->
  Sem r (Either JuvixError a)
runIOEitherPipeline entry a =
  evalHighlightBuilder
    . runJuvixError
    . runReader entry
    . runFilesIO
    $ inject a
