module Juvix.Compiler.Pipeline.Modular.Cache where

import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Data.Effect.Cache
import Juvix.Data.ModuleId

type ModuleCache m = Cache ModuleId (PipelineResult m)
