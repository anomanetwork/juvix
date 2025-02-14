module Juvix.Compiler.Pipeline.Modular.Cache where

import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Compiler.Store.Backend.Module
import Juvix.Data.Effect.Cache
import Juvix.Data.ModuleId

type ModuleCache t = Cache ModuleId (PipelineResult (Module' t))
