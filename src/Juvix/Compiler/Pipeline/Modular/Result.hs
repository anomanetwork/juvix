module Juvix.Compiler.Pipeline.Modular.Result where

import Juvix.Prelude

data PipelineResult a = PipelineResult
  { _pipelineResult :: a,
    -- | True if the module had to be recompiled. False if the module was loaded
    -- from disk.
    _pipelineResultChanged :: Bool
  }
  deriving stock (Generic)

makeLenses ''PipelineResult

instance (NFData a) => NFData (PipelineResult a)
