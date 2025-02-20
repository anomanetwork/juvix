module Juvix.Compiler.Store.Backend.Options where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Options qualified as Store
import Juvix.Extra.Serialize
import Juvix.Prelude
import Path qualified

data Options = Options
  { _optionsInfo :: Store.Options,
    _optionsFinalTarget :: Maybe Target
  }
  deriving stock (Show, Eq, Generic)

instance Serialize Options

instance NFData Options

makeLenses ''Options

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint e@EntryPoint {..} =
  Options
    { _optionsInfo = Store.fromEntryPoint e,
      _optionsFinalTarget = _entryPointTarget
    }

getOptionsSubdir :: Target -> Options -> Path Rel Dir
getOptionsSubdir midTarget opts =
  subdir1
    Path.</> maybe $(mkRelDir "default") (getTargetSubdir midTarget) (opts ^. optionsFinalTarget)
  where
    subdir1 =
      if
          | opts ^. optionsInfo . Store.optionsDebug -> $(mkRelDir "debug")
          | otherwise -> $(mkRelDir "release")
