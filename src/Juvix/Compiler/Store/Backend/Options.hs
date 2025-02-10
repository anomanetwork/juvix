module Juvix.Compiler.Store.Backend.Options where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Serialize
import Juvix.Prelude
import Path qualified

data Options = Options
  { _optionsDebug :: Bool,
    _optionsOptimizationLevel :: Int,
    _optionsFieldSize :: Natural,
    _optionsTarget :: Maybe Target
  }
  deriving stock (Show, Eq, Generic)

instance Serialize Options

instance NFData Options

makeLenses ''Options

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint EntryPoint {..} =
  Options
    { _optionsDebug = _entryPointDebug,
      _optionsOptimizationLevel = _entryPointOptimizationLevel,
      _optionsFieldSize = _entryPointFieldSize,
      _optionsTarget = _entryPointTarget
    }

getOptionsSubdir :: Options -> Path Rel Dir
getOptionsSubdir opts =
  subdir1
    Path.</> maybe $(mkRelDir "default") getTargetSubdir (opts ^. optionsTarget)
  where
    subdir1 =
      if
          | opts ^. optionsDebug -> $(mkRelDir "debug")
          | otherwise -> $(mkRelDir "release")
