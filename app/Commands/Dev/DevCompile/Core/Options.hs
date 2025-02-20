{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Core.Options
  ( module Commands.Dev.DevCompile.Core.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CoreOptions (k :: InputKind) = CoreOptions
  { _coreCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (CoreOptions k)

makeLenses ''CoreOptions

parseCore :: (SingI k) => Parser (CoreOptions k)
parseCore = do
  _coreCompileCommonOptions <- parseCompileCommonOptions
  pure CoreOptions {..}

instance EntryPointOptions (CoreOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetCore)
      . applyOptions (opts ^. coreCompileCommonOptions)
