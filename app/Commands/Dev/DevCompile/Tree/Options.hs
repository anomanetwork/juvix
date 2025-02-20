{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Tree.Options
  ( module Commands.Dev.DevCompile.Tree.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data TreeOptions (k :: InputKind) = TreeOptions
  { _treeCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (TreeOptions k)

makeLenses ''TreeOptions

parseTree :: (SingI k) => Parser (TreeOptions k)
parseTree = do
  _treeCompileCommonOptions <- parseCompileCommonOptions
  pure TreeOptions {..}

instance EntryPointOptions (TreeOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetTree)
      . applyOptions (opts ^. treeCompileCommonOptions)
