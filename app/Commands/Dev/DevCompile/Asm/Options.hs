{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Asm.Options
  ( module Commands.Dev.DevCompile.Asm.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data AsmOptions (k :: InputKind) = AsmOptions
  { _asmCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (AsmOptions k)

makeLenses ''AsmOptions

parseAsm :: forall k. (SingI k) => Parser (AsmOptions k)
parseAsm = do
  _asmCompileCommonOptions <- parseCompileCommonOptions
  pure AsmOptions {..}

instance EntryPointOptions (AsmOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetAsm)
      . applyOptions (opts ^. asmCompileCommonOptions)
