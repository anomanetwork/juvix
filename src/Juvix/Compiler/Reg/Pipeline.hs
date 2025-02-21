module Juvix.Compiler.Reg.Pipeline
  ( module Juvix.Compiler.Reg.Pipeline,
    module Juvix.Compiler.Reg.Data.Module,
    Options,
  )
where

import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint)
import Juvix.Compiler.Reg.Data.Blocks.Module qualified as Blocks
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Transformation.Blocks.Liveness qualified as Blocks
import Juvix.Compiler.Reg.Translation.Blocks.FromReg qualified as Blocks

-- | Perform transformations on JuvixReg necessary before the translation to C
toC :: (Member (Reader EntryPoint) r) => Module -> Sem r Module
toC = mapReader fromEntryPoint . toC'
  where
    toC' :: (Member (Reader Options) r) => Module -> Sem r Module
    toC' = applyTransformations toCTransformations

-- | Perform transformations on JuvixReg necessary before the translation to Rust
toRust :: (Member (Reader EntryPoint) r) => Module -> Sem r Module
toRust = mapReader fromEntryPoint . toRust'
  where
    toRust' :: (Member (Reader Options) r) => Module -> Sem r Module
    toRust' = applyTransformations toRustTransformations

-- | Perform transformations on JuvixReg necessary before the translation to
-- Cairo assembly
toCasm :: (Member (Reader EntryPoint) r) => Module -> Sem r Blocks.Module
toCasm = mapReader fromEntryPoint . toCasm'
  where
    toCasm' :: (Member (Reader Options) r) => Module -> Sem r Blocks.Module
    toCasm' =
      applyTransformations toCasmTransformations
        >=> return . Blocks.computeLiveness . Blocks.fromReg
