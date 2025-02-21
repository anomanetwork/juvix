module Juvix.Compiler.Tree.Pipeline
  ( module Juvix.Compiler.Tree.Pipeline,
    module Juvix.Compiler.Tree.Data.Module,
  )
where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Transformation

toNockma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toNockma = mapReader fromEntryPoint . applyTransformations toNockmaTransformations

toAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toAsm = mapReader fromEntryPoint . applyTransformations toAsmTransformations

toCairoAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toCairoAsm = mapReader fromEntryPoint . applyTransformations toCairoAsmTransformations
