module Juvix.Compiler.Asm.Pipeline
  ( module Juvix.Compiler.Asm.Pipeline,
    module Juvix.Compiler.Asm.Data.Module,
    module Juvix.Compiler.Asm.Options,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

-- | Perform transformations on JuvixAsm necessary before the translation to
-- JuvixReg
toReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toReg = mapReader fromEntryPoint . mapError (JuvixError @AsmError) . toReg'
  where
    toReg' :: (Members '[Error AsmError, Reader Options] r) => Module -> Sem r Module
    toReg' = validate >=> filterUnreachable >=> computeStackUsage >=> computePrealloc

-- | Perform transformations on JuvixAsm necessary before the translation to
-- Nockma
toNockma :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toNockma = mapReader fromEntryPoint . mapError (JuvixError @AsmError) . toNockma'
  where
    toNockma' :: (Members '[Error AsmError, Reader Options] r) => Module -> Sem r Module
    toNockma' = validate
