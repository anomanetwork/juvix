module Juvix.Compiler.Asm.Data.Module
  ( module Juvix.Compiler.Asm.Data.Module,
    module Juvix.Compiler.Tree.Data.Module.Base,
    module Juvix.Compiler.Asm.Data.InfoTable,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Data.Module.Base

type Module = Module'' Code (Maybe FunctionInfoExtra)

type ModuleTable = ModuleTable'' Code (Maybe FunctionInfoExtra)
