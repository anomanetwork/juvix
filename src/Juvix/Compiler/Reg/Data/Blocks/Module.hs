module Juvix.Compiler.Reg.Data.Blocks.Module
  ( module Juvix.Compiler.Reg.Data.Blocks.Module,
    module Juvix.Compiler.Reg.Data.Blocks.InfoTable,
    module Juvix.Compiler.Tree.Data.Module.Base,
  )
where

import Juvix.Compiler.Reg.Data.Blocks.InfoTable
import Juvix.Compiler.Reg.Language.Blocks
import Juvix.Compiler.Tree.Data.Module.Base

type Module = Module'' Block ()

type ModuleTable = ModuleTable'' Block ()
