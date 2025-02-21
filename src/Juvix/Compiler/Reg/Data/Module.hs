module Juvix.Compiler.Reg.Data.Module
  ( module Juvix.Compiler.Reg.Data.Module,
    module Juvix.Compiler.Reg.Data.InfoTable,
    module Juvix.Compiler.Tree.Data.Module.Base,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Data.Module.Base

type Module = Module'' Code ()

type ModuleTable = ModuleTable'' Code ()
