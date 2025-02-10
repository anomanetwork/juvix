module Juvix.Compiler.Core.Data.Stripped.Module
  ( module Juvix.Compiler.Core.Data.Stripped.Module,
    module Juvix.Compiler.Core.Data.Module.Base,
    module Juvix.Compiler.Core.Data.Stripped.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.Stripped.InfoTable

type Module = Module' InfoTable
