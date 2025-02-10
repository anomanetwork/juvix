module Juvix.Compiler.Core.Data.Stripped.Module
  ( module Juvix.Compiler.Core.Data.Stripped.Module,
    module Juvix.Compiler.Core.Data.Module.Base,
    module Juvix.Compiler.Core.Data.Stripped.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.Stripped.InfoTable
import Juvix.Compiler.Core.Language.Stripped

type Module = Module' InfoTable

lookupConstructorInfo' :: Module -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' md tag =
  lookupTabConstructorInfo' (md ^. moduleInfoTable) tag
    <|> lookupTabConstructorInfo' (md ^. moduleImportsTable) tag

lookupConstructorInfo :: Module -> Tag -> ConstructorInfo
lookupConstructorInfo md tag = fromJust $ lookupConstructorInfo' md tag
