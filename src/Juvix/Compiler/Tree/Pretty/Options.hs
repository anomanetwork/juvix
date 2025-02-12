module Juvix.Compiler.Tree.Pretty.Options where

import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Compiler.Tree.Data.Module.Base
import Juvix.Compiler.Tree.Language.Base

data Options = Options
  { _optSymbolNames :: HashMap Symbol Text,
    _optTagNames :: HashMap Tag Text
  }

makeLenses ''Options

defaultOptions :: Module'' t e -> Options
defaultOptions md =
  Options
    { _optSymbolNames =
        fmap (^. functionName) (md ^. moduleInfoTable . infoFunctions)
          <> fmap (^. inductiveName) (md ^. moduleInfoTable . infoInductives)
          <> fmap (^. functionName) (md ^. moduleImportsTable . infoFunctions)
          <> fmap (^. inductiveName) (md ^. moduleImportsTable . infoInductives),
      _optTagNames =
        fmap (^. constructorName) (md ^. moduleInfoTable . infoConstrs)
          <> fmap (^. constructorName) (md ^. moduleImportsTable . infoConstrs)
    }

toCoreOptions :: Options -> Core.Options
toCoreOptions Options {} = Core.defaultOptions
