module Juvix.Compiler.Tree.Extra.Apply where

import Data.FileEmbed qualified as FE
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding
import Juvix.Compiler.Tree.Data.InfoTableBuilder
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Translation.FromSource

data ApplyBuiltins = ApplyBuiltins
  { -- | The number of `juvix_apply_n` functions.
    _applyBuiltinsNum :: Int,
    -- | Maps `n` to the function `juvix_apply_n`.
    _applyBuiltinsMap :: HashMap Int Symbol
  }

makeLenses ''ApplyBuiltins

applyBuiltinsModuleId :: ModuleId
applyBuiltinsModuleId =
  ModuleId
    { _moduleIdPath = nonEmptyToTopModulePathKey (pure "$ApplyBuiltinsModule$"),
      _moduleIdPackageId =
        PackageId
          { _packageIdName = "$",
            _packageIdVersion = SemVer 1 0 0 Nothing Nothing
          }
    }

applyBuiltins :: ApplyBuiltins
applyBuiltins = fst getApplyBuiltins

applyBuiltinsModule :: Module
applyBuiltinsModule = snd getApplyBuiltins

getApplyBuiltins :: (ApplyBuiltins, Module)
getApplyBuiltins = (blts, bs' ^. stateModule)
  where
    bs :: BuilderState
    bs = mkBuilderState (emptyModule applyBuiltinsModuleId)

    bs' :: BuilderState
    bs' =
      fromRight impossible $
        parseText' bs $
          decodeUtf8 $(FE.makeRelativeToProject "runtime/tree/apply.jvt" >>= FE.embedFile)

    blts :: ApplyBuiltins
    blts =
      ApplyBuiltins
        { _applyBuiltinsNum = 4,
          _applyBuiltinsMap =
            HashMap.fromList $ map mkApply [1 .. 4]
        }

    mkApply :: Int -> (Int, Symbol)
    mkApply x = (x, f)
      where
        idt = "juvix_apply_" <> show x
        f = case fromJust $ HashMap.lookup idt (bs' ^. stateIdents) of
          IdentFun s -> s
          _ -> impossible

addApplyBuiltins :: ModuleTable -> ModuleTable
addApplyBuiltins = over moduleTable (HashMap.insert applyBuiltinsModuleId applyBuiltinsModule)
