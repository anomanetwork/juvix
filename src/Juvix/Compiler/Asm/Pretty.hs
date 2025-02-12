module Juvix.Compiler.Asm.Pretty
  ( module Juvix.Compiler.Asm.Pretty,
    module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Pretty.Base
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => Module -> c -> AnsiText
ppOutDefault md = mkAnsiText . PPOutput . doc (defaultOptions md)

ppOut :: (PrettyCode c) => Options -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc o

ppTrace' :: (PrettyCode c) => Options -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc opts

ppTrace :: (PrettyCode c) => Module -> c -> Text
ppTrace md = ppTrace' (defaultOptions md)

ppPrint :: (PrettyCode c) => Module -> c -> Text
ppPrint md = show . ppOutDefault md
