module Juvix.Compiler.Reg.Pretty
  ( module Juvix.Compiler.Reg.Pretty,
    module Juvix.Compiler.Reg.Pretty.Base,
    module Juvix.Compiler.Reg.Pretty.Options,
  )
where

import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Pretty.Base
import Juvix.Compiler.Reg.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

ppOutDefault :: (PrettyCode c) => Module'' t e -> c -> AnsiText
ppOutDefault md = mkAnsiText . PPOutput . doc (defaultOptions md)

ppOut :: (PrettyCode c) => Options -> c -> AnsiText
ppOut o = mkAnsiText . PPOutput . doc o

ppTrace' :: (PrettyCode c) => Options -> c -> Text
ppTrace' opts = Ansi.renderStrict . reAnnotateS stylize . layoutPretty defaultLayoutOptions . doc opts

ppTrace :: (PrettyCode c) => Module'' t e -> c -> Text
ppTrace md = ppTrace' (defaultOptions md)

ppPrint :: (PrettyCode c) => Module'' t e -> c -> Text
ppPrint md = toPlainText . ppOutDefault md
