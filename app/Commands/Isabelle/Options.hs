module Commands.Isabelle.Options where

import CommonOptions
import Juvix.Compiler.Pipeline.EntryPoint

data IsabelleOptions = IsabelleOptions
  { _isabelleNonRecursive :: Bool,
    _isabelleInputFile :: Maybe (AppPath File),
    _isabelleOutputDir :: AppPath Dir,
    _isabelleStdout :: Bool,
    _isabelleOnlyTypes :: Bool
  }
  deriving stock (Data)

makeLenses ''IsabelleOptions

parseIsabelle :: Parser IsabelleOptions
parseIsabelle = do
  _isabelleNonRecursive <-
    switch
      ( long "non-recursive"
          <> help "Do not process imported modules recursively"
      )
  _isabelleOutputDir <-
    parseGenericOutputDir
      ( value "isabelle"
          <> showDefault
          <> help "Isabelle/HOL output directory"
          <> action "directory"
      )
  _isabelleStdout <-
    switch
      ( long "stdout"
          <> help "Write the output to stdout instead of a file"
      )
  _isabelleOnlyTypes <-
    switch
      ( long "only-types"
          <> help "Translate types only"
      )
  _isabelleInputFile <- optional (parseInputFile FileExtJuvix)
  pure IsabelleOptions {..}

instance EntryPointOptions IsabelleOptions where
  applyOptions IsabelleOptions {..} e = e {_entryPointIsabelleOnlyTypes = _isabelleOnlyTypes}
