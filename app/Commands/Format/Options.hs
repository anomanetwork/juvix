module Commands.Format.Options where

import CommonOptions

data FormatOptions = FormatOptions
  { _formatInput :: Maybe (AppPath FileOrDir),
    _formatCheck :: Bool,
    _formatMigration :: Migration,
    _formatInPlace :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseMigration :: Parser Migration
parseMigration = do
  m <-
    optional $
      option
        (enumReader (Proxy @Migrate))
        ( long "migration"
            <> metavar "MIGRATION"
            <> completer (enumCompleter (Proxy @Migrate))
            <> help "Migrates files in a project (doesn't work in single file mode or stdin)"
        )

  pure (Migration m)

parseInputFileOrDir :: Parser (AppPath FileOrDir)
parseInputFileOrDir = do
  _pathPath <-
    argument
      somePreFileOrDirOpt
      ( metavar "JUVIX_FILE_OR_PROJECT"
          <> help ("Path to a " <> show FileExtJuvix <> " file or to a directory containing a Juvix project.")
          <> completer (extCompleter FileExtJuvix)
          <> action "directory"
      )
  pure AppPath {_pathIsInput = True, ..}

parseFormat :: Parser FormatOptions
parseFormat = do
  _formatInput <- optional parseInputFileOrDir
  _formatMigration <- parseMigration
  _formatCheck <-
    switch
      ( long "check"
          <> help "Do not print reformatted sources or unformatted file paths to standard output. Exit code 1 if a file wasn't already formatted."
      )
  _formatInPlace <-
    switch
      ( long "in-place"
          <> help "Do not print reformatted sources to standard output. Overwrite the target's contents with the formatted version if the formatted version differs from the original content."
      )
  pure FormatOptions {..}
