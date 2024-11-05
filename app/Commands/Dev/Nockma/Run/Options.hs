module Commands.Dev.Nockma.Run.Options where

import CommonOptions

data NockmaRunOptions = NockmaRunOptions
  { _nockmaRunFile :: AppPath File,
    _nockmaRunAnomaDir :: Maybe (AppPath Dir),
    _nockmaRunProfile :: Bool,
    _nockmaRunArgs :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunOptions

parseNockmaRunOptions :: Parser NockmaRunOptions
parseNockmaRunOptions = do
  _nockmaRunFile <- parseInputFile FileExtNockma
  _nockmaRunArgs <- optional $ do
    _pathPath <-
      option
        somePreFileOpt
        ( long "args"
            <> metavar "ARGS_FILE"
            <> help "Path to file containing args. When run on anoma, the args file should contain a list (i.e. to pass 2 and [1 4] as args, the contents should be [2 [1 4] 0])."
            <> action "file"
        )
    pure AppPath {_pathIsInput = True, ..}
  _nockmaRunAnomaDir <- optional anomaDirOpt
  _nockmaRunProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaRunOptions {..}
