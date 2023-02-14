module Commands.Dev.Geb.Eval.Options where

import CommonOptions
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb

data GebEvalOptions = GebEvalOptions
  { _gebEvalOptionsInputFile :: AppPath File,
    _gebEvalOptionsEvalStrategy :: Geb.EvalStrategy,
    _gebEvalOptionsNormalise :: Bool
  }
  deriving stock (Data)

makeLenses ''GebEvalOptions

instance CanonicalProjection GebEvalOptions Geb.EvaluatorOptions where
  project x =
    Geb.EvaluatorOptions
      { _evaluatorOptionsEvalStrategy = (x ^. gebEvalOptionsEvalStrategy),
        _evaluatorOptionsNormalise = (x ^. gebEvalOptionsNormalise)
      }

instance CanonicalProjection GebEvalOptions Geb.Options where
  project _ = Geb.defaultOptions

parseGebEvalOptions :: Parser GebEvalOptions
parseGebEvalOptions = do
  _gebEvalOptionsInputFile <- parseInputJuvixGebFile
  _gebEvalOptionsEvalStrategy <- optEvalStrategy
  _gebEvalOptionsNormalise <- optNormalise
  pure GebEvalOptions {..}

optEvalStrategy :: Parser Geb.EvalStrategy
optEvalStrategy =
  option
    (eitherReader parseStrategy)
    ( long "eval-strategy"
        <> short 's'
        <> metavar "EVAL_STRATEGY"
        <> value Geb.CallByValue
        <> showDefaultWith customShow
        <> help "options: call-by-value, call-by-name, and full"
    )
  where
    parseStrategy :: String -> Either String Geb.EvalStrategy
    parseStrategy = \case
      "call-by-value" -> Right Geb.CallByValue
      "call-by-name" -> Right Geb.CallByName
      s -> Left $ "Unrecognised evaluation strategy: " <> s

    customShow :: Geb.EvalStrategy -> String
    customShow = \case
      Geb.CallByValue -> "call-by-value"
      Geb.CallByName -> "call-by-name"

optNormalise :: Parser Bool
optNormalise =
  switch
    ( long "normalise"
        <> short 'n'
        <> showDefault
        <> help "Output a Geb morphism in normal form"
    )
