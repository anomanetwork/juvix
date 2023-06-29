module Commands.Dev.Internal.Options where

import Commands.Dev.Internal.Arity.Options
import Commands.Dev.Internal.Pretty.Options
import Commands.Dev.Internal.Reachability.Options
import Commands.Dev.Internal.Typecheck.Options
import CommonOptions

data InternalCommand
  = Pretty InternalPrettyOptions
  | TypeCheck InternalTypeOptions
  | Arity InternalArityOptions
  | Reachability InternalReachabilityOptions
  deriving stock (Data)

parseInternalCommand :: Parser InternalCommand
parseInternalCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandArity,
        commandTypeCheck,
        commandReachability
      ]
  where
    commandArity :: Mod CommandFields InternalCommand
    commandArity = command "arity" arityInfo

    commandPretty :: Mod CommandFields InternalCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields InternalCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    commandReachability :: Mod CommandFields InternalCommand
    commandReachability = command "reachability" reachabilityInfo

    arityInfo :: ParserInfo InternalCommand
    arityInfo =
      info
        (Arity <$> parseInternalArity)
        (progDesc "Translate a Juvix file to Internal and insert holes")

    prettyInfo :: ParserInfo InternalCommand
    prettyInfo =
      info
        (Pretty <$> parseInternalPretty)
        (progDesc "Translate a Juvix file to Internal and pretty print the result")

    typeCheckInfo :: ParserInfo InternalCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseInternalType)
        (progDesc "Translate a Juvix file to Internal and typecheck the result")

    reachabilityInfo :: ParserInfo InternalCommand
    reachabilityInfo =
      info
        (Reachability <$> parseInternalReachability)
        (progDesc "Print reachability information")
