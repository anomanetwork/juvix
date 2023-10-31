module Commands.Init where

import Data.Text qualified as Text
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Versions
import Juvix.Compiler.Concrete.Print (ppOutDefaultNoComments)
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Data.Effect.Fail.Extra qualified as Fail
import Juvix.Extra.Paths
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Err = Text

parse :: Parsec Void Text a -> Text -> Either Err a
parse p t = mapLeft ppErr (P.runParser p "<stdin>" t)

ppErr :: P.ParseErrorBundle Text Void -> Text
ppErr = pack . errorBundlePretty

init :: forall r. (Members '[Embed IO] r) => Sem r ()
init = do
  checkNotInProject
  say "✨ Your next Juvix adventure is about to begin! ✨"
  say "I will help you set it up"
  pkg <- getPackage
  say ("creating " <> pack (toFilePath packageFilePath))
  embed (Utf8.writeFile @IO (toFilePath packageFilePath) (renderPackage pkg))
  checkPackage
  say "you are all set"
  where
    renderPackage :: Package -> Text
    renderPackage pkg = toPlainText (ppOutDefaultNoComments (toConcrete v1PackageDescriptionType pkg))

checkNotInProject :: forall r. (Members '[Embed IO] r) => Sem r ()
checkNotInProject =
  whenM (orM [doesFileExist juvixYamlFile, doesFileExist packageFilePath]) err
  where
    err :: Sem r ()
    err = do
      say "You are already in a Juvix project"
      embed exitFailure

checkPackage :: forall r. (Members '[Embed IO] r) => Sem r ()
checkPackage = do
  cwd <- getCurrentDir
  ep <- runError @JuvixError (readPackageIO' cwd DefaultBuildDir)
  case ep of
    Left {} -> do
      say "Package.juvix is invalid. Please raise an issue at https://github.com/anoma/juvix/issues"
      embed exitFailure
    Right {} -> return ()

getPackage :: forall r. (Members '[Embed IO] r) => Sem r Package
getPackage = do
  tproj <- getProjName
  say "Write the version of your project [leave empty for 0.0.0]"
  tversion :: SemVer <- getVersion
  cwd <- getCurrentDir
  return
    Package
      { _packageName = tproj,
        _packageVersion = tversion,
        _packageBuildDir = Nothing,
        _packageMain = Nothing,
        _packageDependencies = [defaultStdlibDep DefaultBuildDir],
        _packageFile = cwd <//> juvixYamlFile,
        _packageLockfile = Nothing
      }

getProjName :: forall r. (Members '[Embed IO] r) => Sem r Text
getProjName = do
  d <- getDefault
  let defMsg :: Text
      defMsg = case d of
        Nothing -> mempty
        Just d' -> " [leave empty for '" <> d' <> "']"
  say
    ( "Write the name of your project"
        <> defMsg
        <> " (lower case letters, numbers and dashes are allowed): "
    )
  readName d
  where
    getDefault :: Sem r (Maybe Text)
    getDefault = runFail $ do
      dir <- map toLower . dropTrailingPathSeparator . toFilePath . dirname <$> getCurrentDir
      Fail.fromRight (parse projectNameParser (pack dir))
    readName :: Maybe Text -> Sem r Text
    readName def = go
      where
        go :: Sem r Text
        go = do
          txt <- embed getLine
          if
              | Text.null txt, Just def' <- def -> return def'
              | otherwise ->
                  case parse projectNameParser txt of
                    Right p
                      | Text.length p <= projextNameMaxLength -> return p
                      | otherwise -> do
                          say ("The project name cannot exceed " <> prettyText projextNameMaxLength <> " characters")
                          retry
                    Left err -> do
                      say err
                      retry
          where
            retry :: Sem r Text
            retry = do
              tryAgain
              go

say :: (Members '[Embed IO] r) => Text -> Sem r ()
say = embed . putStrLn

tryAgain :: (Members '[Embed IO] r) => Sem r ()
tryAgain = say "Please, try again:"

getVersion :: forall r. (Members '[Embed IO] r) => Sem r SemVer
getVersion = do
  txt <- embed getLine
  if
      | Text.null txt -> return defaultVersion
      | otherwise -> case parse semver' txt of
          Right r -> return r
          Left err -> do
            say err
            say "The version must follow the 'Semantic Versioning 2.0.0' specification"
            retry
  where
    retry :: Sem r SemVer
    retry = do
      tryAgain
      getVersion

projextNameMaxLength :: Int
projextNameMaxLength = 100

projectNameParser :: Parsec Void Text Text
projectNameParser = do
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP (Just "project name character") validChar
  P.hspace
  P.eof
  return (Text.cons h t)
  where
    validFirstChar :: Char -> Bool
    validFirstChar c =
      isAscii c
        && (isLower c || isNumber c)
    validChar :: Char -> Bool
    validChar c = c == '-' || validFirstChar c
