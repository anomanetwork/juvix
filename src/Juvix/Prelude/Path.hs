module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
    module Path.IO,
    module Juvix.Prelude.Path.SomePath,
  )
where

import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Data.FileExt
import Juvix.Prelude.Base
import Juvix.Prelude.Path.OrphanInstances ()
import Juvix.Prelude.Path.SomePath
import Path hiding (toFilePath, (<.>), (</>))
import Path qualified
import Path.IO hiding (listDirRel, walkDirRel)
import Path.Internal (relRootFP)

data FileOrDir

absDir :: FilePath -> Path Abs Dir
absDir r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsDir r)

infixr 5 <//>

-- | Synonym for Path.</>. Useful to avoid name clashes
(<//>) :: Path b Dir -> Path Rel t -> Path b t
(<//>) = (Path.</>)

infixr 5 <///>

-- | Appends a relative path to some directory
(<///>) :: SomeBase Dir -> Path Rel t -> SomeBase t
(<///>) s r = mapSomeBase (<//> r) s

someFile :: FilePath -> SomeBase File
someFile r = fromMaybe (error ("not a file path: " <> pack r)) (parseSomeFile r)

someDir :: FilePath -> SomeBase Dir
someDir r = fromMaybe (error ("not a dir path: " <> pack r)) (parseSomeDir r)

relFile :: FilePath -> Path Rel File
relFile r = fromMaybe (error ("not a relative file path: " <> pack r)) (parseRelFile r)

relDir :: FilePath -> Path Rel Dir
relDir r = fromMaybe (error ("not a relative directory path: " <> pack r)) (parseRelDir r)

absFile :: FilePath -> Path Abs File
absFile r = fromMaybe (error ("not an absolute file path: " <> pack r)) (parseAbsFile r)

destructAbsDir :: Path Abs Dir -> (Path Abs Dir, [Path Rel Dir])
destructAbsDir d = go d []
  where
    go :: Path Abs Dir -> [Path Rel Dir] -> (Path Abs Dir, [Path Rel Dir])
    go p acc
      | isRoot p = (p, acc)
      | otherwise = go (parent p) (dirname p : acc)

isRoot :: Path a Dir -> Bool
isRoot p = parent p == p

-- | is the root of absolute files always "/" ?
destructAbsFile :: Path Abs File -> (Path Abs Dir, [Path Rel Dir], Path Rel File)
destructAbsFile x = (root, dirs, filename x)
  where
    (root, dirs) = destructAbsDir (parent x)

isHiddenDirectory :: Path b Dir -> Bool
isHiddenDirectory p
  | toFilePath p == relRootFP = False
  | otherwise = case toFilePath (dirname p) of
      '.' : _ -> True
      _ -> False

someBaseToAbs :: Path Abs Dir -> SomeBase b -> Path Abs b
someBaseToAbs root = \case
  Rel r -> root <//> r
  Abs a -> a

removeExtensions :: Path b File -> Path b File
removeExtensions p = maybe p removeExtensions (removeExtension p)

removeExtension :: Path b File -> Maybe (Path b File)
removeExtension = fmap fst . splitExtension

removeExtension' :: Path b File -> Path b File
removeExtension' = fst . fromJust . splitExtension

addExtensions :: forall m l b. (MonadThrow m, Foldable l) => l String -> Path b File -> m (Path b File)
addExtensions exts p = foldM (flip addExtension) p exts

replaceExtensions :: (MonadThrow m, Foldable l) => l String -> Path b File -> m (Path b File)
replaceExtensions ext = addExtensions ext . removeExtensions

replaceExtensions' :: (Foldable l) => l String -> Path b File -> Path b File
replaceExtensions' ext = fromJust . replaceExtensions ext

addExtensions' :: (Foldable l) => l String -> Path b File -> Path b File
addExtensions' ext = fromJust . addExtensions ext

-- | TODO this is ugly. Please, fix it. FileExtJuvixMarkdown needs special
-- treatment because ".juvix.md" is not recognised as a valid extension by `addExtension`
addFileExt :: FileExt -> Path b File -> Path b File
addFileExt = \case
  FileExtJuvixMarkdown -> addExtensions' (toList juvixMarkdownFileExts)
  ext -> addExtension' (fileExtToIsString ext)

addExtension' :: String -> Path b File -> Path b File
addExtension' ext = fromMaybe err . addExtension ext
  where
    err :: a
    err = error (show ext <> " is not a valid extension")

replaceExtension' :: String -> Path b File -> Path b File
replaceExtension' ext = fromJust . replaceExtension ext

dirnameToFile :: Path x Dir -> Path Rel File
dirnameToFile = relFile . dropTrailingPathSeparator . toFilePath . dirname

parents :: Path Abs a -> NonEmpty (Path Abs Dir)
parents = go [] . parent
  where
    go :: [Path Abs Dir] -> Path Abs Dir -> NonEmpty (Path Abs Dir)
    go ac p
      | isRoot p = NonEmpty.reverse (p :| ac)
      | otherwise = go (p : ac) (parent p)

withTempDir' :: (MonadIO m, MonadMask m) => (Path Abs Dir -> m a) -> m a
withTempDir' = withSystemTempDir "tmp"

createTempDir' :: (MonadIO m) => m (Path Abs Dir)
createTempDir' = do
  dir <- getTempDir
  createTempDir dir "tmp"

-- | 'pure True' if the file exists and is executable, 'pure False' otherwise
isExecutable :: (MonadIO m) => Path b File -> m Bool
isExecutable f = doesFileExist f &&^ (executable <$> getPermissions f)

isPathPrefix :: Path b Dir -> Path b t -> Bool
isPathPrefix p1 p2 = case L.stripPrefix (toFilePath p1) (toFilePath p2) of
  Nothing -> False
  Just {} -> True

writeFile :: (MonadIO m) => Path Abs File -> ByteString -> m ()
writeFile p bs = do
  ensureDir (parent p)
  liftIO $ BS.writeFile (toFilePath p) bs

pathFileToPathDir :: Path Abs File -> Path Abs Dir
pathFileToPathDir = absDir . toFilePath

sanitizeFilename :: String -> FilePath
sanitizeFilename = map (\c -> if isAlphaNum c || c `elem` (".-_" :: String) then c else '_')
