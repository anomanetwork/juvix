module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
  )
where

import Juvix.Prelude.Base
import Path hiding ((<.>), (</>))
import Path qualified

-- | Synonym for Path.</>. Useful to avoid name clashes
infixr 5 <//>

(<//>) :: Path b Dir -> Path Rel t -> Path b t
(<//>) = (Path.</>)

relFile :: FilePath -> Path Rel File
relFile = fromJust . parseRelFile

relDir :: FilePath -> Path Rel Dir
relDir = fromJust . parseRelDir

absFile :: FilePath -> Path Abs File
absFile = fromJust . parseAbsFile

absDir :: FilePath -> Path Abs Dir
absDir = fromJust . parseAbsDir

destructPath :: Path b Dir -> [Path Rel Dir]
destructPath p = map relDir (splitPath (toFilePath p))

destructFilePath :: Path b File -> ([Path Rel Dir], Path Rel File)
destructFilePath p = case nonEmptyUnsnoc (nonEmpty' (splitPath (toFilePath p))) of
  (ps, f) -> (fmap relDir (maybe [] toList ps), relFile f)

isJuvixFile :: Path b File -> Bool
isJuvixFile = maybe False (== ".juvix") . fileExtension

isHiddenDirectory :: Path b Dir -> Bool
isHiddenDirectory p = case toFilePath (dirname p) of
  "./" -> False
  '.' : _ -> True
  _ -> False

parseRelFile' :: FilePath -> Path Rel File
parseRelFile' = fromJust . parseRelFile
