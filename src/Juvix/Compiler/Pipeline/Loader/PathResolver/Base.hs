module Juvix.Compiler.Pipeline.Loader.PathResolver.Base
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Parser.Error
import Juvix.Prelude

data RootKind
  = RootKindPackage
  | RootKindSingleFile
  deriving stock (Show)

data RootInfo = RootInfo
  { _rootInfoPath :: Path Abs Dir,
    _rootInfoKind :: RootKind
  }
  deriving stock (Show)

data PathInfoTopModule = PathInfoTopModule
  { _pathInfoTopModule :: ScannedTopModuleName,
    _pathInfoRootInfo :: RootInfo
  }
  deriving stock (Show)

data PathResolver :: Effect where
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  GetPackageInfos :: PathResolver m (HashMap (Path Abs Dir) PackageInfo)
  ExpectedPathInfoTopModule :: ScannedTopModuleName -> PathResolver m PathInfoTopModule
  -- | Given a relative file *with no extension*, returns the list of packages
  -- that contain that file. The file extension is also returned since it can be
  -- FileExtJuvix or FileExtJuvixMarkdown.
  ResolvePath :: ImportScan -> PathResolver m (PackageInfo, FileExt)
  -- | The root is assumed to be a package root.
  WithResolverRoot :: Path Abs Dir -> m a -> PathResolver m a

makeLenses ''RootInfo
makeLenses ''PathInfoTopModule
makeSem ''PathResolver

withPathFile ::
  (Members '[PathResolver] r) =>
  TopModulePath ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPathFile m f = do
  (root, file) <- resolveTopModulePath m
  withResolverRoot root (f (root <//> file))

-- | Returns the root of the package where the module belongs and the path to
-- the module relative to the root.
resolveTopModulePath ::
  (Members '[PathResolver] r) =>
  TopModulePath ->
  Sem r (Path Abs Dir, Path Rel File)
resolveTopModulePath mp = do
  let scan = topModulePathToImportScan mp
      relpath = topModulePathToRelativePathNoExt mp
  (pkg, ext) <- resolvePath scan
  return (pkg ^. packageRoot, addFileExt ext relpath)

checkModulePath ::
  (Members '[Error ParserError] s) =>
  ScannedTopModuleName ->
  PathInfoTopModule ->
  Sem s ()
checkModulePath t pathInfo = do
  let expectedRootInfo = pathInfo ^. pathInfoRootInfo
      loc = getLoc t
      actualPath :: Path Abs File = loc ^. intervalFile
  case expectedRootInfo ^. rootInfoKind of
    RootKindSingleFile -> do
      let expectedNameNoExts :: String = toFilePath . removeExtensions . filename $ actualPath
          actualNameNoExts :: String = scannedTopModuleNameToDottedString t
      unless (expectedNameNoExts == actualNameNoExts)
        . throw
        $ ErrWrongTopModuleNameOrphan
          WrongTopModuleNameOrphan
            { _wrongTopModuleNameOrpahnExpectedName = pack expectedNameNoExts,
              _wrongTopModuleNameOrpahnActualName = WithLoc loc (pack actualNameNoExts)
            }
    RootKindPackage -> do
      let expectedRelPathNoExt = scannedTopModuleNameToRelPath t
          expectedAbsPathNoExt = expectedRootInfo ^. rootInfoPath <//> expectedRelPathNoExt
          expectedAbsPath = addFileExt FileExtJuvix expectedAbsPathNoExt
          actualPathNoExt = removeExtensions actualPath
      unlessM (equalPaths actualPathNoExt expectedAbsPathNoExt)
        . throw
        $ ErrWrongTopModuleName
          WrongTopModuleName
            { _wrongTopModuleNameActualName = t,
              _wrongTopModuleNameExpectedPath = expectedAbsPath,
              _wrongTopModuleNameActualPath = actualPath
            }
