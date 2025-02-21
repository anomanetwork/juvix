module Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
  ( module Juvix.Compiler.Pipeline.Package.Loader.Error,
    module Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker (runTopModuleNameChecker)
import Juvix.Compiler.Core (CoreResult, coreResultModule)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver (evalModuleInfoCachePackageDotJuvix, processFileToStoredCore)
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.PathResolver
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process

data LoaderResource = LoaderResource
  { _loaderResourceResult :: CoreResult,
    _loaderResourcePackagePath :: Path Abs File
  }

makeLenses ''LoaderResource

runEvalFileEffIO ::
  forall r a.
  (Members '[TaggedLock, Reader EntryPoint, Files, EmbedIO, Error PackageLoaderError] r) =>
  Sem (EvalFileEff ': r) a ->
  Sem r a
runEvalFileEffIO = runProvider_ helper
  where
    helper :: forall x. Path Abs File -> Sem (EvalEff ': r) x -> Sem r x
    helper p m = do
      res <- loadPackage' p
      let loaderRes :: LoaderResource =
            LoaderResource
              { _loaderResourceResult = res,
                _loaderResourcePackagePath = p
              }
      interpret (handler loaderRes) m

    handler :: LoaderResource -> EvalEff m x -> Sem r x
    handler res = \case
      Eval' n -> toValue tab <$> evalNode n
      LookupIdentifier n ->
        ( maybe
            ( throw
                PackageLoaderError
                  { _packageLoaderErrorPath = packagePath,
                    _packageLoaderErrorCause = ErrPackageSymbolNotFound
                  }
            )
            return
            ( identifierSymbol n
                >>= (`HashMap.lookup` (tab ^. Core.identContext))
            )
        )
      AssertNodeType n ty -> assertNodeType' n ty
      where
        tab :: Core.InfoTable
        tab = Core.computeCombinedInfoTable (res ^. loaderResourceResult . coreResultModule)

        packagePath :: Path Abs File
        packagePath = res ^. loaderResourcePackagePath

        identifiers :: HashMap Symbol Core.IdentifierInfo
        identifiers = tab ^. Core.infoIdentifiers

        identifierSymbol :: Text -> Maybe Symbol
        identifierSymbol n = fst <$> find (\(_, ii) -> ii ^. Core.identifierName == n) (HashMap.toList identifiers)

        evalNode :: Node -> Sem r Node
        evalNode n = do
          n' <- doEval Nothing False packageLoc tab n
          case n' of
            Right resN -> return resN
            Left e -> do
              throw
                PackageLoaderError
                  { _packageLoaderErrorPath = packagePath,
                    _packageLoaderErrorCause =
                      ErrPackageEvaluationError
                        PackageEvaluationError
                          { _packageEvaluationErrorError = JuvixError e
                          }
                  }
          where
            packageLoc :: Interval
            packageLoc = singletonInterval (mkInitialLoc packagePath)

        assertNodeType' :: (Foldable f) => Node -> f TypeSpec -> Sem r TypeSpec
        assertNodeType' n tys = do
          evalN <- evalNode n
          case evalN of
            NCtr Constr {..} -> do
              let ci = Core.lookupTabConstructorInfo tab _constrTag
                  ii = Core.lookupTabInductiveInfo tab (ci ^. Core.constructorInductive)
                  ty = find (checkInductiveType ii) tys
              fromMaybeM err (return ty)
            _ -> err
          where
            err :: Sem r b
            err = throw PackageLoaderError {_packageLoaderErrorPath = packagePath, _packageLoaderErrorCause = ErrPackageTypeError}

            -- Check that the type of the package identifier is the expected type from the specific
            -- PackageDescription module that is provided by the PathResolver
            checkInductiveType :: Core.InductiveInfo -> TypeSpec -> Bool
            checkInductiveType ii t = checkPackageName (t ^. typeSpecName) && checkPackageLocation (t ^. typeSpecFile)
              where
                checkPackageName :: Text -> Bool
                checkPackageName typeName = ii ^. Core.inductiveName == typeName

                checkPackageLocation :: Path Abs File -> Bool
                checkPackageLocation f = case ii ^. Core.inductiveLocation of
                  Just l -> l ^. intervalFile == f
                  Nothing -> False

loadPackage' :: (Members '[TaggedLock, Reader EntryPoint, Files, EmbedIO, Error PackageLoaderError] r) => Path Abs File -> Sem r CoreResult
loadPackage' packagePath = do
  entry <- ask @EntryPoint
  mapError toPackageError
    . runConcurrent
    . ignoreLogger
    . evalInternetOffline
    . evalHighlightBuilder
    . runProcessIO
    . runFilesIO
    . evalTopNameIdGen defaultModuleId
    . mapError (JuvixError @GitProcessError)
    . runGitProcess
    . runEvalFileEffIO
    . runPackagePathResolver rootPath
    . runTopModuleNameChecker
    . runReader noMigration
    . evalModuleInfoCachePackageDotJuvix
    $ (^. pipelineResult) <$> processFileToStoredCore (packageEntryPoint entry)
  where
    toPackageError :: JuvixError -> PackageLoaderError
    toPackageError e =
      PackageLoaderError
        { _packageLoaderErrorPath = packagePath,
          _packageLoaderErrorCause = ErrPackageJuvixError (PackageJuvixError e)
        }

    rootPath :: Path Abs Dir
    rootPath = parent packagePath

    packageEntryPoint :: EntryPoint -> EntryPoint
    packageEntryPoint entry =
      entry
        { _entryPointRoot = root ^. rootRootDir,
          _entryPointResolverRoot = root ^. rootRootDir,
          _entryPointSomeRoot = root ^. rootSomeRoot,
          _entryPointBuildDir = root ^. rootBuildDir,
          _entryPointPackageId = rootPkg,
          _entryPointModulePath = Just packagePath,
          _entryPointStdin = Nothing,
          _entryPointPipeline = Just PipelineEval
        }
      where
        sroot :: SomeRoot
        sroot =
          SomeRoot
            { _someRootDir = rootPath,
              _someRootType = GlobalPackageDescription
            }

        root :: Root
        root =
          Root
            { _rootSomeRoot = sroot,
              _rootInvokeDir = rootPath,
              _rootBuildDir = DefaultBuildDir
            }

        rootPkg :: PackageId
        rootPkg =
          PackageId
            { _packageIdVersion = defaultVersion,
              _packageIdName = "Package"
            }
