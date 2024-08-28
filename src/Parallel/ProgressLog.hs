module Parallel.ProgressLog where

import Control.Concurrent.STM.TVar (stateTVar)
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Compiler.Concrete.Print.Base
import Juvix.Compiler.Pipeline.Driver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base
import Juvix.Data.CodeAnn
import Juvix.Data.Logger
import Juvix.Prelude

data ProgressLog :: Effect where
  ProgressLog :: LogItem -> ProgressLog m ()

data ProgressLogOptions = ProgressLogOptions
  { _progressLogOptionsShowThreadId :: Bool,
    _progressLogOptionsPackageRoot :: Path Abs Dir,
    _progressLogOptionsImportTree :: ImportTree
  }

data LogItem = LogItem
  { _logItemModule :: ImportNode,
    _logItemThreadId :: ThreadId,
    _logItemAction :: CompileAction,
    _logItemMessage :: Doc CodeAnn
  }

data ProgressLogState = ProgressLogState
  { _stateProcessed :: Natural,
    _stateProcessedPerPackage :: HashMap (Path Abs Dir) Natural
  }

data LogQueueItem
  = LogQueueItem LogItemDetails
  | -- | no more log items will be handled after this
    LogQueueClose

type LogQueue = TQueue LogQueueItem

data LogKind
  = LogMainPackage
  | LogDependency

data LogItemDetails = LogItemDetails
  { _logItemDetailsNumber :: Natural,
    _logItemDetailsKind :: LogKind,
    _logItemDetailsLog :: LogItem
  }

makeSem ''ProgressLog
makeLenses ''ProgressLogOptions
makeLenses ''LogItem
makeLenses ''ProgressLogState
makeLenses ''LogItemDetails

iniProgressLogState :: ProgressLogState
iniProgressLogState =
  ProgressLogState
    { _stateProcessed = 0,
      _stateProcessedPerPackage = mempty
    }

runProgressLog ::
  forall r a.
  (Members '[Logger, Concurrent] r) =>
  ProgressLogOptions ->
  Sem (ProgressLog ': r) a ->
  Sem r a
runProgressLog opts m = do
  logs :: LogQueue <- newTQueueIO
  st :: TVar ProgressLogState <- newTVarIO iniProgressLogState
  withAsync (handleLogs logs) $ \logHandler -> do
    x <- interpret (handler st logs) m
    atomically (STM.writeTQueue logs LogQueueClose)
    wait logHandler
    return x
  where
    tree :: ImportTree
    tree = opts ^. progressLogOptionsImportTree

    numModulesByPackage :: HashMap (Path Abs Dir) Natural
    numModulesByPackage = fromIntegral . length <$> importTreeNodesByPackage tree

    packageSize :: Path Abs Dir -> Natural
    packageSize pkg = fromJust (numModulesByPackage ^. at pkg)

    mainPackage :: Path Abs Dir
    mainPackage = opts ^. progressLogOptionsPackageRoot

    handleLogs :: forall r'. (Members '[Logger, Concurrent] r') => LogQueue -> Sem r' ()
    handleLogs q = queueLoopWhile q $ \case
      LogQueueClose -> do
        logVerbose (mkAnsiText (annotate AnnKeyword "Finished compilation"))
        return False
      LogQueueItem d -> do
        let l :: LogItem
            l = d ^. logItemDetailsLog

            node :: ImportNode
            node = l ^. logItemModule

            dependencyTag = case d ^. logItemDetailsKind of
              LogMainPackage -> Nothing
              LogDependency -> Just (annotate AnnKeyword "Dependency" <+> pretty (node ^. importNodePackageRoot))

            num =
              annotate AnnLiteralInteger (pretty (d ^. logItemDetailsNumber))
                <+> kwOf
                <+> annotate AnnLiteralInteger (pretty (packageSize (node ^. importNodePackageRoot)))

            tid :: Maybe (Doc CodeAnn) = do
              guard (opts ^. progressLogOptionsShowThreadId)
              return (annotate AnnImportant (pretty @String (show (l ^. logItemThreadId))))

            msg :: AnsiText
            msg =
              mkAnsiText $
                (brackets <$> tid)
                  <?+> (brackets <$> dependencyTag)
                  <?+> brackets num
                    <+> annotate AnnKeyword (pretty (l ^. logItemAction))
                    <+> l ^. logItemMessage

        let loglvl = compileActionLogLevel (l ^. logItemAction)
        logMessage loglvl msg
        return True

    handler :: TVar ProgressLogState -> LogQueue -> EffectHandlerFO ProgressLog r
    handler st logs = \case
      ProgressLog i ->
        atomically $ do
          (n, isLast) <- getNextNumber
          let k
                | fromMainPackage = LogMainPackage
                | otherwise = LogDependency
              d =
                LogItemDetails
                  { _logItemDetailsKind = k,
                    _logItemDetailsNumber = n,
                    _logItemDetailsLog = i
                  }
          STM.writeTQueue logs (LogQueueItem d)
          when isLast (STM.writeTQueue logs LogQueueClose)
        where
          fromPackage :: Path Abs Dir
          fromPackage = i ^. logItemModule . importNodePackageRoot

          fromMainPackage :: Bool
          fromMainPackage = fromPackage == mainPackage

          totalModules :: Natural
          totalModules = importTreeSize (opts ^. progressLogOptionsImportTree)

          getNextNumber :: STM (Natural, Bool)
          getNextNumber =
            stateTVar st $ \old ->
              let processed = old ^. stateProcessed + 1
                  pkgProcessed = over (at fromPackage) (Just . maybe 1 succ) (old ^. stateProcessedPerPackage)
                  st' =
                    old
                      { _stateProcessed = processed,
                        _stateProcessedPerPackage = pkgProcessed
                      }
                  ret = (fromJust (pkgProcessed ^. at fromPackage), processed == totalModules)
               in (ret, st')

queueLoopWhile :: (Members '[Concurrent] r) => TQueue a -> (a -> Sem r Bool) -> Sem r ()
queueLoopWhile q f = do
  whileCond <- atomically (readTQueue q) >>= f
  when (whileCond) (queueLoopWhile q f)

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
