module Anoma.Client.Base where

import Base
import Juvix.Prelude.Pretty

data TestStep :: Effect where
  Step :: Text -> TestStep m ()

makeSem ''TestStep

runStep :: (Member EmbedIO r) => (Text -> IO ()) -> Sem (TestStep ': r) a -> Sem r a
runStep f = interpret $ \case
  Step t -> liftIO $ f t

pollForOutput :: forall r a. (Members '[Error SimpleError, EmbedIO] r) => Int -> (a -> Bool) -> Sem r a -> Sem r a
pollForOutput timeoutMillis isDataAvailable action = runConcurrent $ do
  raceResult <- race timeoutAction go
  case raceResult of
    Left {} -> throw (SimpleError (mkAnsiText @Text "pollForOutput: Operation timed out"))
    Right xs -> return xs
  where
    go :: Sem (Concurrent ': r) a
    go = do
      res <- inject action
      if
          | isDataAvailable res -> return res
          | otherwise -> threadDelay (50 * 1000) >> go

    timeoutAction :: (Member Concurrent x) => Sem x ()
    timeoutAction = void (threadDelay (timeoutMillis * 1000))
