{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Data.Effect.Cache
  ( runCache,
    evalCache,
    evalCacheSetup,
    evalCacheEmpty,
    evalCacheEmptySetup,
    runCacheEmpty,
    cacheGet,
    cacheGetResult,
    cacheLookup,
    evalSingletonCache,
    cacheSingletonGet,
    CacheResult (..),
    cacheResultHit,
    cacheResult,
    cacheSetupHandler,
    Cache,
    SCache,
  )
where

import Juvix.Prelude.Base

data CacheResult a = CacheResult
  { _cacheResultHit :: Bool,
    _cacheResult :: a
  }
  deriving stock (Show, Eq, Generic)

instance (NFData a) => NFData (CacheResult a)

makeLenses ''CacheResult

data Cache (k :: GHCType) (v :: GHCType) :: Effect where
  CacheGetResult :: k -> Cache k v m (CacheResult v)
  CacheLookup :: k -> Cache k v m (Maybe v)

makeSem ''Cache

-- | Singleton cache
type SCache = Cache ()

-- | Run a 'Cache' effect purely.
runCache ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCache f c = runStateShared c . re f
{-# INLINE runCache #-}

runCacheSetup ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) ()) ->
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCacheSetup setup f c = runStateShared c . reSetup setup f
{-# INLINE runCacheSetup #-}

evalCache ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCache f c = fmap snd . runCache f c
{-# INLINE evalCache #-}

evalCacheSetup ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) ()) ->
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCacheSetup setup f c = fmap snd . runCacheSetup setup f c
{-# INLINE evalCacheSetup #-}

evalCacheEmpty ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCacheEmpty f = evalCache f mempty
{-# INLINE evalCacheEmpty #-}

evalCacheEmptySetup ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) ()) ->
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCacheEmptySetup setup f = evalCacheSetup setup f mempty
{-# INLINE evalCacheEmptySetup #-}

runCacheEmpty ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCacheEmpty f = runCache f mempty
{-# INLINE runCacheEmpty #-}

cacheSingletonGet :: (Members '[SCache v] r) => Sem r v
cacheSingletonGet = cacheGet ()

evalSingletonCache ::
  Sem (SCache v ': r) v ->
  Sem (SCache v ': r) a ->
  Sem r a
evalSingletonCache f c = evalCacheEmpty @() (const f) c
{-# INLINE evalSingletonCache #-}

cacheGet ::
  forall k v r.
  (Member (Cache k v) r) =>
  k ->
  Sem r v
cacheGet = fmap (^. cacheResult) . cacheGetResult

reSetup ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) ()) ->
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem (SharedState (HashMap k v) ': r) a
reSetup setup f = interpretTop (cacheSetupHandler setup f)

re ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem (SharedState (HashMap k v) ': r) a
re f = interpretTop (cacheSimpleHandler f)

cacheSimpleHandler ::
  forall v k r.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  EffectHandlerFO (Cache k v) (SharedState (HashMap k v) ': r)
cacheSimpleHandler f =
  \case
    CacheLookup k -> cacheLookup' k
    CacheGetResult k -> do
      mv <- cacheLookup' k
      case mv of
        Nothing -> do
          _cacheResult <- re f (f k)
          modifyShared @(HashMap k v) (set (at k) (Just _cacheResult))
          return
            CacheResult
              { _cacheResultHit = False,
                _cacheResult
              }
        Just _cacheResult ->
          return
            CacheResult
              { _cacheResultHit = True,
                _cacheResult
              }

cacheLookup' :: forall k v r. (Hashable k, Members '[SharedState (HashMap k v)] r) => k -> Sem r (Maybe v)
cacheLookup' k = getsShared @(HashMap k v) (^. at k)

cacheSetupHandler ::
  forall k v r.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) ()) ->
  (k -> Sem (Cache k v ': r) v) ->
  EffectHandlerFO (Cache k v) (SharedState (HashMap k v) ': r)
cacheSetupHandler setup f = do
  \case
    CacheLookup k -> cacheLookup' k
    CacheGetResult k -> do
      mv <- cacheLookup' k
      case mv of
        Just _cacheResult ->
          return
            CacheResult
              { _cacheResultHit = True,
                _cacheResult
              }
        Nothing -> re f $ do
          setup k
          cacheGetResult @k @v k
