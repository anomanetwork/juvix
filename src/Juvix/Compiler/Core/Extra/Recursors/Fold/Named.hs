module Juvix.Compiler.Core.Extra.Recursors.Fold.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Fold

ufoldA :: Applicative f => (a -> [a] -> a) -> (Node -> f a) -> Node -> f a
ufoldA uplus f = ufoldG unitCollector uplus (const f)

ufoldAB :: Applicative f => (a -> [a] -> a) -> (BinderList Info -> Node -> f a) -> Node -> f a
ufoldAB uplus f = ufoldG binderInfoCollector uplus f

ufoldAN :: Applicative f => (a -> [a] -> a) -> (Index -> Node -> f a) -> Node -> f a
ufoldAN uplus f = ufoldG binderNumCollector uplus f

walk :: Applicative f => (Node -> f ()) -> Node -> f ()
walk = ufoldA (foldr mappend)

walkN :: Applicative f => (Index -> Node -> f ()) -> Node -> f ()
walkN = ufoldAN (foldr mappend)

walkB :: Applicative f => (BinderList Info -> Node -> f ()) -> Node -> f ()
walkB = ufoldAB (foldr mappend)

ufold :: (a -> [a] -> a) -> (Node -> a) -> Node -> a
ufold uplus f = runIdentity . ufoldA uplus (return . f)

ufoldB :: (a -> [a] -> a) -> (BinderList Info -> Node -> a) -> Node -> a
ufoldB uplus f = runIdentity . ufoldAB uplus (\is -> return . f is)

ufoldN :: (a -> [a] -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f = runIdentity . ufoldAN uplus (\idx -> return . f idx)

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc = run . execState acc . walk (\n' -> modify' (`f` n'))

gatherB :: (BinderList Info -> a -> Node -> a) -> a -> Node -> a
gatherB f acc = run . execState acc . walkB (\is n' -> modify' (\a -> f is a n'))

gatherN :: (Index -> a -> Node -> a) -> a -> Node -> a
gatherN f acc = run . execState acc . walkN (\idx n' -> modify' (\a -> f idx a n'))
