{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AbstractGraph.Impl.Function
  () where

import Data.AbstractGraph.Class

-- Classes
import Control.Comonad
import Data.Profunctor

----------------------------------------
-- Function based implementation of total graphs

newtype FunG n e = G { unG :: n -> n -> e }

lift0 :: e -> FunG n e
lift0 e = G (const (const e))

instance (Eq n) => Graph (FunG n e) where
  type Node (FunG n e) = n
  type Edge (FunG n e) = e

  constant = lift0

  sample = unG

  updateWith f (G g) src dst e = G $
    \n1 n2 ->
      if (n1,n2) == (src,dst)
        then f (g n1 n2) e
        else g n1 n2

instance Semigroup e => Semigroup (FunG n e) where
  f <> g = G $ unG f <> unG g

instance Monoid e => Monoid (FunG n e) where
  mempty = lift0 mempty

instance (Bounded n, Enum n, Eq n) => FinGraph (FunG n e) where
  nodes _ = enumFrom minBound

instance (Bounded n, Enum n, Eq n) => Foldable (FunG n) where
  foldMap f g = foldMap f $ edges g
  foldr f acc g = Prelude.foldr f acc $ edges g

instance Functor (FunG n) where
  fmap f g = G (\n -> f . unG g n)

-- While this may be doable, it is almost certainly not worth it at the moment
-- instance (Bounded n, Enum n, Eq n) => Traversable (FunG n) where
--   traverse f g = G . (\n1 n2 -> f (unG g n1 n2)

instance (Eq n) => Applicative (FunG n) where
  pure = lift0
  f <*> g = G $ \n1 n2 -> unG f n1 n2 (unG g n1 n2)

instance (Eq n) => Monad (FunG n) where
  g >>= k = G $ \n1 n2 -> unG (k (unG g n1 n2)) n1 n2

instance Monoid n => Comonad (FunG n) where
  extract g = unG g mempty mempty
  duplicate g = G $ \o1 o2 -> (G (\i1 i2 -> unG g (mappend o1 i1) (mappend o2 i2)))

instance Profunctor FunG where
  dimap ab cd g = G $ \n1 n2 -> cd (unG g (ab n1) (ab n2))

--instance Bifunctor FunG where
--  bimap :: (a -> b) -> (c -> d) -> FunG a c -> FunG b d
--  first = ????
--  second = fmap
