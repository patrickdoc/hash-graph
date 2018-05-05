{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AbstractGraph.Prim where

-- HashG
import Data.Hashable
import Data.HashMap.Strict

-- | This class represents total graphs
class Graph g n e | g -> n e where
  -- | The graph where every pair of nodes has the given edge between them
  constant      :: e -> g
  -- | Find the edge between a pair of nodes in the graph
  sample        :: g -> n -> n -> e
  -- | Update an edge using the provided function.
  --
  -- Note: For noncommutative update functions,
  -- the edges are passed as @f old new@.
  updateWith    :: (e -> e -> e) -> g -> n -> n -> e -> g

----------------------------------------
-- HashMap based implementation of total graphs

data HashG n e = HG
  { unHG :: HashMap n (HashMap n e)
  , defaultVal :: e }

instance (Eq n, Hashable n) => Graph (HashG n e) n e where

  constant = HG empty

  -- This is bad
  sample (HG g d) = \n1 n2 -> lookupDefault d n2 (lookupDefault empty n1 g)

  updateWith f (HG g d) src dst e = HG (adjust (adjust (f e) dst) src g) d

----------------------------------------
-- Function based implementation of total graphs

newtype FunG n e = G { unG :: n -> n -> e } deriving (Functor)

instance (Eq n) => Graph (FunG n e) n e where

  constant e = G (const (const e))

  sample (G g) = g

  updateWith f (G g) src dst e = G $
    \n1 n2 ->
      if (n1,n2) == (src,dst)
        then f (g n1 n2) e
        else g n1 n2

-- instance Semigroup e => Semigroup (FunG n e) where
--   (G f) <> (G g) = G $ f <> g
-- 
-- instance (Eq n, Monoid e) => Monoid (FunG n e) where
--   mempty = constant mempty
-- 
-- instance Functor (FunG n) where
--   fmap f (G g) = G $ fmap (fmap f) g
--   derived:
--   fmap f (G a) = G (\n2 -> (\n4 -> f (a n2 n4)))
--   fmap f (G a) = G (\n2 -> f . a n2)
-- 
-- instance (Eq n) => Applicative (FunG n) where
--   pure = constant
--   (G f) <*> (G g) = G $ \n1 n2 -> f n1 n2 (g n1 n2)
-- 
-- instance (Eq n) => Monad (FunG n) where
--   (G g) >>= k = G $ \n1 n2 -> sample (k (g n1 n2)) n1 n2

-- contramap :: (n -> m) -> Graph m e -> Graph n e
-- contramap f g =
--   \n1 n2 ->
--     g (f n1) (f n2)

-- | This might need to be given depending on implementation??
-- comap :: (m -> n) -> Graph m e -> Graph n e
-- comap f g = g

-- | Create a graph from a list
-- fromList :: [(n,e,n)] -> Graph n e
