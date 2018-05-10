{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AbstractGraph.Class where

import Data.Profunctor

-- | This class represents total graphs
class TGraph g where
  type Node g :: *
  type Edge g :: *
  -- | The graph where every pair of nodes has the given edge between them
  constant      :: Edge g -> g

  -- | Find the edge between a pair of nodes in the graph
  sample        :: g -> Node g -> Node g -> Edge g

  -- | Update an edge using the provided function.
  --
  -- Note: For noncommutative update functions,
  -- the edges are passed as @f old new@.
  updateWith    :: (Edge g -> Edge g -> Edge g) -> g -> Node g -> Node g -> Edge g -> g

-- | Modify an edge
update :: (TGraph g, Semigroup (Edge g)) => g -> Node g -> Node g -> Edge g -> g
update = updateWith (<>)

-- | Contravariant mapping over nodes
nmap :: (TGraph (g n1 e), TGraph (g n2 e), Profunctor g) => (n2 -> n1) -> g n1 e -> g n2 e
nmap = lmap

-- | Map over edges
emap :: (TGraph (gn e1), TGraph (gn e2), Functor gn) => (e1 -> e2) -> gn e1 -> gn e2
emap = fmap

-- | List all nodes of type 'Node g'
nodes :: (TGraph g, Bounded (Node g), Enum (Node g)) => g -> [Node g]
nodes _ = enumFrom minBound

-- | Count nodes
order :: forall g. (TGraph g, Bounded (Node g), Enum (Node g)) => g -> Int
order _ = fromEnum (maxBound :: Node g)

-- | List all of the edges
edges :: (TGraph g, Bounded (Node g), Enum (Node g)) => g -> [Edge g]
edges g = sample g <$> ns <*> ns
  where
    ns = nodes g

-- | Count edges
size :: forall g. (TGraph g, Bounded (Node g), Enum (Node g)) => g -> Int
size _ = (fromEnum (maxBound :: Node g))^(2 :: Int)

-- | List all '(Node, Edge, Node)' triplets in the graph
toList :: (TGraph g, Bounded (Node g), Enum (Node g)) => g -> [(Node g, Edge g, Node g)]
toList g = (\n1 n2 -> (n1, sample g n1 n2, n2)) <$> ns <*> ns
  where
    ns = nodes g
