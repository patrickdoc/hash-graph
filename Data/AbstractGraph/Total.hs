-- | Total Graphs
--
-- These graphs are total with respect to the node type.
-- Any element of type Node is always in the graph.
-- That is, for n1, n2 :: Node, lookup n1 n2 :: Edge is
-- defined.
module Data.AbstractGraph.Total where

import Data.AbstractGraph.Prim

import Data.Profunctor
import Data.Semigroup
import Prelude hiding (lookup)

-- | Find the edge between two nodes
lookup :: (Graph g n e) => g -> n -> n -> e
lookup = sample

-- | Modify an edge
update :: (Graph g n e, Eq n, Semigroup e) => g -> n -> n -> e -> g
update = updateWith (<>)

-- | Contravariant mapping over nodes
nmap :: (Graph (g m e) m e, Graph (g n e) n e, Profunctor g) => (n -> m) -> g m e -> g n e
nmap = lmap

-- | Map over edges
emap :: (Graph (g n e) n e, Graph (g n f) n f, Functor (g n)) => (e -> f) -> g n e -> g n f
emap = fmap

---------------------------------------------------------
-- Bounded and Enum functions
--
-- These functions in some way require access to every node.
-- Bounded and Enum give us a way to work with a collection of every node.

nodes :: (Graph g n e, Bounded n, Enum n) => g -> [n]
nodes _ = [minBound..]

-- | Number of nodes
order :: (Graph g n e, Bounded n, Enum n) => g -> Int
order = length . nodes

-- | Number of edges
--
-- This is only /= mempty
size :: (Graph g n e, Bounded n, Enum n, Eq e, Monoid e) => g -> Int
size g = length $ filter (/= mempty) $ edges g

-- | List of edges
edges :: (Graph g n e, Bounded n, Enum n) => g -> [e]
edges g = lookup g <$> ns <*> ns
  where
    ns = nodes g

-- | Fold over edges
gfoldr :: (Graph g n e, Bounded n, Enum n) => (e -> b -> b) -> b -> g -> b
gfoldr f acc = foldr f acc . edges

-- | Convert to a list of (Node, Edge, Node)
toList :: (Graph g n e, Bounded n, Enum n) => g -> [(n,e,n)]
toList g = (\n1 n2 -> (n1, lookup g n1 n2, n2)) <$> ns <*> ns
  where
    ns = nodes g
