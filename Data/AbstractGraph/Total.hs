{-# LANGUAGE FlexibleContexts #-}
-- | Total Graphs
--
-- These graphs are total with respect to the node type.
-- Any element of type Node is always in the graph.
-- That is, for n1, n2 :: Node, lookup n1 n2 :: Edge is
-- defined.
module Data.AbstractGraph.Total where

import Data.AbstractGraph.Class

import Data.Profunctor
import Data.Semigroup
import Prelude hiding (lookup)

-- | Find the edge between two nodes
lookup :: (Graph g) => g -> Node g -> Node g -> Edge g
lookup = sample

-- | Modify an edge
update :: (Graph g, Semigroup (Edge g)) => g -> Node g -> Node g -> Edge g -> g
update = updateWith (<>)

-- | Contravariant mapping over nodes
nmap :: (Graph (g m e), Graph (g n e), Profunctor g) => (n -> m) -> g m e -> g n e
nmap = lmap

-- | Map over edges
emap :: (Graph (gn e), Graph (gn f), Functor gn) => (e -> f) -> gn e -> gn f
emap = fmap

---------------------------------------------------------
-- Bounded and Enum functions
--
-- These functions in some way require access to every node.
-- Bounded and Enum give us a way to work with a collection of every node.

-- | Number of nodes
order :: (FinGraph g) => g -> Int
order = length . nodes

-- | Number of edges
--
-- This is only /= mempty
size :: (FinGraph g, Eq (Edge g), Monoid (Edge g)) => g -> Int
size g = length $ filter (/= mempty) $ edges g

-- | Fold over edges
gfoldr :: (FinGraph g) => (Edge g -> b -> b) -> b -> g -> b
gfoldr f acc = foldr f acc . edges

-- | Convert to a list of (Node, Edge, Node)
toList :: (FinGraph g) => g -> [(Node g, Edge g, Node g)]
toList g = (\n1 n2 -> (n1, lookup g n1 n2, n2)) <$> ns <*> ns
  where
    ns = nodes g
