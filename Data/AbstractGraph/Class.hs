{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AbstractGraph.Class where

-- | This class represents total graphs
class Graph g where
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

-- | This class represents finite graphs
class Graph g => FinGraph g where
  -- | The nodes of the graph
  nodes :: g -> [Node g]

  -- | The edges of the graph
  edges :: g -> [Edge g]
  edges g = sample g <$> ns <*> ns
    where
      ns = nodes g
