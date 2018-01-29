{-# LANGUAGE BangPatterns #-}
module Data.HashGraph.Algorithms (
    -- * Traversals
    bfs
  , bfsn
  , dfs
  , dfsn
  , prim
  , primAt

  -- * Paths?
  , pathTree
  ) where

import Data.List (foldl')
import Data.HashGraph.Strict
import Data.HashGraph.Algorithms.MST (prim, primAt)
import Data.Hashable
import qualified Data.HashSet as HS

{-
 - Articulation Point
 - Bi-connected components of an undirected graph
 - Dominators
 - Graph Voronoi Diagram
 - Maximum Independent Node Sets
 - Max Flow
 - Shortest Path
 - Transitive and or Reflective Closure
 -}

-----------------------------------
-- Breadth-first Search

-- | Breadth-first search
bfs :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> [b]
bfs g = case matchAny g of
    Just (ctx,_) -> snd $ bfs_ g HS.empty [ctx]
    Nothing -> []
{-# INLINABLE bfs #-}

bfsn :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> [b]
bfsn n g = case match n g of
    Just (ctx,_) -> snd $ bfs_ g HS.empty [ctx]
    Nothing -> []
{-# INLINABLE bfsn #-}

bfs_ :: (Eq b, Hashable b) => Gr a b -> HS.HashSet b -> [Context' a b] -> (HS.HashSet b, [b])
bfs_ _ set [] = (set, [])
bfs_ g initialSet initialContext = go initialSet initialContext
  where
    go set [] = (set, [])
    go !set cs =
        (\(newSet, parents, kids) ->
            (\(resSet, resNodes) -> (resSet, parents ++ resNodes)) (go newSet kids)) $ foldl' helper (set,[],[]) cs
    helper (hs,ps,ctxs) (Context' _ n ss) = if HS.member n hs
        then (hs,ps,ctxs)
        else (HS.insert n hs, n:ps, map (\(Tail _ s) -> g!s) (HS.toList ss) ++ ctxs)
{-# INLINABLE bfs_ #-}

-----------------------------------
-- Depth-first Search

-- | Depth-first search
dfs :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> [b]
dfs g = case matchAny g of
    Just (ctx,_) -> snd $ dfs_ g HS.empty ctx
    Nothing -> []
{-# INLINABLE dfs #-}

dfsn :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> [b]
dfsn n g = case match n g of
    Just (ctx,_) -> snd $ dfs_ g HS.empty ctx
    Nothing -> []
{-# INLINABLE dfsn #-}

dfs_ :: (Eq b, Hashable b) => Gr a b -> HS.HashSet b -> Context' a b -> (HS.HashSet b, [b])
dfs_ g = go
  where
    go !set (Context' _ n ss) = if HS.member n set
        then (set,[])
        else let (newSet, lst) = HS.foldl' (\(hs,ls) (Tail _ s) -> (\(dSet,dNodes) -> (dSet, dNodes ++ ls)) (go hs (g!s)))
                                       (HS.insert n set,[]) ss
             in (newSet, n : lst)
{-# INLINABLE dfs_ #-}

-----------------------------------
-- pathTree from Graphalyze

-- | Return all paths starting at the given node
-- WARNING: Infinite loop if the graph contains a loop (an edge from a node to itself)
pathTree :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> [[Edge a b]]
pathTree n g = case match n g of
    Just (_, g') -> case outEdges n g of
        Just es -> if Prelude.null es
            then [[]]
            else concatMap (\e@(Edge _ _ s) -> map (e:) (pathTree s g')) es
        Nothing -> [[]]
    Nothing -> []
