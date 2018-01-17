{-# LANGUAGE BangPatterns #-}
module Data.HashGraph.Algorithms (
    -- * Traversals
    bfs
  , bfsn
  , dfs
  , dfsn
    ) where

import Data.List (foldl')
import Data.HashGraph.Strict
import Data.Hashable
import qualified Data.HashSet as HS

{-
 - Articulation Point
 - Bi-connected components of an undirected graph
 - Dominators
 - Graph Voronoi Diagram
 - Maximum Independent Node Sets
 - Minimum Spanning Tree
 - Max Flow
 - Shortest Path
 - Transitive and or Reflective Closure
 -}

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
