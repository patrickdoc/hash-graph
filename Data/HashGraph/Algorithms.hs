{-# LANGUAGE BangPatterns #-}
module Data.HashGraph.Algorithms (
    -- * Traversals
    bfs
  , bfsn
  , dfs
  , dfsn
  , prim
  , primAt
  , topSort

  -- * Paths?
  , pathTree
  ) where

import Data.List (foldl')
import Data.HashGraph.Strict
import Data.HashGraph.Algorithms.MST (prim, primAt)
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, maybe)

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
bfs g = maybe [] (\(ctx,_) -> snd $ bfs_ g HS.empty [ctx]) $ matchAny g
{-# INLINABLE bfs #-}

bfsn :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> [b]
bfsn n g = maybe [] (\(ctx,_) -> snd $ bfs_ g HS.empty [ctx]) $ match n g
{-# INLINABLE bfsn #-}

bfs_ :: (Eq b, Hashable b) => Gr a b -> HS.HashSet b -> [Context a b] -> (HS.HashSet b, [b])
bfs_ _ set [] = (set, [])
bfs_ g !set cs =
    (\(newSet, parents, kids) ->
      (\(resSet, resNodes) -> (resSet, parents ++ resNodes)) (bfs_ g newSet kids)) $ foldl' helper (set,[],[]) cs
  where
    helper (hs,ps,ctxs) (n,Context' _ ss) = if HS.member n hs
        then (hs,ps,ctxs)
        else (HS.insert n hs, n:ps, map (\(Tail _ s) -> (s,g!s)) (HS.toList ss) ++ ctxs)
{-# INLINABLE bfs_ #-}

-----------------------------------
-- Depth-first Search

-- | Depth-first search
dfs :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> [b]
dfs g = maybe [] (snd . dfs_ g HS.empty . fst) $ matchAny g
{-# INLINABLE dfs #-}

dfsn :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> [b]
dfsn n g = maybe [] (snd . dfs_ g HS.empty . fst) $ match n g
{-# INLINABLE dfsn #-}

dfs_ :: (Eq b, Hashable b) => Gr a b -> HS.HashSet b -> Context a b -> (HS.HashSet b, [b])
dfs_ g !set (n,Context' _ ss)
  = if HS.member n set
       then (set,[])
       else let (newSet, lst) = HS.foldl'
                                  (\(hs,ls) (Tail _ s) -> (\(dSet,dNodes) -> (dSet, dNodes ++ ls)) (dfs_ g hs (s,g!s)))
                                  (HS.insert n set,[])
                                  ss
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

-----------------------------------
-- Topological sorts

topSort :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> Maybe [b]
topSort g = go open
  where
    open = HS.fromList $ nodes g
    -- while there are unmarked nodes, visit them
    go o = snd $ HS.foldl' (\(unmarked, list) n -> visit n unmarked HS.empty list) (o, Just []) o
      where
    -- check for marks, then visit children, mark n, and add to list
    visit _ o _ Nothing = (o, Nothing)
    visit n o t l
        | not (HS.member n o) = (o, l)
        | HS.member n t = (o, Nothing)
        | otherwise = (HS.delete n newO, fmap (n :) newL)
      where
        -- visit all children
        (newO, newL) = foldl' (\(o',l') node -> visit node o' (HS.insert n t) l') (o,l) (fromJust $ succs n g)
