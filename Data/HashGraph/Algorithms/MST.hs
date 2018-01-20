{-# LANGUAGE BangPatterns #-}

module Data.HashGraph.Algorithms.MST (
    prim
  , primAt
  , kruskal
  ) where

import Data.HashGraph.Strict
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')

-- | Prim's algorithm for the minimum spanning tree
--
-- Implemented with a with a hash set of visited nodes and a hash map of
-- optimal edges

prim :: (Eq b, Hashable a, Hashable b, Ord a) => Gr a b -> Gr a b
prim g = case matchAny g of
    Just (ctx, _) -> prim' g ctx
    Nothing -> g

primAt :: (Eq b, Hashable a, Hashable b, Ord a) => b -> Gr a b -> Gr a b
primAt start g = case g !? start of
    Just ctx -> prim' g ctx
    Nothing -> g

prim' :: (Eq b, Hashable a, Hashable b, Ord a) => Gr a b -> Context' a b -> Gr a b
prim' g (Context' _ node sucs) = fixTails $ go HS.empty HM.empty (Context' HS.empty node sucs)
  where
    -- Context has new ps (just the edge that was used to include this node) and old ss
    go !visited !optimal (Context' ps n ss) =
        let newVisited = HS.insert n visited
        in case pickNextNode (addTails n newVisited optimal ss) of
            Just (newOpt, newCtx) -> (n, Context' ps n HS.empty) : go newVisited newOpt newCtx
            Nothing -> [(n, Context' ps n HS.empty)]
    -- remove picked node from optimal and prep it for next round
    pickNextNode opt
        = (\(s,hd) -> (HM.delete s opt, Context' (HS.singleton hd) s (tails (g ! s)))) <$> minimumByWeight opt

-- Pick out the least weight edge from the optimal map
minimumByWeight :: (Ord a) => HM.HashMap b (Head a b) -> Maybe (b, Head a b)
minimumByWeight
    = HM.foldlWithKey' (\mh k hd ->
        case mh of
            Just e -> Just $ minEdge (k,hd) e
            Nothing -> Just (k,hd)) Nothing

-- Add all new edges to the optimal map, keeping only better ones
addTails :: (Eq b, Hashable b, Ord a) => b                          -- New node
                                      -> HS.HashSet b               -- Visited nodes
                                      -> HM.HashMap b (Head a b)    -- Optimal edges
                                      -> HS.HashSet (Tail a b)      -- New edges
                                      -> HM.HashMap b (Head a b)    -- New optimal edges
addTails n visited = HS.foldl' checkedInsert
  where
    checkedInsert hm (Tail l s)
        = if HS.member s visited
            then hm
            else HM.insertWith minHead s (Head l n) hm

-- The generated list does not have any tails, match all the heads with tails
fixTails :: (Eq a, Eq b, Hashable a, Hashable b) => [(b, Context' a b)] -> Gr a b
fixTails ls
    = let es = foldl' (\es' (s, Context' ps _ _) -> map (\(Head l p) -> Edge p l s) (HS.toList ps) ++ es') [] ls
      in Gr $ foldl' (flip insTail) (HM.fromList ls) es

minEdge :: Ord a => (b, Head a b) -> (b, Head a b) -> (b, Head a b)
minEdge e1@(_, Head l1 _) e2@(_, Head l2 _) = if l1 < l2 then e1 else e2

minHead :: Ord a => Head a b -> Head a b -> Head a b
minHead hd1@(Head l1 _) hd2@(Head l2 _) = if l1 < l2 then hd1 else hd2

-- | Kruskal's algorithm for the minimum spanning tree
kruskal :: Gr a b -> Gr a b
kruskal g = undefined
