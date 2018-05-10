{-# LANGUAGE DeriveGeneric #-}

-------------------------------------
-- |
-- Module       : Data.HashGraph.Strict
-- Copyright    : 2017 Patrick Dougherty
-- License      : BSD2
-- Maintainer   : Patrick Dougherty <patrick.doc@ameritech.net>
--
-- A hashing-based graph implementation.

module Data.HashGraph.Strict (
    -- * Graph Type
      Gr(..)

    -- * Construction
    , empty
    , singleton
    , mkGraph

    -- * Basic interface
    , null
    , nodes
    , order
    , edges
    , size
    , (!)
    , (!?)

    -- * Inductive
    , (&)
    , match
    , matchAny

    -- * Maps
    , nmap
    , emap
    , nemapH

    -- * Folds
    , foldr

    -- * Queries
    , member
    , neighbors
    , preds
    , succs
    , inEdges
    , outEdges
    , inDegree
    , outDegree
    , degree
    , hasEdge
    , hasNeighbor

    -- * Filters
    , nfilter
    , efilter

    -- * Insertion and Deletion
    , insNode
    , safeInsNode
    , delNode
    , insEdge
    , delEdge

    -- * Lists
    , toList
    , fromList
    , fromListWith

    -- * Pretty printing
    , pretty

    -- The rest
    , Context'(..)
    , Edge(..)
    , Head(..)
    , Tail(..)
    , insTail
    ) where

import Control.DeepSeq
import Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import GHC.Generics
import Prelude hiding (foldr, null)

-- Graph type
type GraphRep a b = HM.HashMap b (Context' a b)
newtype Gr a b = Gr (GraphRep a b) deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (Gr a b) where
    rnf (Gr hm) = rnf hm

-- | The Edge type and corresponding half edges
data Edge a b = Edge !b !a !b  deriving (Eq, Generic, Show)
data Head a b = Head !a !b deriving (Eq, Generic, Show)
data Tail a b = Tail !a !b deriving (Eq, Generic, Show)

instance (Eq a, Eq b, Ord a) => Ord (Edge a b) where
    compare (Edge _ l1 _) (Edge _ l2 _) = compare l1 l2

instance (Hashable a, Hashable b) => Hashable (Edge a b)
instance (Hashable a, Hashable b) => Hashable (Head a b)
instance (Hashable a, Hashable b) => Hashable (Tail a b)

instance (NFData a, NFData b) => NFData (Edge a b)
instance (NFData a, NFData b) => NFData (Head a b)
instance (NFData a, NFData b) => NFData (Tail a b)

-- | The Context of a node within the Graph type
data Context' a b = Context'
    { heads :: !(HS.HashSet (Head a b)) -- ^ Predecessors of the node
    , tails :: !(HS.HashSet (Tail a b)) -- ^ Successors of the node
    } deriving (Eq, Generic, Show)

instance (NFData a, NFData b) => NFData (Context' a b)

-------------------------------
-- Construction

-- | /O(1)/ Construct an empty graph
empty :: Gr a b
empty = Gr HM.empty

-- | /O(1)/ Construct a graph with a single node
singleton :: (Eq b, Hashable b) => b -> Gr a b
singleton b = Gr $ HM.singleton b (Context' HS.empty HS.empty)

-- TODO: cleanup and determine time complexity
-- | Construct a graph from the given edges and nodes
mkGraph :: (Eq a, Eq b, Hashable a, Hashable b) => [Edge a b] -> [b] -> Gr a b
mkGraph es ns = L.foldl' (flip insEdge) nodeGraph es
  where
    nodeGraph = Gr $ HM.fromList $ map (\x -> (x, Context' HS.empty HS.empty)) ns
{-# INLINE mkGraph #-}

-------------------------------
-- Basic interface

-- | /O(1)/ Return 'True' if this graph is empty, 'False' otherwise
null :: Gr a b -> Bool
null (Gr g) = HM.null g
{-# INLINABLE null #-}

-- | /O(n)/ Return the number of nodes in the graph
order :: Gr a b -> Int
order (Gr g) = HM.size g

-- | /O(n+e)/ Return the number of edges in the graph
size :: Gr a b -> Int
size = L.foldl' (\c (_, Context' ps _) -> c + HS.size ps) 0 . toList

infixl 9 !, !?
-- | /O(log n)/ Return the 'Context' of a node in the graph.
-- Call 'error' if the node is not in the graph.
(!) :: (Eq b, Hashable b) => Gr a b -> b -> Context' a b
(!) g n = fromMaybe (error "Data.Graph.Inductive.Strict.(!): node not found") $ g !? n

-- | /O(log n)/ Return the 'Context' of a node if it is in the graph,
-- or 'Nothing' if it is not.
(!?) :: (Eq b, Hashable b) => Gr a b -> b -> Maybe (Context' a b)
(!?) (Gr graph) n = HM.lookup n graph
{-# INLINE (!?) #-}

-- | /O(n)/ Return a list of the nodes in the graph.
-- The list is produced lazily
nodes :: Gr a b -> [b]
nodes (Gr g) = HM.keys g

-- | /O(n)/ Return a list of the contexts in the graph.
-- The list is produced lazily
contexts :: Gr a b -> [Context' a b]
contexts (Gr g) = HM.elems g

-- TODO: Determine time complexity
-- | /O(?)/ Return a list of the edges in the graph
edges :: Gr a b -> [Edge a b]
edges (Gr hm) = HM.foldlWithKey' (\lst p ctx -> getTails p ctx ++ lst) [] hm
  where
    getTails p (Context' _ ss) = HS.foldl' (\lst (Tail l s) -> Edge p l s : lst) [] ss

--------------------------------------
-- Inductive

-- | Extract a node from the graph
match :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> Maybe (Context' a b, Gr a b)
match n g = g !? n >>= \ctx -> Just (ctx, delCtx n ctx g)
{-# INLINE match #-}

-- | Extract any node from the graph
matchAny :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> Maybe ((b,Context' a b), Gr a b)
matchAny g = L.uncons (toList g) >>= \((l,ctx),_) -> Just ((l,ctx), delCtx l ctx g)
{-# INLINE matchAny #-}

infixr 9 &
-- TODO: Figure out how this should be implemented
-- | Merge the 'Context' into the graph
-- Currently deletes old node if present
(&) :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Context' a b -> Gr a b -> Gr a b
(&) l ctx (Gr g) = Gr $ HM.insert l ctx g

--------------------------------------
-- Maps

-- TODO: Clarify collisions after mapping
-- TODO: Consider cmap for contexts

-- | Map /f/ over the nodes.
nmap :: (Eq a, Eq c, Hashable a, Hashable c) => (b -> c) -> Gr a b -> Gr a c
nmap = nemapH id

-- | Map /f/ over the edges.
emap :: (Eq b, Eq c, Hashable b, Hashable c) => (a -> c) -> Gr a b -> Gr c b
emap fe (Gr g) = Gr $ HM.map go g
  where
    go (Context' ps ss) = Context' (goHead ps) (goTail ss)
    goHead = HS.map (\(Head l p) -> Head (fe l) p)
    goTail = HS.map (\(Tail l s) -> Tail (fe l) s)

-- HashMap based
-- | Map /fe/ over the edges and /fn/ over the nodes.
nemapH :: (Eq c, Eq d, Hashable c, Hashable d) => (a -> c) -> (b -> d) -> Gr a b -> Gr c d
nemapH fe fn g = fromList $ map go $ toList g
  where
    go (n, Context' ps ss) = (fn n, Context' (goHead ps) (goTail ss))
    goHead = HS.map (\(Head l p) -> Head (fe l) (fn p))
    goTail = HS.map (\(Tail l s) -> Tail (fe l) (fn s))

-- Inductive based
-- | Map /fe/ over the edges and /fn/ over the nodes.
{-
nemapI :: (a -> c) -> (b -> d) -> Gr a b -> Gr c d nemapI fe fn g = case matchAny g of
    Just (Context' ps l ss, g') -> Context' (goHead ps) (fn l) (goTail ss) & nemapI fe fn g'
    Nothing -> empty
  where
    goHead = HS.map (\(Head l p) -> Head (fe l) (fn p))
    goTail = HS.map (\(Tail l s) -> Tail (fe l) (fn s))
-}

---------------------------------------
-- Folds

instance Foldable (Gr a) where
    foldr = foldr

-- | HashMap based
foldr :: (b -> c -> c) -> c -> Gr a b -> c
foldr f x g = L.foldr (\(n, _) c -> f n c) x $ toList g

------------------------------------
-- Queries

-- | /O(log n)/ Return 'True' if the given node is in the graph, 'False' otherwise.
member :: (Eq b, Hashable b) => b -> Gr a b -> Bool
member n (Gr g) = HM.member n g

-- | /O(?)/ Return a list of the neighbors of the given node.
neighbors :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [b]
neighbors n g = g !? n >>= \(Context' ps ss) ->
        let hds = HS.foldl' (\hs (Head _ p) -> HS.insert p hs) HS.empty ps
        in Just $ HS.toList $ HS.foldl' (\hs (Tail _ s) -> HS.insert s hs) hds ss

-- | /O(?)/ Return a list of the predecessors of the given node.
preds :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [b]
preds n g = HS.foldl' (\ls (Head _ p) -> p : ls) [] . heads <$> g !? n

-- | /O(?)/ Return a list of the successors of the given node.
succs :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [b]
succs n g = HS.foldl' (\ls (Tail _ s) -> s : ls) [] . tails <$> g !? n

-- | /O(?)/ Return a list of the incoming edges to the node.
inEdges :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [Edge a b]
inEdges n g = HS.foldl' (\ls (Head l p) -> Edge p l n : ls) [] . heads <$> g !? n

-- | /O(?)/ Return a list of the outgoing edges from the node.
outEdges :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [Edge a b]
outEdges n g = HS.foldl' (\ls (Tail l s) -> Edge n l s : ls) [] . tails <$> g !? n

-- | /O(?)/ Return the number of incoming edges to the node.
inDegree :: (Eq b, Hashable b) => b -> Gr a b -> Maybe Int
inDegree b g = length <$> inEdges b g

-- | /O(?)/ Return the number of outgoing edges from a node.
outDegree :: (Eq b, Hashable b) => b -> Gr a b -> Maybe Int
outDegree b g = length <$> outEdges b g

-- | /O(?)/ Return the number of edges touching this node
degree :: (Eq b, Hashable b) => b -> Gr a b -> Maybe Int
degree n g = (+) <$> inDegree n g <*> outDegree n g

-- | /O(?)/ Return 'True' if the graph contains the given edge, 'False' otherwise.
hasEdge :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> Gr a b -> Bool
hasEdge (Edge p l s) g = case g !? p of
    Just (Context' _ ss) -> HS.member (Tail l s) ss
    Nothing -> False

-- | TODO: FIX, should be true if undirected edge between nodes
-- | /O(?)/ Return 'True' if the given nodes are neighbors, 'False' otherwise.
hasNeighbor :: (Eq b, Hashable b) => b -> b -> Gr a b -> Bool
hasNeighbor n1 n2 g = case g !? n1 of
    Just (Context' _ ss) -> HS.foldl' (\t (Tail _ p) -> t || p == n2) False ss
    Nothing -> False

-----------------------------
-- Filters

{-
 - filter:
 - - contexts
 - - create subgraph from specific nodes?
 - ^ could just be nfilter (`elem` [Node])
 -}

-- | /O(n+e)/ Filter this graph by retaining only
-- nodes that satisfy the predicate 'f'.
nfilter :: (b -> Bool) -> Gr a b -> Gr a b
nfilter f (Gr g) = Gr $ HM.mapMaybeWithKey go g
  where
    go n (Context' ps ss) = if f n
        then Just (Context' (HS.filter (\(Head _ p) -> f p) ps)
                            (HS.filter (\(Tail _ s) -> f s) ss))
        else Nothing

-- | /O(n+e)/ Filter this graph by retaining only
-- edges that satisfy the predicate 'f'.
efilter :: (Edge a b -> Bool) -> Gr a b -> Gr a b
efilter f (Gr g) = Gr $ HM.mapWithKey go g
  where
    go n (Context' ps ss) = Context' (HS.filter (\(Head l p) -> f (Edge p l n)) ps)
                                     (HS.filter (\(Tail l s) -> f (Edge n l s)) ss)

------------------------------
--  Insertion and Deletion

-- | Insert a node, deleting the current context if the node exists
insNode :: (Eq b, Hashable b) => b -> Gr a b -> Gr a b
insNode n (Gr g) = Gr $ HM.insert n (Context' HS.empty HS.empty) g

-- | Insert a node only if it does not already exist in the graph
safeInsNode :: (Eq b, Hashable b) => b -> Gr a b -> Gr a b
safeInsNode n g
  = if n `member` g
      then g
      else insNode n g

-- | Insert a node, using the combining function if it already exists
insNodeWith :: (Eq b, Hashable b) => (Context' a b -> Context' a b -> Context' a b) -> b -> Context' a b -> Gr a b -> Gr a b
insNodeWith f n c (Gr g) = Gr $ HM.insertWith f n c g

-- | Remove a node and its edges
delNode :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> Gr a b
delNode n g = case g !? n of
                Just ctx -> delCtx n ctx g
                Nothing -> g
{-# INLINABLE delNode #-}

-- | Remove a context
delCtx :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Context' a b -> Gr a b -> Gr a b
delCtx n ctx (Gr graph) = Gr $ delHeads n ctx $ delTails n ctx $ HM.delete n graph

-- TODO: Currently unsafe? check how adjust works
insEdge :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> Gr a b -> Gr a b
insEdge e (Gr g) = Gr $ insTail e (insHead e g)
{-# INLINABLE insEdge #-}

-- | Remove an edge
delEdge :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> Gr a b -> Gr a b
delEdge e (Gr g) = Gr $ delTail e (delHead e g)
{-# INLINABLE delEdge #-}

-- Insertion and Deletion Internals

-- | Insert a head into the graph
insHead :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insHead (Edge p l s) = HM.adjust go s
  where
    go (Context' ps _ss) = Context' (HS.insert (Head l p) ps) _ss
{-# INLINABLE insHead #-}

-- | Remove a head from the graph
delHead :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
delHead (Edge p l s) = HM.adjust go s
  where
    go (Context' ps _ss) = Context' (HS.delete (Head l p) ps) _ss
{-# INLINABLE delHead #-}

-- | Remove the head ends of tails attached to the node
delHeads :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Context' a b -> GraphRep a b -> GraphRep a b
delHeads p (Context' _ ss) g = HS.foldl' go g ss
  where
    go hm (Tail l s) = delHead (Edge p l s) hm
{-# INLINABLE delHeads #-}

-- | Insert a tail into the graph
insTail :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insTail (Edge p l s) = HM.adjust go p
  where
    go (Context' _ps ss) = Context' _ps $ HS.insert (Tail l s) ss
{-# INLINABLE insTail #-}

-- | Remove a tail from the graph
delTail :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
delTail (Edge p l s) = HM.adjust go p
  where
    go (Context' _ps ss) = Context' _ps $ HS.delete (Tail l s) ss
{-# INLINABLE delTail #-}

-- | Remove the head ends of tails attached to the node
delTails :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Context' a b -> GraphRep a b -> GraphRep a b
delTails s (Context' ps _) g = HS.foldl' go g ps
  where
    go hm (Head l p) = delTail (Edge p l s) hm
{-# INLINABLE delTails #-}

-----------------------------------------
-- Equality?

----------------------------------------
-- Lists

-- | /O(n)/ Return a list of this graph's elements. The list is
-- produced lazily. The order of its elements is unspecified.
toList :: Gr a b -> [(b, Context' a b)]
toList (Gr g) = HM.toList g

-- | /O(n)/ Construct a graph with the supplied structure. If the
-- list contains duplicate nodes, the later edges take precedence.
fromList :: (Eq b, Hashable b) => [(b, Context' a b)] -> Gr a b
fromList = Gr . HM.fromList
{-# INLINE fromList #-}

-- | /O(n*log n)/ Construct a graph with the supplied structure. Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq b, Hashable b) => (Context' a b -> Context' a b -> Context' a b) -> [(b, Context' a b)] -> Gr a b
fromListWith f = Gr . HM.fromListWith f

------------------------------------
-- Pretty Printing

-- | Pretty-print the graph
pretty :: (Show a, Show b) => Gr a b -> String
pretty (Gr g)
  = HM.foldlWithKey' (\str n (Context' ps ss) -> show (HS.toList ps) ++ " -> "
                                         ++ show n
                                         ++ " -> " ++ show (HS.toList ss)
                                         ++ "\n" ++ str) [] g
