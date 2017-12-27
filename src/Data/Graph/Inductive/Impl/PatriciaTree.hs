{-# LANGUAGE BangPatterns #-}

module Data.Graph.Inductive.Impl.PatriciaTree where

import Control.DeepSeq
import Data.Foldable (foldlM, foldrM)
import Data.Hashable
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | The Graph implementation type
type GraphRep a b = HM.HashMap b (Context' a b)
newtype Gr a b = Gr (GraphRep a b) deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (Gr a b) where
    rnf (Gr hm) = rnf hm

-- | The Edge type and corresponding half edges
data Edge a b = Edge { pred :: b, label :: a, succ :: b } deriving (Eq, Show)
data Head a b = Head !a !b deriving (Eq, Show)
data SmallHead = SmallHead  !Char !Int deriving (Eq, Show)
data Tail a b = Tail a b deriving (Eq, Show)

instance (Hashable a, Hashable b) => Hashable (Head a b) where
    hashWithSalt p (Head a b) = hashWithSalt p a `hashWithSalt` b

instance (NFData a, NFData b) => NFData (Head a b) where
    rnf (Head a b) = rnf a `seq` rnf b

instance NFData (SmallHead) where
    rnf (SmallHead a b) = rnf a `seq` rnf b

instance (Hashable a, Hashable b) => Hashable (Tail a b) where
    hashWithSalt p (Tail a b) = hashWithSalt p a `hashWithSalt` b

instance (NFData a, NFData b) => NFData (Tail a b) where
    rnf (Tail a b) = rnf a `seq` rnf b

{-
newtype Head a b = Head (a, b) deriving (Eq, Show)
newtype Tail a b = Tail (a, b) deriving (Eq, Show)

instance (Hashable a, Hashable b) => Hashable (Head a b) where
    hashWithSalt p (Head x) = hashWithSalt p x

instance (NFData a, NFData b) => NFData (Head a b) where
    rnf (Head x) = rnf x

instance (Hashable a, Hashable b) => Hashable (Tail a b) where
    hashWithSalt p (Tail x) = hashWithSalt p x

instance (NFData a, NFData b) => NFData (Tail a b) where
    rnf (Tail x) = rnf x
-}

-- | The Context of a node within the Graph type
data Context' a b = Context' { preds :: !(HS.HashSet (Head a b))
                             , self  :: !b
                             , succs :: !(HS.HashSet (Tail a b)) }
    deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (Context' a b) where
    rnf (Context' ps b ss) = rnf ps `seq` rnf b `seq` rnf ss

-- | Extract the Head half of a directed edge
edgeHead :: Edge a b -> Head a b
edgeHead (Edge p l _) = Head l p
--edgeHead (Edge p l _) = Head (l, p)

-- | Extract the Tail half of a directed edge
edgeTail :: Edge a b -> Tail a b
edgeTail (Edge _ l s) = Tail l s
--edgeTail (Edge _ l s) = Tail (l, s)

-------------------------------
-- Graph class functions

empty :: Gr a b
empty = Gr HM.empty

isEmpty :: Gr a b -> Bool
isEmpty (Gr g) = HM.null g

-- | Extract a node from the graph
match :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> Maybe (Context' a b, Gr a b)
match n (Gr graph) = case HM.lookup n graph of
    Just ctx -> let newGraph = delNode n graph
                in Just (ctx, Gr newGraph)
    Nothing -> Nothing

-- TODO: cleanup
-- foldl' :: (a -> b-> a) a -> [b] -> a
mkGraph :: (Eq a, Eq b, Hashable a, Hashable b, NFData a, NFData b) => [Edge a b] -> [b] -> Gr a b
mkGraph es ns = Gr $ foldl' (flip (force . insEdge)) nodeGraph es
  where
    nodeGraph = force $ HM.fromList $ map (\x -> (x, Context' HS.empty x HS.empty)) ns

{-
mkGraph :: (Eq a, Eq b, Hashable a, Hashable b) => [Edge a b] -> [b] -> Maybe (Gr a b)
mkGraph es ns = Just $ Gr $ withBoth
  where
    groupAndModHead _ [] = []
    groupAndModHead eq (x@(Edge p l s):xs) = (s, Head (l, p) : map edgeHead ys) : groupAndModHead eq zs
                                           where (ys, zs) = span (eq x) xs
    groupAndModTail _ [] = []
    groupAndModTail eq (x@(Edge p l s):xs) = (p, Tail (l, s) : map edgeTail ys) : groupAndModTail eq zs
                                           where (ys, zs) = span (eq x) xs
    heads = groupAndModHead (\(Edge _ _ x) (Edge _ _ y) -> x == y) es
    tails = groupAndModTail (\(Edge x _ _) (Edge y _ _) -> x == y) es
    tailMap = HM.fromList $ map (\(x, ls) -> (x, Context' HS.empty x (HS.fromList ls))) tails
    headMap = HM.fromList $ map (\(x, ls) -> (x, Context' (HS.fromList ls) x HS.empty)) heads
    nds = HM.fromList [ (n, Context' HS.empty n HS.empty) | n <- ns ]
    withTails = HM.unionWith (\x _ -> x) tailMap nds
    withBoth = HM.unionWith (\(Context' hs x _) (Context' _ _ ts) -> Context' hs x ts) headMap withTails
-}

-- | A list of the nodes in the graph
nodes :: Gr a b -> [b]
nodes (Gr g) = [ node | (node, _) <- HM.toList g ]

-- | The number of nodes in the graph
order :: Gr a b -> Int
order (Gr g) = HM.size g

-- | A list of the edges in the graph
edges :: Gr a b -> [Edge a b]
edges (Gr hm) = HM.foldl' (\lst ctx -> getTails ctx ++ lst) [] hm
  where
    getTails (Context' _ p ss) = HS.foldl' (\lst (Tail l s) -> Edge p l s : lst) [] ss
    --getTails (Context' _ p ss) = HS.foldl' (\lst (Tail (l, s)) -> Edge p l s : lst) [] ss

size :: Gr a b -> Int
size = length . edges

------------------------------
-- Utility functions
-- TODO: Currently unsafe? check how adjust works
insEdge :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insEdge e hm = insTail e (insHead e hm)

-- | Insert a node, deleting the current context if the node exists
insNode :: (Eq b, Hashable b) => b -> GraphRep a b -> GraphRep a b
insNode n = HM.insert n (Context' HS.empty n HS.empty)

-- | Insert a node only if it does not already exist in the graph
safeInsNode :: (Eq b, Hashable b) => b -> GraphRep a b -> GraphRep a b
safeInsNode n g = case HM.lookup n g of
    Just _ -> g
    Nothing -> HM.insert n (Context' HS.empty n HS.empty) g

-- | Remove a node and its edges
delNode :: (Eq a, Eq b, Hashable a, Hashable b) => b -> GraphRep a b -> GraphRep a b
delNode n hm = case HM.lookup n hm of
    Just ctx -> HM.delete n $ delHeads ctx $ delTails ctx hm
    Nothing -> hm

-- | Remove the head ends of tails attached to the node
delHeads :: (Eq a, Eq b, Hashable a, Hashable b) => Context' a b -> GraphRep a b -> GraphRep a b
delHeads (Context' _ b ss) hm = HS.foldl' go hm ss
  where
    go h (Tail l s) = delHead (Head l b) s h
    --go h (Tail (l, s)) = delHead (Head (l, b)) s h

-- | Remove the tail ends of heads attached to the node
delTails :: (Eq a, Eq b, Hashable a, Hashable b) => Context' a b -> GraphRep a b -> GraphRep a b
delTails (Context' ps b _) hm = HS.foldl' go hm ps
  where
    go h (Head l p) = delTail (Tail l b) p h
    --go h (Head (l, p)) = delTail (Tail (l, b)) p h

-- | Insert a head into the graph
insHead :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insHead (Edge p l s) = HM.adjust go s
  where
    go !(Context' ps _l _ss) = Context' (HS.insert (Head l p) ps) _l _ss

-- | Remove a head from the graph
delHead :: (Eq a, Eq b, Hashable a, Hashable b) => Head a b -> b -> GraphRep a b -> GraphRep a b
delHead he = HM.adjust go
  where
    go (Context' ps _l _ss) = Context' (HS.delete he ps) _l _ss

-- | Insert a tail into the graph
insTail :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insTail (Edge p l s) = HM.adjust go p
  where
    go (Context' _ps _l ss) = Context' _ps _l (HS.insert (Tail l s) ss)

-- | Remove a tail from the graph
delTail :: (Eq a, Eq b, Hashable a, Hashable b) => Tail a b -> b -> GraphRep a b -> GraphRep a b
delTail tl = HM.adjust go
  where
    go (Context' _ps _l ss) = Context' _ps _l (HS.delete tl ss)
