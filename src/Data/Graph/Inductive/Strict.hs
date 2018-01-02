module Data.Graph.Inductive.Strict where

import Control.DeepSeq
import Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Prelude hiding (foldr)

-- * Graph type
type GraphRep a b = HM.HashMap b (Context' a b)
newtype Gr a b = Gr (GraphRep a b) deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (Gr a b) where
    rnf (Gr hm) = rnf hm

-- | The Edge type and corresponding half edges
data Edge a b = Edge !b !a !b  deriving (Eq, Show)
data Head a b = Head !a !b deriving (Eq, Show)
data Tail a b = Tail !a !b deriving (Eq, Show)

instance (Hashable a, Hashable b) => Hashable (Head a b) where
    hashWithSalt p (Head a b) = hashWithSalt p a `hashWithSalt` b

instance (NFData a, NFData b) => NFData (Head a b) where
    rnf (Head a b) = rnf a `seq` rnf b

instance (Hashable a, Hashable b) => Hashable (Tail a b) where
    hashWithSalt p (Tail a b) = hashWithSalt p a `hashWithSalt` b

instance (NFData a, NFData b) => NFData (Tail a b) where
    rnf (Tail a b) = rnf a `seq` rnf b

-- | The Context of a node within the Graph type
data Context' a b = Context'
    { heads :: !(HS.HashSet (Head a b)) -- ^ Predecessors of the node
    , self  :: !b                       -- ^ The node label
    , tails :: !(HS.HashSet (Tail a b)) -- ^ Successors of the node
    } deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (Context' a b) where
    rnf (Context' ps b ss) = rnf ps `seq` rnf b `seq` rnf ss

-- | Extract the Head half of a directed edge
edgeHead :: Edge a b -> Head a b
edgeHead (Edge p l _) = Head l p

-- | Extract the Tail half of a directed edge
edgeTail :: Edge a b -> Tail a b
edgeTail (Edge _ l s) = Tail l s

-------------------------------
-- * Construction

-- | /O(1)/ Construct an empty graph
empty :: Gr a b
empty = Gr HM.empty

-- | /O(1)/ Construct a graph with a single node
singleton :: (Eq b, Hashable b) => b -> Gr a b
singleton b = Gr $ HM.singleton b (Context' HS.empty b HS.empty)

-- TODO: cleanup and determine time complexity
mkGraph :: (Eq a, Eq b, Hashable a, Hashable b, NFData a, NFData b) => [Edge a b] -> [b] -> Gr a b
mkGraph es ns = Gr $ L.foldl' (flip insEdge) nodeGraph es
  where
    nodeGraph = HM.fromList $ map (\x -> (x, Context' HS.empty x HS.empty)) ns

{-
-- | Construct a graph from the given edges and nodes
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

-------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this graph is empty, 'False' otherwise
null :: Gr a b -> Bool
null (Gr g) = HM.null g

-- | /O(n)/ Return the number of nodes in the graph
order :: Gr a b -> Int
order (Gr g) = HM.size g

-- TODO: Determine time complexity
-- | /O(?)/ Return the number of edges in the graph
size :: Gr a b -> Int
size = length . edges

-- | Extract a node from the graph
match :: (Eq a, Eq b, Hashable a, Hashable b) => b -> Gr a b -> Maybe (Context' a b, Gr a b)
match n (Gr graph) = case HM.lookup n graph of
    Just ctx -> let newGraph = delNode n graph
                in Just (ctx, Gr newGraph)
    Nothing -> Nothing

-- | Extract any node from the graph
matchAny :: (Eq a, Eq b, Hashable a, Hashable b) => Gr a b -> Maybe (Context' a b, Gr a b)
matchAny g = case nodes g of
    (n:_) -> match n g
    [] -> Nothing

-- TODO: Figure out API for dynamic graphs
{-
-- | Merge the 'Context' into the graph
(&) :: Context a b -> Gr a b -> Gr a b
(&) (Context' p l s) (Gr g) = insNode
-}

infixl 9 !, !?
-- | /O(log n)/ Return the 'Context' of a node in the graph.
-- Call 'error' if the node is not in the graph.
(!) :: (Eq b, Hashable b) => Gr a b -> b -> Context' a b
(!) g n = case g !? n of
    Just ctx -> ctx
    Nothing -> error "Data.Graph.Inductive.Strict.(!): node not found"

-- | /O(log n)/ Return the 'Context' of a node if it is in the graph,
-- or 'Nothing' if it is not.
(!?) :: (Eq b, Hashable b) => Gr a b -> b -> Maybe (Context' a b)
(!?) (Gr graph) n = HM.lookup n graph

-- | /O(n)/ Return a list of the nodes in the graph.
-- The list is produced lazily
nodes :: Gr a b -> [b]
nodes (Gr g) = [ node | (node, _) <- HM.toList g ]

-- TODO: Determine time complexity
-- | /O(?)/ Return a list of the edges in the graph
edges :: Gr a b -> [Edge a b]
edges (Gr hm) = HM.foldl' (\lst ctx -> getTails ctx ++ lst) [] hm
  where
    getTails (Context' _ p ss) = HS.foldl' (\lst (Tail l s) -> Edge p l s : lst) [] ss

--------------------------------------
-- * Maps

-- TODO: Clarify collisions after mapping
-- TODO: Consider cmap for contexts

-- | Map /f/ over the nodes.
nmap :: (Eq a, Eq c, Hashable a, Hashable c) => (b -> c) -> Gr a b -> Gr a c
nmap = nemapH id

-- | Map /f/ over the edges.
emap :: (Eq b, Eq c, Hashable b, Hashable c) => (a -> c) -> Gr a b -> Gr c b
emap f = nemapH f id

-- HashMap based
-- | Map /fe/ over the edges and /fn/ over the nodes.
nemapH :: (Eq c, Eq d, Hashable c, Hashable d) => (a -> c) -> (b -> d) -> Gr a b -> Gr c d
nemapH fe fn (Gr g) = Gr $ HM.fromList $ map go $ HM.toList g
  where
    go (n, (Context' ps _ ss)) = (fn n, (Context' (goHead ps) (fn n) (goTail ss)))
    goHead = HS.map (\(Head l p) -> Head (fe l) (fn p))
    goTail = HS.map (\(Tail l s) -> Tail (fe l) (fn s))

-- Inductive based
-- | Map /fe/ over the edges and /fn/ over the nodes.
{-
nemapI :: (a -> c) -> (b -> d) -> Gr a b -> Gr c d
nemapI fe fn g = case matchAny g of
    Just (Context' ps l ss, g') -> Context' (goHead ps) (fn l) (goTail ss) & nemapI fe fn g'
    Nothing -> empty
  where
    goHead = HS.map (\(Head l p) -> Head (fe l) (fn p))
    goTail = HS.map (\(Tail l s) -> Tail (fe l) (fn s))
-}

---------------------------------------
-- * Folds

instance Foldable (Gr a) where
    foldr = foldr

-- | HashMap based
foldr :: (b -> c -> c) -> c -> Gr a b -> c
foldr f x (Gr g) = L.foldr (\(n, _) c -> f n c) x $ HM.toList g

------------------------------------
-- * Queries

-- | /O(log n)/ Return 'True' if the given node is in the graph, 'False' otherwise.
member :: (Eq b, Hashable b) => b -> Gr a b -> Bool
member n (Gr g) = HM.member n g

-- | /O(?)/ Return a list of the neighbors of the given node.
neighbors :: (Eq b, Hashable b) => b -> Gr a b -> Maybe [b]
neighbors n g = (++) <$> hs <*> ts
  where
    ctx = g !? n
    hs = HS.foldl' (\ls (Head _ p) -> p : ls) [] . heads <$> ctx
    ts = HS.foldl' (\ls (Tail _ s) -> s : ls) [] . tails <$> ctx

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
    Just (Context' _ _ ss) -> HS.member (Tail l s) ss
    Nothing -> False

-- | /O(?)/ Return 'True' if the given nodes are neighbors, 'False' otherwise.
hasNeighbor :: (Eq b, Hashable b) => b -> b -> Gr a b -> Bool
hasNeighbor n1 n2 g = case g !? n1 of
    Just (Context' _ _ ss) -> HS.foldl' (\t (Tail _ p) -> t || p == n2) False ss
    Nothing -> False

-----------------------------
-- * Filters

{-
 - filter:
 - - contexts
 - - nodes
 - - edges
 - - given specific nodes?
 -}

nfilter :: (b -> Bool) -> Gr a b -> Gr a b
nfilter f (Gr g) = Gr $ HM.fromList $ filter f' $ HM.toList g
  where
    f' (n, (Context' ps n' ss)) = (f n, Context' (HS.filter (\(Head _ p) -> f p) ps)
                                                 (f n')
                                                 (HS.filter (\(Tail _ s) -> f s) ss)

efilter :: (Edge a b -> Bool) -> Gr a b -> Gr a b

------------------------------
-- * Insertion and Deletion

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

-- TODO: Currently unsafe? check how adjust works
insEdge :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insEdge e hm = insTail e (insHead e hm)

-- | Remove the head ends of tails attached to the node
delHeads :: (Eq a, Eq b, Hashable a, Hashable b) => Context' a b -> GraphRep a b -> GraphRep a b
delHeads (Context' _ b ss) hm = HS.foldl' go hm ss
  where
    go h (Tail l s) = delHead (Head l b) s h

-- | Remove the tail ends of heads attached to the node
delTails :: (Eq a, Eq b, Hashable a, Hashable b) => Context' a b -> GraphRep a b -> GraphRep a b
delTails (Context' ps b _) hm = HS.foldl' go hm ps
  where
    go h (Head l p) = delTail (Tail l b) p h

-- | Insert a head into the graph
insHead :: (Eq a, Eq b, Hashable a, Hashable b) => Edge a b -> GraphRep a b -> GraphRep a b
insHead (Edge p l s) = HM.adjust go s
  where
    go (Context' ps _l _ss) = Context' (HS.insert (Head l p) ps) _l _ss

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

{-
 - equality
 - build from contexts?
 - pretty printing
-}
