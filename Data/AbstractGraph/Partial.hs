-- | Partial Graphs
--
-- Elements of type Node may or may not be in the graph
-- That is, for n1, n2 :: Node, lookup n1 n2 :: Edge might
-- fail.
module Data.AbstractGraph.Partial where

type Graph n e = n - > Maybe (n -> e)

empty :: Monoid e => Graph n e
empty = const (const mempty)

-- This is only /= mempty, it does not account for multiple edges
size :: Monoid => Graph n e -> Int
size g = length $ filter (/= mempty) $ edges g

edges :: Graph n e -> [e]
edges g = lookup <$> nodes g <*> nodes g

-- | Filter edges
efilter :: (Graph g n e, Monoid e) => (e -> Bool) -> g -> g
efilter f g = emap (\e -> if f e then e else mempty) g

-- | A simple printer
pretty :: (Graph g n e, Show e, Show n) => g -> String
pretty = show . toList

-- | Find the edge between two nodes
lookup :: (Graph g n e) => g -> n -> n -> e
lookup = sample

-- | Modify an edge
update :: (Graph g n e, Eq n, Semigroup e) => g -> n -> n -> e -> g
update = updateWith (<>)

-- | Contravariant mapping over nodes
nmap :: (Graph g m e, Graph g n e) => (n -> m) -> g -> g
nmap = contramap

-- | Map over edges
emap :: (Graph g n e, Graph g n f) => (e -> f) -> g -> g
emap = fmap

--------------------------------------------
-- Nodes functions

-- -- | Number of nodes
-- order :: (Graph g n e) => g -> Int
-- order = length . nodes
-- 
-- -- | Number of edges
-- --
-- -- This is only /= mempty
-- size :: (Graph g n e, Eq e, Monoid e) => g -> Int
-- size g = length $ filter (/= mempty) $ edges g
-- 
-- -- | List of edges
-- edges :: (Graph g n e) => g -> [e]
-- edges g = lookup g <$> ns <*> ns
--   where
--     ns = nodes g
-- 
-- -- | Fold over edges
-- gfoldr :: (Graph g n e) => (e -> b -> b) -> b -> g -> b
-- gfoldr f acc = foldr f acc . edges
-- 
-- -- | Successors (nodes we have an edge to)
-- --
-- -- This is only /= mempty
-- succs :: (Graph g n e, Eq e, Monoid e) => g -> n -> [n]
-- succs g n = [ n' | n' <- nodes g, lookup g n n' /= mempty ]
-- 
-- -- | Predecessors
-- --
-- -- This is only /= mempty
-- preds :: (Graph g n e, Eq e, Monoid e) => g -> n -> [n]
-- preds g n = [ n' | n' <- nodes g, lookup g n' n /= mempty ]
-- 
-- -- | Incoming edges
-- inEdges :: (Graph g n e) => g -> n -> [e]
-- inEdges g n = [ e | n' <- nodes g, let e = lookup g n' n ]
-- 
-- -- | Outgoing edges
-- outEdges :: (Graph g n e) => g -> n -> [e]
-- outEdges g n = [ e | n' <- nodes g, let e = lookup g n n' ]
-- 
-- -- | Convert to a list of (Node, Edge, Node)
-- toList :: (Graph g n e) => g -> [(n,e,n)]
-- toList g = (\n1 n2 -> (n1, lookup g n1 n2, n2)) <$> ns <*> ns
--   where
--     ns = nodes g
