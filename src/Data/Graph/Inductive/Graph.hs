module Data.Graph.Inductive.Graph where

-- | A graph
class Graph gr where
    {-# MINIMAL empty, isEmpty, match, mkGraph, nodes, edges #-}

    -- | An empty 'Graph'.
    empty   :: gr a b

    -- | True if given 'Graph' is empty.
    isEmpty :: gr a b -> Bool

    -- | Try to decopmose a 'Graph' into the `Context` for the given node
    -- and the remaining 'Graph'.
    match   :: Eq a => a -> gr a b -> Maybe (Context a b, gr a b) 

    -- | Create a 'Graph' from the lists of nodes and edges.
    --
    -- For graphs that are also instances of 'DynGraph'. @mkGraph ns
    -- es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
    -- 'empty'@.
    mkGraph :: [a] -> [Edge b] -> gr a b

    -- | A list of all nodes in a 'Graph'.
    nodes   :: gr a b -> [a]

    -- | The number of nodes in a 'Graph'.
    order   :: gr a b -> Int
    order   = length . nodes

    -- | A list of all edges in a 'Graph'.
    edges   :: gr a b -> [Edge b]

    -- | The number of edges in a 'Graph'.
    size    :: gr a b -> Int
    size    = length . edges

    -- | Try to decompose a 'Graph' into the 'Context' for an arbitrary node
    -- and the remaining 'Graph'.
    matchAny :: gr a b -> Maybe (Context a b, gr a b)
    matchAny g = case nodes g of
        [] -> Nothing
        (n:_) -> match n g

class (Graph gr) => DynGraph gr where
    -- | Merge the given 'Context' into the 'DynGraph'.
    --
    -- Context adjacencies should only refer to either a node already
    -- in a graph or the node in the Context itself (for loops).
    --
    -- Behaviour is undefined if the specified node already exists in the
    -- graph.
    (&) :: Context a b -> gr a b -> gr a b


--------------------------------------------------------
-- Useful Functions

fold ::


instance (Graph g) => Functor (g a) where
    fmap = map

{-
instance (Graph g) => Foldable (g a) where
    fold = 

instance (Graph g) => Traversable (g a) where
    traverse f = traverse

instance (Graph g) => Applicative (g a) where
    pure n = 
    f <*> g = 

instance (Show a, Show b) => Show (Graph a b) where
    show gr = case matchAny gr of
        Nothing -> ""
        Just (Context a b, graph) -> 
-}
