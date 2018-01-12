module Main where

import Control.DeepSeq (NFData)
import qualified Data.Graph.Inductive.Strict as G
import qualified Data.Graph.Inductive.PatriciaTree as Old
import qualified Data.Graph.Inductive.Graph as Old
import qualified Data.HashSet as HS

import Criterion.Main

main :: IO ()
main = defaultMain
  [ fgl 1000        -- Benchmark original FGL library
  , fglNg 1000      -- Compare against original FGL library
  , details 1000    -- Detailed benchmarks from current implementation
  , algos           -- Benchmark graph algorithms
  ]

------------------------------
-- * FGL (original library) benchmarks

-- | Benchmark original library
fgl :: Int -> Benchmark
fgl n = let graph = buildOld (oldEdges n) (oldNodes n) in

  bgroup "old"
    [ bgroup "construction"
      [ bench "mkGraph"  $ whnf (buildOld (oldEdges n)) (oldNodes n)
      , bench "fromList" $ whnf (Old.buildGr :: [Old.Context a b] -> Old.Gr a b) (oldCtxts n)
      ]

    -- Basic Interface
    , bgroup "basic"
      [ bench "null"     $ whnf Old.isEmpty graph
      , bench "match"    $ whnf (Old.match n) graph
      , bench "matchAny" $ whnf Old.matchAny graph
      , bench "nodes"    $ whnf Old.labNodes graph
      , bench "order"    $ whnf Old.order graph
      , bench "edges"    $ whnf Old.labEdges graph
      , bench "size"     $ whnf Old.size graph
      , bench "(&)"      $ whnf (([], n+1, n+1, []) Old.&) graph
      ]

    -- Maps
    , bgroup "maps"
      [ bench "nmap"   $ whnf (Old.nmap id) graph
      , bench "emap"   $ whnf (Old.emap id) graph
      , bench "nemap"  $ whnf (Old.nemap id id) graph
      , bench "ctxMap" $ whnf (Old.gmap id) graph
      ]

    -- Folds
    , bgroup "folds"
      [ bench "foldr"  $ whnf (Old.ufold (\(_, _, a, _) c -> a + c) 0) graph
      ]

    -- Queries
    , bgroup "queries"
      [ bench "member"        $ whnf (Old.gelem (n-1)) graph
      , bench "neighbors"     $ whnf ((flip Old.neighbors) (n-1)) graph
      , bench "preds"         $ whnf ((flip Old.pre) (n-1)) graph
      , bench "succs"         $ whnf ((flip Old.suc) (n-1)) graph
      , bench "inEdges"       $ whnf ((flip Old.inn) (n-1)) graph
      , bench "outEdges"      $ whnf ((flip Old.out) (n-1)) graph
      , bench "inDegree"      $ whnf ((flip Old.indeg) (n-1)) graph
      , bench "outDegree"     $ whnf ((flip Old.outdeg) (n-1)) graph
      , bench "degree"        $ whnf ((flip Old.deg) (n-1)) graph
      , bench "hasEdge"       $ whnf ((flip Old.hasEdge) (n-1,n-2)) graph
      , bench "hasNeighbor"   $ whnf ((\x y z -> Old.hasNeighbor z x y) (n-1) (n-2)) graph
      ]

    -- Filters
    , bgroup "filters"
      [ bench "gfiltermap"      $ whnf (Old.gfiltermap (\x -> Just x)) graph
      , bench "nfilter (label)" $ whnf (Old.labnfilter (\_ -> True)) graph
      , bench "nfilter (node)"  $ whnf (Old.nfilter (\_ -> True)) graph
      , bench "subgraph"        $ whnf (Old.subgraph [1..n `div` 2]) graph
      ]

    -- Insertion and Deletion
    , bgroup "insertion and deletion"
      [ bench "insNode"  $ whnf (Old.insNode (n+1, n+1)) graph
      , bench "delNode"  $ whnf (Old.delNode (n-1)) graph
      , bench "insEdge"  $ whnf (Old.insEdge (1,1,())) graph
      , bench "delEdge"  $ whnf (Old.delEdge (n-1,n-1)) graph
      , bench "insNodes" $ whnf (Old.insNodes [(x,x) | x <- [n+1..n+10]]) graph
      , bench "delNodes" $ whnf (Old.delNodes [1..n `div` 2]) graph
      , bench "insEdges" $ whnf (Old.insEdges [(1,1,())]) graph
      , bench "delEdges" $ whnf (Old.delEdges [(x,y) | x <- [n-1], y <- [1..n-1]]) graph
      ]
    ]

------------------------------
-- * FGL-NG (new library) benchmarks

fglNg :: Int -> Benchmark
fglNg n = let graph = G.fromList (listGraph n) in

  bgroup "new"
    -- Construction functions
    [ bgroup "construction"
      [ bench "singleton" $ whnf (G.singleton :: Int -> G.Gr () Int)  n
      , bench "mkGraph"   $ whnf (G.mkGraph (newEdges n)) [1..n]
      , bench "fromList"  $ whnf G.fromList (listGraph n)
      ]

    -- Basic interface
    , bgroup "basic"
      [ bench "null"     $ whnf G.null graph
      , bench "match"    $ whnf (G.match  n) graph
      , bench "matchAny" $ whnf G.matchAny graph
      , bench "nodes"    $ whnf G.nodes graph
      , bench "order"    $ whnf G.order graph
      , bench "edges"    $ whnf G.edges graph
      , bench "size"     $ whnf G.size graph
      , bench "(!)"      $ whnf (G.! n) graph
      , bench "(!?)"     $ whnf (G.!? n)  graph
      ]

    -- Maps
    , bgroup "maps"
      [ bench "nmap"  $ whnf (G.nmap id) graph
      , bench "emap"  $ whnf (G.emap id) graph
      , bench "nemap" $ whnf (G.nemapH id id) graph
      ]

    -- Folds
    , bgroup "folds"
      [ bench "foldr" $ whnf (G.foldr (+) 0) graph
      ]

    -- Queries
    , bgroup "queries"
      [ bench "member"      $ whnf (G.member n) graph
      , bench "neighbors"   $ whnf (G.neighbors n) graph
      , bench "preds"       $ whnf (G.preds n) graph
      , bench "succs"       $ whnf (G.succs n) graph
      , bench "inEdges"     $ whnf (G.inEdges n) graph
      , bench "outEdges"    $ whnf (G.outEdges n) graph
      , bench "inDegree"    $ whnf (G.inDegree n) graph
      , bench "outDegree"   $ whnf (G.outDegree n) graph
      , bench "degree"      $ whnf (G.degree n) graph
      , bench "hasEdge"     $ whnf (G.hasEdge (G.Edge (n-1) () (n-2))) graph
      , bench "hasNeighbor" $ whnf (G.hasNeighbor (n-1) (n-2)) graph
      ]

    -- Filters
    , bgroup "filters"
      [ bench "nfilter" $ whnf (G.nfilter (const True)) graph
      , bench "efilter" $ whnf (G.efilter (const True)) graph
      ]

    -- Insertion and Deletion
    , bgroup "insertion and deletion"
      [ bench "insNode"     $ whnf (G.insNode (n+1)) graph
      , bench "safeInsNode" $ whnf (G.safeInsNode (n+1)) graph
      , bench "delNode"     $ whnf (G.delNode n) graph
      , bench "insEdge"     $ whnf (G.insEdge (G.Edge 1 () 1)) graph
      ]
    ]

------------------------------
-- * Detailed benchmarks

details :: Int -> Benchmark
details n = let graph = G.fromList (listGraph n) in

  bgroup "details"
  [ bgroup "insertion"
    [ bench "insNode"      $ whnf (G.insNode (n+1)) graph
    , bench "insNode-dup"  $ whnf (G.insNode (n-1)) graph
    , bench "insEdge"      $ whnf (G.insEdge (G.Edge 1 () 1)) graph
    , bench "insEdge-dup"  $ whnf (G.insEdge (G.Edge n () n)) graph
    ]

  , bgroup "deletion"
    [ bench "delNode"      $ whnf (G.delNode (n-1)) graph
    , bench "delNode-miss" $ whnf (G.delNode (n+1)) graph
    , bench "delEdge"      $ whnf (G.delEdge (G.Edge n () n)) graph
    , bench "delEdge-miss" $ whnf (G.delEdge (G.Edge 1 () 1)) graph
    ]

  , bgroup "lookup"
    [ bench "lookup"       $ whnf (G.!? (n-1)) graph
    , bench "lookup-miss"  $ whnf (G.!? (n+1)) graph
    ]
  ]

------------------------------
-- * Algorithm benchmarks

algos :: Benchmark
algos = bgroup "algos" []

------------------------------
-- Utilities

-- | Build a complete graph using fgl
buildOld :: [Old.LEdge ()] -> [Old.LNode Int] -> Old.Gr Int ()
buildOld es ns = Old.mkGraph ns es

-- | Build a complete graph using fgl list of ctxts
buildOldList :: [Old.Context Int ()] -> Old.Gr Int ()
buildOldList ctxts = Old.buildGr ctxts

-- | Generate old edges from number of nodes
-- Exclude (1, 1, ()) for testing purposes
oldEdges :: Int -> [(Int, Int, ())]
oldEdges n = tail $ (\x y -> (x,y,())) <$> [1..n] <*> [1..n]

-- | Generate old nodes from number of nodes
oldNodes :: Int -> [(Int, Int)]
oldNodes n = map (\x -> (x,x)) [1..n]

-- | Generate old Contexts to build a complete graph
oldCtxts :: Int -> [Old.Context Int ()]
oldCtxts n = first : [([((),n'') | n'' <- [1..n']], n', n', [((),n'') | n'' <- [1..n']]) | n' <- [1..n]]
  where
    first :: Old.Context Int ()
    first = ([], 1, 1, [])

-- | Generate new edges from number of nodes
-- Exclude (1, (), 1) for testing purposes
newEdges :: Int -> [G.Edge () Int]
newEdges n = tail $ (\x y -> G.Edge x () y) <$> [1..n] <*> [1..n]

listGraph :: Int -> [(Int, G.Context' () Int)]
listGraph n = first : [ (n', G.Context' (HS.fromList [G.Head () n'' | n'' <- [1..n]])
                                        n'
                                        (HS.fromList [G.Tail () n'' | n'' <- [1..n]])) | n' <- [2..n] ]
  where
    first = (1, G.Context' (HS.fromList [G.Head () n'' | n'' <- [2..n]]) 1 (HS.fromList [G.Tail () n'' | n'' <- [2..n]]))
