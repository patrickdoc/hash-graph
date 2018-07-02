{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- New library imports
import qualified Data.HashGraph.Strict as G
import qualified Data.HashGraph.Algorithms as G

-- FGL imports
import qualified Data.Graph.Inductive.Basic as Old
import qualified Data.Graph.Inductive.Graph as Old
import qualified Data.Graph.Inductive.PatriciaTree as Old
import qualified Data.Graph.Inductive.Query as Old

import qualified Data.HashSet as HS
import Control.DeepSeq (NFData(..))
import Criterion.Main

main :: IO ()
main = defaultMain
  [ fgl 1000        -- Benchmark original FGL library
  , hashGraph 1000      -- Compare against original FGL library
  , oldDetails 1000 -- Detailed benchmarks from old implementation
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
      , bench "match"    $ nf (Old.match n) graph
      , bench "matchAny" $ nf Old.matchAny graph
      , bench "nodes"    $ nf Old.labNodes graph
      , bench "order"    $ whnf Old.order graph
      , bench "edges"    $ nf Old.labEdges graph
      , bench "size"     $ whnf Old.size graph
      , bench "(&)"      $ whnf (([], n+1, n+1, []) Old.&) graph
      ]

    -- Maps
    , bgroup "maps"
      [ bench "nmap"   $ nf (Old.nmap id) graph
      , bench "emap"   $ nf (Old.emap id) graph
      , bench "nemap"  $ nf (Old.nemap id id) graph
      , bench "ctxMap" $ nf (Old.gmap id) graph
      ]

    -- Folds
    , bgroup "folds"
      [ bench "foldr"  $ whnf (Old.ufold (\(_, _, a, _) c -> a + c) 0) graph
      ]

    -- Queries
    , bgroup "queries"
      [ bench "member"        $ whnf (Old.gelem (n-1)) graph
      , bench "neighbors"     $ nf (flip Old.neighbors (n-1)) graph
      , bench "preds"         $ nf (flip Old.pre (n-1)) graph
      , bench "succs"         $ nf (flip Old.suc (n-1)) graph
      , bench "inEdges"       $ nf (flip Old.inn (n-1)) graph
      , bench "outEdges"      $ nf (flip Old.out (n-1)) graph
      , bench "inDegree"      $ whnf (flip Old.indeg (n-1)) graph
      , bench "outDegree"     $ whnf (flip Old.outdeg (n-1)) graph
      , bench "degree"        $ whnf (flip Old.deg (n-1)) graph
      , bench "hasEdge"       $ whnf (flip Old.hasEdge (n-1,n-2)) graph
      , bench "hasNeighbor"   $ whnf ((\x y z -> Old.hasNeighbor z x y) (n-1) (n-2)) graph
      ]

    -- Filters
    , bgroup "filters"
      [ bench "gfiltermap"      $ whnf (Old.gfiltermap Just) graph
      , bench "nfilter (label)" $ nf (Old.labnfilter (const True)) graph
      , bench "nfilter (node)"  $ nf (Old.nfilter (const True)) graph
      , bench "efilter"         $ whnf (Old.efilter (const True)) graph
      , bench "subgraph"        $ whnf (Old.subgraph [1..n `div` 2]) graph
      ]

    -- Insertion and Deletion
    , bgroup "insertion and deletion"
      [ bench "insNode"  $ whnf (Old.insNode (n+1, n+1)) graph
      , bench "delNode"  $ whnf (Old.delNode (n-1)) graph
      , bench "insEdge"  $ whnf (Old.insEdge (1,1,1)) graph
      , bench "delEdge"  $ whnf (Old.delEdge (n-1,n-1)) graph
      , bench "insNodes" $ whnf (Old.insNodes [(x,x) | x <- [n+1..n+10]]) graph
      , bench "delNodes" $ whnf (Old.delNodes [1..n `div` 2]) graph
      , bench "insEdges" $ whnf (Old.insEdges [(1,1,1)]) graph
      , bench "delEdges" $ whnf (Old.delEdges [(x,y) | x <- [n-1], y <- [1..n-1]]) graph
      ]

    -- Algorithms
    , bgroup "algorithms"
      [ bench "bfs" $ nf (Old.bfs (n-1)) graph
      , bench "dfs" $ nf (Old.dfs [n-1]) graph
      , bench "mst" $ nf (Old.msTreeAt n) graph
      ]
    ]

-- To `nf` the `msTree` results we need this instance
instance (NFData a) => NFData (Old.LPath a) where
    rnf (Old.LP lp) = rnf lp

------------------------------
-- * hash-graph (new library) benchmarks

hashGraph :: Int -> Benchmark
hashGraph n = let graph = G.fromList (listGraph n) in

  bgroup "new"
    -- Construction functions
    [ bgroup "construction"
      [ bench "singleton" $ whnf (G.singleton :: Int -> G.Gr Int Int)  n
      , bench "mkGraph"   $ whnf (G.mkGraph (newEdges n)) [1..n]
      , bench "fromList"  $ whnf G.fromList (listGraph n)
      ]

    -- Basic interface
    , bgroup "basic"
      [ bench "null"     $ whnf G.null graph
      , bench "nodes"    $ nf G.nodes graph
      , bench "order"    $ whnf G.order graph
      , bench "edges"    $ nf G.edges graph
      , bench "size"     $ whnf G.size graph
      , bench "(!)"      $ whnf (G.! n) graph
      , bench "(!?)"     $ whnf (G.!? n)  graph
      , bench "match"    $ nf (G.match n) graph
      , bench "matchAny" $ nf G.matchAny graph
      , bench "(&)"      $ whnf ((n+1,G.Context' HS.empty HS.empty) G.&) graph
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
      , bench "neighbors"   $ nf (G.neighbors n) graph
      , bench "preds"       $ nf (G.preds n) graph
      , bench "succs"       $ nf (G.succs n) graph
      , bench "inEdges"     $ nf (G.inEdges n) graph
      , bench "outEdges"    $ nf (G.outEdges n) graph
      , bench "inDegree"    $ whnf (G.inDegree n) graph
      , bench "outDegree"   $ whnf (G.outDegree n) graph
      , bench "degree"      $ whnf (G.degree n) graph
      , bench "hasEdge"     $ whnf (G.hasEdge (G.Edge (n-1) ((n-1)*(n-2)) (n-2))) graph
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
      , bench "insEdge"     $ whnf (G.insEdge (G.Edge 1 1 1)) graph
      , bench "delEdge"     $ whnf (G.delEdge (G.Edge n (n*n) n)) graph
      ]

    -- Algorithms
    , bgroup "algorithms"
      [ bench "bfs" $ nf (G.bfsn (n-1)) graph
      , bench "dfs" $ nf (G.dfsn (n-1)) graph
      , bench "mst" $ nf G.prim graph
      ]
    ]

------------------------------
-- * Detailed benchmarks

details :: Int -> Benchmark
details n = let graph = G.fromList (listGraph n) in

  bgroup "new/details"
  [ bgroup "insertion"
    [ bench "insNode"      $ whnf (G.insNode (n+1)) graph
    , bench "insNode-dup"  $ whnf (G.insNode (n-1)) graph
    , bench "insEdge"      $ whnf (G.insEdge (G.Edge 1 1 1)) graph
    , bench "insEdge-dup"  $ whnf (G.insEdge (G.Edge n (n*n) n)) graph
    ]

  , bgroup "deletion"
    [ bench "delNode"      $ whnf (G.delNode (n-1)) graph
    , bench "delNode-miss" $ whnf (G.delNode (n+1)) graph
    , bench "delEdge"      $ whnf (G.delEdge (G.Edge n (n*n) n)) graph
    , bench "delEdge-miss" $ whnf (G.delEdge (G.Edge 1 1 1)) graph
    ]

  , bgroup "lookup"
    [ bench "lookup"       $ whnf (G.!? (n-1)) graph
    , bench "lookup-miss"  $ whnf (G.!? (n+1)) graph
    ]
  ]

oldDetails :: Int -> Benchmark
oldDetails n = let graph = Old.buildGr (oldCtxts n) :: Old.Gr Int Int in

  bgroup "old/details"
  [ bgroup "insertion"
    [ bench "insNode"     $ whnf (Old.insNode (n+1, n+1)) graph
    , bench "insNode-dup" $ whnf (Old.insNode (n-1, n-1)) graph
    , bench "insEdge"     $ whnf (Old.insEdge (1,1,1)) graph
    , bench "insEdge-dup" $ whnf (Old.insEdge (1,2,2)) graph
    ]

  , bgroup "deletion"
    [ bench "delNode"      $ whnf (Old.delNode (n-1)) graph
    , bench "delNode-miss" $ whnf (Old.delNode (n+1)) graph
    , bench "delEdge"      $ whnf (Old.delEdge (n-1,n-1)) graph
    , bench "delEdge-miss" $ whnf (Old.delEdge (1,1)) graph
    ]
  ]

------------------------------
-- * Algorithm benchmarks

algos :: Benchmark
algos = bgroup "algos" []

------------------------------
-- Utilities

-- | Build a complete graph using fgl
buildOld :: [Old.LEdge Int] -> [Old.LNode Int] -> Old.Gr Int Int
buildOld es ns = Old.mkGraph ns es

-- | Build a complete graph using fgl list of ctxts
buildOldList :: [Old.Context Int Int] -> Old.Gr Int Int
buildOldList ctxts = Old.buildGr ctxts

-- | Generate old edges from number of nodes
-- Exclude (1, 1, 1) for testing purposes
oldEdges :: Int -> [(Int, Int, Int)]
oldEdges n = tail $ (\x y -> (x,y,x*y)) <$> [1..n] <*> [1..n]

-- | Generate old nodes from number of nodes
oldNodes :: Int -> [(Int, Int)]
oldNodes n = map (\x -> (x,x)) [1..n]

-- | Generate old Contexts to build a complete graph
oldCtxts :: Int -> [Old.Context Int Int]
oldCtxts n = first : [([(n'*n'',n'') | n'' <- [1..n']], n', n', [(n'*n'',n'') | n'' <- [1..n']]) | n' <- [2..n]]
  where
    first :: Old.Context Int Int
    first = ([], 1, 1, [])

-- | Generate new edges from number of nodes
-- Exclude (1, 1, 1) for testing purposes
newEdges :: Int -> [G.Edge Int Int]
newEdges n = tail $ (\x y -> G.Edge x (x*y) y) <$> [1..n] <*> [1..n]

listGraph :: Int -> [G.Context Int Int]
listGraph n = first : [ (n', G.Context' (HS.fromList [G.Head (n'*n'') n'' | n'' <- [1..n]])
                                        (HS.fromList [G.Tail (n'*n'') n'' | n'' <- [1..n]])) | n' <- [2..n] ]
  where
    first = (1, G.Context' (HS.fromList [G.Head n'' n'' | n'' <- [2..n]]) (HS.fromList [G.Tail n'' n'' | n'' <- [2..n]]))
