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
  , details         -- Detailed benchmarks from current implementation
  , algos           -- Benchmark graph algorithms
  ]

------------------------------
-- * FGL (original library) benchmarks

-- | Benchmark original library
fgl :: Int -> Benchmark
fgl n = let graph = buildOld (oldEdges n) (oldNodes n) in

  bgroup "old"
    -- Match original library's benchmark
    [ bgroup "old benchmarks"
      [ bench "build"   $ whnf (buildOld (oldEdges n)) (oldNodes n)
      , bench "insNode" $ whnf (Old.insNode (n+1, n+1)) graph
      , bench "insEdge" $ whnf (Old.insEdge (1,1,())) graph
      , bench "gmap"    $ whnf (Old.gmap id) graph
      , bench "nmap"    $ whnf (Old.nmap id) graph
      , bench "emap"    $ whnf (Old.emap id) graph
      ]

    -- Basic Interface
    , bgroup "class"
      [ bench "isEmpty"  $ whnf Old.isEmpty graph
      , bench "match"    $ whnf (Old.match n) graph
      , bench "mkGraph"  $ whnf (buildOld (oldEdges n)) (oldNodes n)
      , bench "nodes"    $ whnf Old.labNodes graph
      , bench "matchAny" $ whnf Old.matchAny graph
      , bench "order"    $ whnf Old.order graph
      , bench "edges"    $ whnf Old.labEdges graph
      , bench "(&)"      $ whnf (([], n+1, n+1, []) Old.&) graph
      ]

    -- Queries
    , bgroup "queries"
      [ bench "member (node)" $ whnf (Old.gelem (n-1)) graph
      , bench "member (edge)" $ whnf ((flip Old.hasEdge) (n-1,n-2)) graph
      , bench "neighbors"     $ whnf ((flip Old.neighbors) (n-1)) graph
      , bench "succs"         $ whnf ((flip Old.suc) (n-1)) graph
      , bench "preds"         $ whnf ((flip Old.pre) (n-1)) graph
      , bench "outs"          $ whnf ((flip Old.out) (n-1)) graph
      , bench "ins"           $ whnf ((flip Old.inn) (n-1)) graph
      , bench "outDeg"        $ whnf ((flip Old.outdeg) (n-1)) graph
      , bench "inDeg"         $ whnf ((flip Old.indeg) (n-1)) graph
      , bench "deg"           $ whnf ((flip Old.deg) (n-1)) graph
      , bench "hasNeighbor"   $ whnf ((\x y z -> Old.hasNeighbor z x y) (n-1) (n-2)) graph
      ]

    -- Insertion and Deletion
    , bgroup "insertion and deletion"
      [ bench "insNode"  $ whnf (Old.insNode (n+1, n+1)) graph
      , bench "delNode"  $ whnf (Old.delNode (n-1)) graph
      , bench "insNodes" $ whnf (Old.insNodes [(x,x) | x <- [n+1..n+10]]) graph
      , bench "delNodes" $ whnf (Old.delNodes [1..n `div` 2]) graph
      , bench "insEdge"  $ whnf (Old.insEdge (1,1,())) graph
      , bench "delEdge"  $ whnf (Old.delEdge (n-1,n-1)) graph
      , bench "insEdges" $ whnf (Old.insEdges [(1,1,())]) graph
      , bench "delEdges" $ whnf (Old.delEdges [(x,y) | x <- [n-1], y <- [1..n-1]]) graph
      ]

    -- Filters
    , bgroup "filters"
      [ bench "gfiltermap"      $ whnf (Old.gfiltermap (\x -> Just x)) graph
      , bench "nfilter (label)" $ whnf (Old.labnfilter (\_ -> True)) graph
      , bench "nfilter (node)"  $ whnf (Old.nfilter (\_ -> True)) graph
      , bench "subgraph"        $ whnf (Old.subgraph [1..n `div` 2]) graph
      ]

    -- Folds and Maps
    , bgroup "folds and maps"
      [ bench "foldr"  $ whnf (Old.ufold (\(_, _, a, _) c -> a + c) 0) graph
      , bench "ctxMap" $ whnf (Old.gmap id) graph
      , bench "nmap"   $ whnf (Old.nmap id) graph
      , bench "emap"   $ whnf (Old.emap id) graph
      , bench "nemap"  $ whnf (Old.nemap id id) graph
      ]
    ]

------------------------------
-- * FGL-NG (new library) benchmarks

fglNg :: Int -> Benchmark
fglNg n = let graph = G.fromList (listGraph n) in

  bgroup "new"
    -- Construction functions
    [ bgroup "construction"
      [ bench "singleton" $ nf (G.singleton :: Int -> G.Gr () Int)  n
      , bench "mkGraph"   $ nf (G.mkGraph (newEdges n)) [1..n]
      ]

    -- Basic interface
    , bgroup "basic"
      [ bench "null"      $ nf G.null graph
      , bench "order"     $ nf G.order graph
      , bench "size"      $ nf G.size graph
      , bench "match"     $ nf (G.match  n) graph
      , bench "matchAny"  $ nf G.matchAny graph
      , bench "(!)"       $ nf (G.! n) graph
      , bench "(!?)"      $ nf (G.!? n)  graph
      , bench "nodes"     $ nf G.nodes graph
      , bench "edges"     $ nf G.edges graph
      ]

    -- Maps
    , bgroup "maps"
      [ bench "nmap"      $ nf (G.nmap id) graph
      , bench "emap"      $ nf (G.emap id) graph
      , bench "nemapH"    $ nf (G.nemapH id id) graph
      ]

    -- Folds
    , bgroup "folds"
      [ bench "foldr"     $ nf (G.foldr (+) 0) graph
      ]

    -- Queries
    , bgroup "queries"
      [ bench "member"      $ nf (G.member n) graph
      , bench "neighbors"   $ nf (G.neighbors n) graph
      , bench "preds"       $ nf (G.preds n) graph
      , bench "succs"       $ nf (G.succs n) graph
      , bench "inEdges"     $ nf (G.inEdges n) graph
      , bench "outEdges"    $ nf (G.outEdges n) graph
      , bench "inDegree"    $ nf (G.inDegree n) graph
      , bench "outDegree"   $ nf (G.outDegree n) graph
      , bench "degree"      $ nf (G.degree n) graph
      , bench "hasEdge"     $ nf (G.hasEdge (G.Edge (n-1) () (n-2))) graph
      , bench "hasNeighbor" $ nf (G.hasNeighbor (n-1) (n-2)) graph
      ]

    -- Filters
    , bgroup "filters"
      [ bench "nfilter" $ nf (G.nfilter (const True)) graph
      , bench "efilter" $ nf (G.efilter (const True)) graph
      ]

    -- Insertion and Deletion
    , bgroup "insertion and deletion"
      [ bench "insNode"     $ nf (G.insNode (n+1)) graph
      , bench "safeInsNode" $ nf (G.safeInsNode (n+1)) graph
      , bench "delNode"     $ nf (G.delNode n) graph
      , bench "insEdge"     $ nf (G.insEdge (G.Edge 1 () 1)) graph
      ]
    ]

------------------------------
-- * Detailed benchmarks

details :: Benchmark
details =
  bgroup "details" []
  {-
  [ bgroup "insertion"
    [ bench "new node" $ whnf G.insNode 10
    , bench "collision" $ whnf G.insNode 10
    ]
  ]
  -}

------------------------------
-- * Algorithm benchmarks

algos :: Benchmark
algos = bgroup "algos" []

------------------------------
-- Utilities

-- | Create the inputs for complete graph construction
nsAndEs :: Int -> IO ([(Int, Int, ())], [(Int, Int)], [G.Edge () Int], [(Int, G.Context' () Int)])
nsAndEs n = return (oldEdges n, oldNodes n, newEdges n, listGraph n)

graphs :: IO (Old.Gr Int (), Old.Gr Int (), G.Gr () Int, G.Gr () Int)
graphs = do
    let small = 100
        large = 1000
        oldSmall = buildOld (oldEdges small) (oldNodes small)
        oldLarge = buildOld (oldEdges large) (oldNodes large)
        newSmall = buildNew (newEdges small) [1..small]
        newLarge = buildNew (newEdges large) [1..large]
    return (oldSmall, oldLarge, newSmall, newLarge)

-- | Build a complete graph using fgl-ng
buildNew :: [G.Edge () Int] -> [Int] -> G.Gr () Int
buildNew = G.mkGraph

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
