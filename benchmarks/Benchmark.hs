module Main where

import Control.DeepSeq (NFData)
import qualified Data.Graph.Inductive.Strict as G
import qualified Data.Graph.Inductive.PatriciaTree as Old
import qualified Data.Graph.Inductive.Graph as Old
import qualified Data.HashSet as HS

import Criterion.Main

main :: IO ()
main = do
    comparisons     -- Compare against original FGL library
    library         -- Benchmark library functionality
    --algos           -- Benchmark graph algorithms

------------------------------
-- * Comparison benchmarks

comparisons :: IO ()
comparisons = defaultMain
  [ bgroup "comparisons"
    [ bgroup "fgl"
      [ bgroup "build"
        [ bgroup "small"  $ sizedBuild 100
        , bgroup "medium" $ sizedBuild 500
        , bgroup "large"  $ sizedBuild 1000
        ]
      , bgroup "insert node"
        [ bgroup "small"  $ sizedFuncCompare (Old.insNode (101, 101)) (G.insNode 101) 100
        , bgroup "medium" $ sizedFuncCompare (Old.insNode (501, 501)) (G.insNode 501) 500
        , bgroup "large"  $ sizedFuncCompare (Old.insNode (1001, 1001)) (G.insNode 1001) 1000
        ]
      , bgroup "insert edge"
        [ bgroup "small"  $ sizedFuncCompare (Old.insEdge (1,1,())) (G.insEdge (G.Edge 1 () 1)) 100
        , bgroup "medium" $ sizedFuncCompare (Old.insEdge (1,1,())) (G.insEdge (G.Edge 1 () 1)) 500
        , bgroup "large"  $ sizedFuncCompare (Old.insEdge (1,1,())) (G.insEdge (G.Edge 1 () 1)) 1000
        ]
      {- TODO: Figure out new gmap
      , bgroup "gmap" []
        [ bgroup "small"  $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 100
        , bgroup "medium" $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 500
        , bgroup "large"  $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 1000
        ]
      -}
      -- FGL nmap does not adjust the graph, merely change the value stored there
      -- this representational advantage is huge.
      , bgroup "nmap"
        [ bgroup "small"  $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 100
        , bgroup "medium" $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 500
        , bgroup "large"  $ sizedFuncCompare (Old.nmap (+1)) (G.nmap (+1)) 1000
        ]
      , bgroup "emap"
        [ bgroup "small"  $ sizedFuncCompare (Old.emap (const '1')) (G.emap (const '1')) 100
        , bgroup "medium" $ sizedFuncCompare (Old.emap (const '1')) (G.emap (const '1')) 500
        , bgroup "large"  $ sizedFuncCompare (Old.emap (const '1')) (G.emap (const '1')) 1000
        ]
      ]
    , bgroup "all"
      [ bgroup "match"
        [ bgroup "small"  $ sizedFuncCompare (Old.match 1) (G.match 1) 100
        , bgroup "medium" $ sizedFuncCompare (Old.match 1) (G.match 1) 500
        , bgroup "large"  $ sizedFuncCompare (Old.match 1) (G.match 1) 1000
        ]
      ]
    ]
  ]

-- | Compare building functions at different sizes
sizedBuild :: Int -> [Benchmark]
sizedBuild n
    = [ bench "old" $ nf (buildOld (oldEdges n)) (oldNodes n)
      , bench "new" $ nf (buildNew (newEdges n)) [1..n]
      , bench "new/list" $ nf G.fromList (listGraph n)
      ]

-- | Compare equivalent old and new functions applied to graphs of given size
sizedFuncCompare :: (NFData a, NFData b) => (Old.Gr Int () -> a) -> (G.Gr () Int -> b) -> Int -> [Benchmark]
sizedFuncCompare oldF newF n
    = let oldGraph = buildOld (oldEdges n) (oldNodes n)
          newGraph = buildNew (newEdges n) [1..n]
      in [ bench "old" $ nf oldF oldGraph
         , bench "new" $ nf newF newGraph ]


{-
 - Comparisons to make
 -
 - static:
 - - project to nodes and edges
 - - ordered match
 - - match any
 -
 - dynamic:
 - - insert node
 - - insert edge
 -
 - other:
 - - folds
-}

------------------------------
-- * Library benchmarks

library :: IO ()
library = defaultMain
  [ bgroup "library" []
  {-
    [ bench "empty"     $ whnf G.empty
    , bench "singleton" $ whnf G.singleton
    , bench "mkGraph"   $ whnf G.mkGraph

    -- Basic interface
    , bench "null"      $ whnf G.null
    , bench "order"     $ whnf G.order
    , bench "size"      $ whnf G.size
    , bench "match"     $ whnf G.match
    , bench "matchAny"  $ whnf G.matchAny
    , bench "(!)"       $ whnf G.(!)
    , bench "(!?)"      $ whnf G.(!?)
    , bench "nodes"     $ whnf G.nodes
    , bench "edges"     $ whnf G.edges

    -- Maps
    , bench "nmap"      $ whnf G.nmap
    , bench "emap"      $ whnf G.emap
    , bench "nemapH"    $ whnf G.nemapH

    -- Folds
    , bench "foldr"     $ whnf G.foldr

    -- Queries
    , bench "member"    $ whnf G.member
    , bench "neighbors" $ whnf G.neighbors
    , bench "preds"     $ whnf G.preds
    , bench "succs"     $ whnf G.succs
    , bench "inEdges"   $ whnf G.inEdges
    , bench "outEdges"  $ whnf G.outEdges
    , bench "inDegree"  $ whnf G.inDegree
    , bench "outDegree" $ whnf G.outDegree
    , bench "degree"    $ whnf G.degree
    , bench "hasEdge"   $ whnf G.hasEdge
    , bench "hasNeighbor" $ whnf G.hasNeighbor

    -- Filters
    , bench "nfilter"   $ whnf G.nfilter
    , bench "efilter"   $ whnf G.efilter

    -- Insertion and Deletion
    , bench "insNode"   $ whnf G.insNode
    , bench "safeInsNode" $ whnf G.safeInsNode
    , bench "delNode"   $ whnf G.delNode
    , bench "insEdge"   $ whnf G.insEdge
    , bench "delHeads"  $ whnf G.delHeads
    , bench "delTails"  $ whnf G.delTails
    , bench "insHead"   $ whnf G.insHead
    , bench "delHead"   $ whnf G.delHead
    , bench "insTail"   $ whnf G.insTail
    , bench "delTail"   $ whnf G.delTail
    ]
    -}
  ]

------------------------------
-- * Algorithm benchmarks


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

-- | Generate old edges from number of nodes
-- Exclude (1, 1, ()) for testing purposes
oldEdges :: Int -> [(Int, Int, ())]
oldEdges n = tail $ (\x y -> (x,y,())) <$> [1..n] <*> [1..n]

-- | Generate old nodes from number of nodes
oldNodes :: Int -> [(Int, Int)]
oldNodes n = map (\x -> (x,x)) [1..n]

-- | Generate new edges from number of nodes
-- Exclude (1, (), 1) for testing purposes
newEdges :: Int -> [G.Edge () Int]
newEdges n = tail $ (\x y -> G.Edge x () y) <$> [1..n] <*> [1..n]

listGraph :: Int -> [(Int, G.Context' () Int)]
listGraph n = [ (n', G.Context' (HS.fromList [G.Head () n'' | n'' <- [1..n]])
                                n'
                                (HS.fromList [G.Tail () n'' | n'' <- [1..n]])) | n' <- [1..n] ]
