module Main where

import qualified Data.Graph.Inductive.Strict as G
import qualified Data.Graph.Inductive.PatriciaTree as Old
import qualified Data.Graph.Inductive.Graph as Old

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
  [ bgroup "Comparisons"
    [ env (nsAndEs 100) (\ ~(oldEs,oldNs,newEs) -> bgroup "build/small"
      [ bench "old" $ nf (buildOld oldEs) oldNs
      , bench "new" $ nf (buildNew newEs) [1..100]
      ])
    , env (nsAndEs 500) (\ ~(oldEs,oldNs,newEs) -> bgroup "build/medium"
      [ bench "old" $ nf (buildOld oldEs) oldNs
      , bench "new" $ nf (buildNew newEs) [1..500]
      ])
    , env (nsAndEs 1000) (\ ~(oldEs,oldNs,newEs) -> bgroup "build/large"
      [ bench "old" $ nf (buildOld oldEs) oldNs
      , bench "new" $ nf (buildNew newEs) [1..1000]
      ])
    , bgroup "insert node" []
    , bgroup "insert edge" []
    , bgroup "gmap" []
    -- FGL nmap does not adjust the graph, merely change the value stored there
    -- this representational advantage is huge.
    , env graphs (\ ~(oldSmall, oldLarge, newSmall, newLarge) -> bgroup "nmap"
      [ bench "small/old" $ nf (Old.nmap (+1)) oldSmall
      , bench "small/new" $ nf (G.nmap (+1)) newSmall
      , bench "large/old" $ nf (Old.nmap (+1)) oldLarge
      , bench "large/new" $ nf (G.nmap (+1)) newLarge
      ])
    , env graphs (\ ~(oldSmall, oldLarge, newSmall, newLarge) -> bgroup "emap"
      [ bench "small/old" $ nf (Old.emap (\_ -> '1')) oldSmall
      , bench "small/new" $ nf (G.emap (\_ -> '1')) newSmall
      , bench "large/old" $ nf (Old.emap (\_ -> '1')) oldLarge
      , bench "large/new" $ nf (G.emap (\_ -> '1')) newLarge
      ])
    ]
  ]


{-
 - Comparisons to make
 -
 - static:
 - - use mkGraph to construct a complete graph of size 100 500 1000
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
 - - maps
-}

------------------------------
-- * Library benchmarks

library :: IO ()
library = defaultMain
  [ bgroup "Library"
    []
  ]

------------------------------
-- * Algorithm benchmarks


------------------------------
-- Utilities

-- | Create the inputs for complete graph construction
nsAndEs :: Int -> IO ([(Int, Int, ())], [(Int, Int)], [G.Edge () Int])
nsAndEs n = return (oldEdges n, oldNodes n, newEdges n)

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
buildNew es ns = G.mkGraph es ns

-- | Build a complete graph using fgl
buildOld :: [Old.LEdge ()] -> [Old.LNode Int] -> Old.Gr Int ()
buildOld es ns = Old.mkGraph ns es

-- | Generate old edges from number of nodes
oldEdges :: Int -> [(Int, Int, ())]
oldEdges n = (\x y -> (x,y,())) <$> [1..n] <*> [1..n]

-- | Generate old nodes from number of nodes
oldNodes :: Int -> [(Int, Int)]
oldNodes n = map (\x -> (x,x)) [1..n]

-- | Generate new edges from number of nodes
newEdges :: Int -> [G.Edge () Int]
newEdges n = (\x y -> G.Edge x () y) <$> [1..n] <*> [1..n]
