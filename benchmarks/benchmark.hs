module Main where

import Data.Graph.Inductive.Impl.PatriciaTree

import Control.DeepSeq (rnf)
import Criterion.Main
import Microbench

main :: IO ()
main = do --micro
          crit

{-
-- | microbenchmarks to match the original FGL library
micro :: IO ()
micro = do microbench "buildFull into Patricia tree 100" (buildMicro 100)
           microbench "buildFull into Patricia tree 500" (buildMicro 500)
           microbench "buildFull into Patricia tree 1000" (buildMicro 1000)
           --microbench "insNode into Patricia Tree" insNodePatricia
           --microbench "insEdge into Patricia tree" insEdgePatricia
           --microbench "gmap on Patricia tree" gmapPatricia
           --microbench "node mapping on Patricia tree" nmapPatricia
           --microbench "edge mapping on Patricia tree" emapPatricia

buildMicro :: Int -> Int -> ()
buildMicro sz times = rnf [rnf (buildCompleteStatic sz) | i <- [1..times]]
-}

-- | Criterion benchmarks
crit :: IO ()
crit = defaultMain [
    let input n = (\p s -> Edge p 'a' s) <$> [1..n] <*> [1..n]
        small = input 100
        med = input 500
        large = input 1000
    in bgroup "static" [
      bench "100"   $ nf (buildCompleteStatic small) 100
    , bench "500"   $ nf (buildCompleteStatic med) 500
    , bench "1000"  $ nf (buildCompleteStatic large) 1000
    , bench "insNode"
    ]
  ]
-- | Build a complete static graph using mkGraph
buildCompleteStatic :: [Edge Char Int] -> Int -> Maybe (Gr Char Int)
buildCompleteStatic es n = mkGraph es [1..n]

-- | Build a complete dynamic graph using (&)
buildCompleteDynamic :: Int -> Gr a b
buildCompleteDynamic = undefined


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
