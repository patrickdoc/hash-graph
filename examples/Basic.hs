-- Create and print a simple graph
--
-- Nodes are city names
-- Edges are distances between the cities

module Main where

import Data.HashGraph.Strict (Edge(..), mkGraph, pretty)

main :: IO ()
main = putStr $ pretty $ mkGraph es ns

ns :: [String]
ns = [ "Boston"
     , "Chicago"
     , "Los Angeles"
     ]

es :: [Edge Int String]
es = [ Edge "Chicago" 1000 "Boston"
     , Edge "Chicago" 2000 "Los Angeles"
     , Edge "Boston"  3000 "Los Angeles"
     ]
