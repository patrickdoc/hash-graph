{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- This module sets up a simple graph of courses and prereqs.
-- Each node is a course, and each edge is labeled with the
-- difficulty of progressing along that edge.

module Main where

import Data.HashGraph.Strict
import Data.HashGraph.Algorithms

main :: IO ()
main = do
    putStr $ pretty $ primAt pythonAndTerm courseGraph

type Course = String
type Difficulty = Int

-- | The graph will have edges labeled with /Difficulty/'s
-- and nodes labeled with /Course/'s
courseGraph :: Gr Difficulty Course
courseGraph = mkGraph preReqs courses

-- Nodes

-- | A list of all the courses
courses :: [Course]
courses
    = [ introC
      , pythonAndTerm
      , bashAndIntermTerm
      , networking
      , git
      , ml
      , stat
      , discMath
      , formal
      , crypto
      , arch
      , haskell
      , compilers
      , gc
      , rust
      , testing
      , automation
      , os
      , distSys
      , building
      , algo
      , dataStruct
      , perfAndOpt
      , pkgAndLib
      , monitoring
      , cpuGraphics
      , linAlg
      , gpuGraphics
      ]

-- Course definitions
introC              = "An Introduction to C"
pythonAndTerm       = "Python and the Terminal"
bashAndIntermTerm   = "Bash and Intermediate Terminal"
networking          = "Networking"
git                 = "Git"
ml                  = "Machine Learning"
stat                = "Statistics"
discMath            = "Discrete Math"
formal              = "Formal Languages"
crypto              = "Cryptography"
arch                = "Architecture"
haskell             = "Haskell"
compilers           = "Compilers"
gc                  = "Garbage Collection"
rust                = "Rust"
testing             = "Testing"
automation          = "Automation"
os                  = "Operating Systems"
distSys             = "Distributed Systems"
building            = "Building a Computer"
algo                = "Algorithms"
dataStruct          = "Data Structures"
perfAndOpt          = "Performance and Optimization"
pkgAndLib           = "Package Management and Working with Libraries"
monitoring          = "Monitoring"
cpuGraphics         = "CPU Graphics"
linAlg              = "Linear Algebra"
gpuGraphics         = "GPU Graphics"


-- Edges

-- | A list of the edges of the graph
--
-- These /Edge/'s connect two /Course/'s with a /Difficulty/ label
preReqs :: [Edge Difficulty Course]
preReqs =
    [ Edge introC             1   bashAndIntermTerm
    , Edge introC             1   networking
    , Edge introC             1   crypto
    , Edge introC             1   arch
    , Edge introC             1   haskell
    , Edge introC             1   algo
    , Edge introC             1   dataStruct
    , Edge pythonAndTerm      1   introC
    , Edge pythonAndTerm      1   bashAndIntermTerm
    , Edge pythonAndTerm      1   testing
    , Edge pythonAndTerm      1   algo
    , Edge pythonAndTerm      1   pkgAndLib
    , Edge bashAndIntermTerm  1   networking
    , Edge bashAndIntermTerm  1   git
    , Edge bashAndIntermTerm  1   crypto
    , Edge bashAndIntermTerm  1   arch
    , Edge bashAndIntermTerm  1   haskell
    , Edge bashAndIntermTerm  1   testing
    , Edge bashAndIntermTerm  1   algo
    , Edge bashAndIntermTerm  1   dataStruct
    , Edge networking         1   git
    , Edge networking         1   crypto
    , Edge networking         1   arch
    , Edge networking         1   haskell
    , Edge networking         1   testing
    , Edge networking         1   os
    , Edge networking         1   dataStruct
    , Edge haskell            1   compilers
    , Edge haskell            1   gc
    , Edge haskell            1   testing
    , Edge arch               1   building
    , Edge perfAndOpt         1   monitoring
    , Edge pkgAndLib          1   automation
    , Edge stat               1   ml
    , Edge linAlg             1   cpuGraphics
    ]
