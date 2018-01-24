-- This module shows you how to go fast.
--
-- This requires you to use some internal types,
-- which I am working on improving

module Main where

import Data.HashGraph.Strict (Context'(..), Head(..), Tail(..), fromList, size)
import qualified Data.HashSet as HS

main :: IO ()
main = print $ size $ fromList contexts 

-- HashGraph.fromList is a tiny wrapper around HashMap.fromList,
-- so we have to emulate a HashMap entry. Thus we have the node (b)
-- and it's context.
--
-- This list of contexts creates a graph with 1000 nodes, and
-- every possible edge between them.
contexts :: [(Int, Context' () Int)]
contexts = [ (n, Context' ps n ss) | n <- [1..1000] ]
  where
    -- A HashSet of half edges to predecessors.
    -- Edges labeled with () are effectively unlabeled.
    ps = HS.fromList [ Head () n | n <- [1..1000] ]
    -- A HashSet of half edges to successors.
    ss = HS.fromList [ Tail () n | n <- [1..1000] ]
