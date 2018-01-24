-- This module demonstrates the inductive API similar to FGL's interface.

module Main where

import Data.HashGraph.Strict (Gr, Context'(..), (&), empty, matchAny)
import qualified Data.HashSet as HS

main :: IO ()
main = deconstruct graph

graph :: Gr String Int
graph = Context' HS.empty 1 HS.empty &
        Context' HS.empty 3 HS.empty &
        Context' HS.empty 2 HS.empty & empty

deconstruct :: Gr String Int -> IO ()
deconstruct g = case matchAny g of
    Just (Context' _ node _, g') -> do print node
                                       deconstruct g'
    Nothing -> putStrLn "Graph is empty"
