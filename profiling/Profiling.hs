module Main where

import Control.DeepSeq
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Graph.Inductive.Impl.PatriciaTree

main :: IO ()
main = do
--    print $!! ([ SmallHead 'a' n | n <- [1..1000000] ] :: [ SmallHead ])
--    print $!! buildCompleteStatic 1000
--    print $!! buildBigHashMap
--    print $ order hm
--  where
--    hm :: Gr Char Int
--    hm = Gr $ foldl' (\a b -> insHead b 1 a) (HM.fromList [(1, (Context' HS.empty 1 HS.empty))]) [ Head 'a' n | n <- [1..10000] ]

buildBigHashMap :: Gr Char Int
buildBigHashMap = Gr $ HM.fromList [(ids, (Context' (HS.fromList heads)
                                                    ids
                                                    (HS.fromList tails))) | ids <- [1..1000]]
  where
    heads = [ Head 'a' n | n <- [1..1000] ]
    tails = [ Tail 'a' n | n <- [1..1000] ]

-- | Build a complete static graph using mkGraph
buildCompleteStatic :: Int -> Gr Char Int
buildCompleteStatic n = mkGraph es ids
  where
    ids = [1..n] 
    es = [ Edge a 'a' b | a <- [1..n], b <- [1..n] ]
