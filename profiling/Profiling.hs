module Main where

import Control.DeepSeq
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Graph.Inductive.Strict

main :: IO ()
main = do
-- Profile current implementation
--    print $ buildCompleteStatic 1000
-- Profile HashMap.fromList for theoretical max
--    print $!! buildBigHashMap

buildBigHashMap :: Gr Char Int
buildBigHashMap = Gr $ HM.fromList [(ids, Context' (HS.fromList heads)
                                                   ids
                                                   (HS.fromList tails)) | ids <- [1..1000]]
  where
    heads = [ Head 'a' n | n <- [1..1000] ]
    tails = [ Tail 'a' n | n <- [1..1000] ]

-- | Build a complete static graph using mkGraph
buildCompleteStatic :: Int -> Gr Char Int
buildCompleteStatic n = mkGraph es ids
  where
    ids = [1..n] 
    es = [ Edge a 'a' b | a <- [1..n], b <- [1..n] ]
