module Data.HashGraph.Arbitrary where

import Data.Hashable
import Data.HashGraph.Strict
import Data.List (nub)
import Test.QuickCheck (Arbitrary(..), listOf)

-- | A complete graph, where all nodes have an edge to all other nodes.
-- Simulated undirected by having one edge each way.
newtype CompleteGraph a b = CG (Gr a b)

-- | A graph shaped like a binary tree.
newtype BTreeGraph a b = BTG (Gr a b)

-- | A circular graph, where there are 

instance (Arbitrary a, Arbitrary b, Eq a, Eq b, Hashable a, Hashable b) => Arbitrary (CompleteGraph a b) where
    arbitrary = do
        ns <- nub <$> listOf arbitrary
        eLabel <- arbitrary
        let es = (\x y -> Edge x eLabel y) <$> ns <*> ns
        return $ CG $ mkGraph es ns

instance (Arbitrary a, Arbitrary b, Eq a, Eq b, Hashable a, Hashable b) => Arbitrary (BTreeGraph a b) where
    arbitrary = do
        ns <- nub <$> listOf arbitrary
        eLabel <- arbitrary
        let len = length ns
            es = [ Edge (ns!!x) eLabel (ns!!y) | x <- [0..len-1], y <- [2*x+1, 2*x+2], y < len ]
        return $ BTG $ mkGraph es ns
