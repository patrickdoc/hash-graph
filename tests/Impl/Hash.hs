{-# LANGUAGE FlexibleContexts #-}

module Impl.Hash
  ( hashTests
  ) where

import Data.AbstractGraph.Class
import Data.AbstractGraph.Impl.Hash

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

graphEq :: (Eq e) => HashG Bool e -> HashG Bool e -> Bool -> Bool -> Bool
graphEq g1 g2 = \n1 n2 -> sample g1 n1 n2 == sample g2 n1 n2

hashTests :: Spec
hashTests = describe "Hash Abstract Graphs" $ do
  let exE = [42] :: [Int]
      gEmpty = mempty :: HashG Bool [Int]
      g = update gEmpty True False exE :: HashG Bool [Int]
      gUp n1 n2 = update gEmpty n1 n2 exE :: HashG Bool [Int]
  describe "Total graph class" $ do

    -- Class base ops
    prop "constant is everywhere defined and equal" $
        \n1 n2 -> sample gEmpty n1 n2 == mempty

    prop "sample finds updates" $
        \n1 n2 -> sample (gUp n1 n2) n1 n2 === exE

    prop "updateWith uses 'f old new'" $
        \n1 n2 -> sample (updateWith (flip const) g n1 n2 exE) n1 n2 === exE

  -- Should implement Arbitrary for graph types to make this easier/better
  describe "Semigroup laws" $ do
    prop "Associativity" $ do
      let g1 = g
          g2 = update gEmpty True False [43]
          g3 = update gEmpty True False [44]

      graphEq (g1 <> (g2 <> g3))
              ((g1 <> g2) <> g3)

  describe "Monoid laws" $ do
    prop "Right identity" $
      graphEq (g <> gEmpty) g

    prop "Left identity" $
      graphEq (gEmpty <> g) g

    -- Only check if override
    -- prop "mconcat = foldr (<>) gEmpty" $

  describe "Functor laws" $ do
    prop "fmap id" $
      graphEq (fmap id g) g

    prop "fmap distributes over composition" $ do
      let f = sum
          h = map (*10)
      graphEq (fmap (f . h)  g) (fmap f (fmap h g))

  -- describe "Applicative laws" $ do
  --   prop "identity" $
  --     graphEq (pure id <*> g) g

  --   prop "composition" $ do
  --     let u = constant show :: HashG Bool (Int -> String)
  --         v = constant length :: HashG Bool ([Int] -> Int)
  --         w = g
  --     graphEq (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

  --   prop "homomorphism" $ do
  --     let f = length
  --         x = exE
  --     graphEq (pure f <*> pure x) (pure (f x))

  --   prop "interchange" $ do
  --     let y = exE
  --         u = constant length
  --     graphEq (u <*> pure y) (pure ($ y) <*> u)

  -- describe "Monad laws" $ do
  --   prop "return a >>= k = k a" $ do
  --     let k = return . show
  --     graphEq (return exE >>= k) (k exE)

  --   prop "m >>= return = m" $
  --     graphEq (g >>= return) g

  --   prop "m >>= (\\x -> k x >>= h) = (m >>= k) >>= h" $ do
  --     let k = return . fmap (*10)
  --         h = return . show
  --     graphEq (g >>= (\x -> k x >>= h)) ((g >>= k) >>= h)

