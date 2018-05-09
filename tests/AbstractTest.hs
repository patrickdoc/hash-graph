module Main where

import Data.AbstractGraph.Class

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))

main :: IO ()
main = hspec library

-- it "does a thing" $
--     x `shouldBe` y
-- prop "law holds" $
--     \x -> id (f x) === f (id x)

graphEq :: (Graph g n e) => g n e -> g n e -> Property
graphEq g1 g2 = \n1 n2 -> sample n1 n2 g1 == sample n1 n2 g2

library :: Spec
library = describe "Abstract Graphs" $ do
  let exE = [42] :: [Int]
      g = update True False mempty exE
  describe "Total graph class" $ do

    -- Class base ops
    prop "constant is everywhere defined and equal" $
        \n1 n2 -> sample mempty n1 n2 == mempty

    prop "sample finds updates" $
        let g1 n1 n2 = update mempty n1 n2 exE
        in \n1 n2 -> sample (g n1 n2) n1 n2 == exE

    prop "updateWith uses 'f old new'" $
        \n1 n2 -> sample (updateWith (flip const) n1 n2 exE) n1 n2 == exE

  -- Should implement Arbitrary for graph types to make this easier/better
  describe "Semigroup laws" $ do
    prop "Associativity" $ do
      let g1 = g
          g2 = update True False [43] mempty
          g3 = update True False [44] mempty

      graphEq (g1 <> (g2 <> g3))
              ((g1 <> g2) <> g3)

  describe "Monoid laws" $ do
    prop "Right identity" $
      graphEq (g <> mempty) g

    prop "Left identity" $
      graphEq (mempty <> g) g

    -- Only check if override
    -- prop "mconcat = foldr (<>) mempty" $

  describe "Functor laws" $ do
    prop "fmap id" $
      graphEq (fmap id g) g

    prop "fmap distributes over composition" $ do
      let f [x] = x
          h [x] = [x*10]
      graphEq (fmap (f . h)  g) (fmap f (fmap h g))

  describe "Applicative laws" $ do
    prop "identity" $
      graphEq (pure id <*> g) g
      
    prop "composition" $
      graphEq (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))
      
    prop "homomorphism" $
      graphEq (pure f <*> pure x) (pure (f x))
      
    prop "interchange" $
      graphEq (u <*> pure y) (pure ($ y) <*> u)

  describe "Monad laws" $ do
    prop "return a >>= k = k a" $ do
      -- k :: a -> g n String
      let k = return . show
      graphEq (return exE >>= k) (k exE)

    prop "m >>= return = m" $
      graphEq (g >>= return) g
    
    prop "m >>= (\\x -> k x >>= h) = (m >>= k) >>= h" $ do
      let k = return . fmap (*10)
          h = return . show
      graphEq (g >>= (\x -> k x >>= h)) ((g >>= k) >>= h)

