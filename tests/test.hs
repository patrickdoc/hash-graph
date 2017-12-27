module Main where

import Data.Graph.Inductive.Impl.PatriciaTree

import qualified Data.HashSet as HS
import Data.List (nub, sort)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===))

type TestGraph = Gr Int Char

main :: IO ()
main = hspec $ do
    describe "Static tests" $ do
        describe "empty" $ do
            let e = (empty :: TestGraph)
            it "isEmpty" $
                isEmpty e `shouldBe` True
            it "has no nodes" $
                nodes e `shouldBe` []
            it "has no edges" $
                edges e `shouldBe` []
            it "has order 0" $
                order e `shouldBe` 0
        describe "mkGraph" $ do
            let e = (empty :: TestGraph)
                eGraph = (mkGraph [] [] :: Maybe TestGraph)
            it "makes the empty graph" $
                eGraph `shouldBe` Just e
            prop "makes the correct number of nodes" $
                \ns -> fmap order (mkGraph [] ns :: Maybe TestGraph) === Just (length (nub ns))
            prop "makes the correct set of nodes" $
                \ns -> let newNodes = sort (nub ns)
                           (Just nodeList) = fmap nodes (mkGraph [] newNodes :: Maybe TestGraph)
                       in sort (nub nodeList) === newNodes
            --prop "makes the correct number of edges" $
            --    \ns -> 
        describe "match" $ do
            let (Just gr) = (mkGraph [] ['a'] :: Maybe TestGraph)
            it "finds and removes nodes in the graph" $
                match 'a' gr `shouldBe` Just (Context' HS.empty 'a' HS.empty, empty)
            it "doesn't find nodes not in the graph" $
                match 'b' gr `shouldBe` Nothing
