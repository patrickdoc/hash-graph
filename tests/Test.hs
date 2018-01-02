module Main where

import qualified Data.Graph.Inductive.Strict as G

import qualified Data.HashSet as HS
import Data.List (nub, sort)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===))

type TestGraph = G.Gr Int Char

main :: IO ()
main = hspec $
    describe "Static tests" $ do
        describe "empty" $ do
            let e = G.empty :: TestGraph
            it "is null" $
                G.null e `shouldBe` True
            it "has no nodes" $
                G.nodes e `shouldBe` []
            it "has no edges" $
                G.edges e `shouldBe` []
            it "has order 0" $
                G.order e `shouldBe` 0
        describe "mkGraph" $ do
            let e = G.empty :: TestGraph
                eGraph = G.mkGraph [] [] :: TestGraph
            it "makes the empty graph" $
                eGraph `shouldBe` e
            prop "makes the correct number of nodes" $
                \ns -> G.order (G.mkGraph [] ns :: TestGraph) === length (nub ns)
            prop "makes the correct set of nodes" $
                \ns -> let newNodes = sort (nub ns)
                           nodeList = G.nodes (G.mkGraph [] newNodes :: TestGraph)
                       in sort (nub nodeList) === newNodes
            --prop "makes the correct number of edges" $
            --    \ns -> 
        describe "match" $ do
            let gr = G.mkGraph [] ['a'] :: TestGraph
            it "finds and removes nodes in the graph" $
                G.match 'a' gr `shouldBe` Just (G.Context' HS.empty 'a' HS.empty, G.empty)
            it "doesn't find nodes not in the graph" $
                G.match 'b' gr `shouldBe` Nothing
