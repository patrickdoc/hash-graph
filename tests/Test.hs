module Main where

import qualified Data.Graph.Inductive.Strict as G

import Control.Exception (evaluate)
import qualified Data.HashSet as HS
import Data.List (nub, sort)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===))

type TestGraph = G.Gr Int Char

main :: IO ()
main = hspec $ describe "Strict Graphs" $ do
    describe "Construction" $ do
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
            it "has size 0" $
                G.size e `shouldBe` 0
        describe "singleton" $ do
            let g = G.singleton 'a' :: TestGraph
            it "is not null" $
                G.null g `shouldBe` False
            it "has one node" $
                G.nodes g `shouldBe` ['a']
            it "has no edges" $
                G.edges g `shouldBe` []
            it "has order 1" $
                G.order g `shouldBe` 1
            it "has size 0" $
                G.size g `shouldBe` 0
        describe "mkGraph" $ do
            let e = G.empty :: TestGraph
                edgeList ns = (\x y -> G.Edge x 1 y) <$> ns <*> ns
            it "makes the empty graph" $
                (G.mkGraph [] [] :: TestGraph) `shouldBe` e
            prop "makes the correct number of nodes" $
                \ns -> G.order (G.mkGraph [] ns :: TestGraph) === length (nub ns)
            prop "makes the correct set of nodes" $
                \ns -> let nodeList = G.nodes (G.mkGraph [] ns :: TestGraph)
                       in sort nodeList === sort (nub ns)
            prop "makes the correct number of edges" $
                \ns -> let graph = G.mkGraph (edgeList ns) ns :: TestGraph
                       in G.size graph === length (nub ns) * length (nub ns)
            prop "makes the correct set of edges" $
                \ns -> let es = G.edges (G.mkGraph (edgeList ns) ns :: TestGraph)
                       in HS.fromList es === HS.fromList (edgeList ns)
    describe "Basic Interface" $ do
        describe "null" $ do
            it "is described throughout" $ True `shouldBe` True
        describe "order" $ do
            it "is described throughout" $ True `shouldBe` True
        describe "size" $ do
            it "is described throughout" $ True `shouldBe` True
        describe "match" $ do
            let gr = G.mkGraph [] ['a'] :: TestGraph
            it "finds and removes nodes in the graph" $
                G.match 'a' gr `shouldBe` Just (G.Context' HS.empty 'a' HS.empty, G.empty)
            it "doesn't find nodes not in the graph" $
                G.match 'b' gr `shouldBe` Nothing
        describe "matchAny" $ do
            let gr = G.mkGraph [] ['a'] :: TestGraph
            it "finds nodes when the graph is non-empty" $
                G.matchAny gr `shouldBe` Just (G.Context' HS.empty 'a' HS.empty, G.empty)
            it "does not find a node when the graph is empty" $
                G.matchAny (G.empty :: TestGraph) `shouldBe` Nothing
{-
        describe "(&)" $ do
-}
        describe "(!)" $ do
            let gr = G.mkGraph [] ['a'] :: TestGraph
            it "finds nodes in the graph" $
                gr G.! 'a' `shouldBe` G.Context' HS.empty 'a' HS.empty
            it "doesn't find nodes not in the graph" $
                evaluate (gr G.! 'b') `shouldThrow` errorCall "Data.Graph.Inductive.Strict.(!): node not found"
        describe "(!?)" $ do
            let gr = G.mkGraph [] ['a'] :: TestGraph
            it "finds nodes in the graph" $
                gr G.!? 'a' `shouldBe` Just (G.Context' HS.empty 'a' HS.empty)
            it "doesn't find nodes not in the graph" $
                gr G.!? 'b' `shouldBe` Nothing
        describe "nodes" $ do
            it "is described throughout" $ True `shouldBe` True
        describe "edges" $ do
            it "is described throughout" $ True `shouldBe` True
    describe "Maps" $ do
        let fn = (:[])
            fe = (*(-1))
            edgeList ns = (\x y -> G.Edge x 1 y) <$> ns <*> ns
            gr = G.mkGraph (edgeList "ab") "ab"
        describe "node map" $ do
            it "adjusts every node" $
                G.nodes (G.nmap fn gr :: G.Gr Int String) `shouldBe` ["a","b"]
            it "adjusts every edge" $
                G.edges (G.nmap fn gr :: G.Gr Int String)
                    `shouldBe` [ G.Edge "b" 1 "b"
                               , G.Edge "b" 1 "a"
                               , G.Edge "a" 1 "b"
                               , G.Edge "a" 1 "a" ]
        describe "edge map" $ do
            it "adjusts every edge" $
                G.edges (G.emap fe gr :: G.Gr Int Char)
                    `shouldBe` [ G.Edge 'b' (-1) 'b'
                               , G.Edge 'b' (-1) 'a'
                               , G.Edge 'a' (-1) 'b'
                               , G.Edge 'a' (-1) 'a' ]
        describe "node and Edge map" $ do
            it "is described by node map and edge map" $ True `shouldBe` True
{-
    describe "Folds" $ do
        describe "foldr" $ do
    describe "Queries" $ do
        describe "member" $ do
        describe "neighbors" $ do
        describe "preds" $ do
        describe "succs" $ do
        describe "inEdges" $ do
        describe "outEdges" $ do
        describe "inDegree" $ do
        describe "outDegree" $ do
        describe "degree" $ do
        describe "hasEdge" $ do
        describe "hasNeighbor" $ do
    describe "Filters" $ do
        describe "node filter" $ do
        describe "edge filter" $ do
    describe "Insertion and Deletion" $ do
-}
