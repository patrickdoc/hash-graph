module Main where

import qualified Data.HashGraph.Strict as G
import qualified Data.HashGraph.Algorithms as GA

import Control.Exception (evaluate)
import qualified Data.HashSet as HS
import Data.List (nub, sort)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===))

type TestGraph = G.Gr Int Char

main :: IO ()
main = hspec $ do
    library
    algos


library :: Spec
library = describe "Strict Graphs" $ do

    -- Construction
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

    -- Basic Interface
    describe "Basic Interface" $ do
        describe "null" $
            it "is described throughout" $ True `shouldBe` True
        describe "order" $
            it "is described throughout" $ True `shouldBe` True
        describe "size" $
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
        --describe "(&)" $ do
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
        describe "nodes" $
            it "is described throughout" $ True `shouldBe` True
        describe "edges" $
            it "is described throughout" $ True `shouldBe` True

    -- Maps
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
        describe "edge map" $
            it "adjusts every edge" $
                G.edges (G.emap fe gr :: G.Gr Int Char)
                    `shouldBe` [ G.Edge 'b' (-1) 'b'
                               , G.Edge 'b' (-1) 'a'
                               , G.Edge 'a' (-1) 'b'
                               , G.Edge 'a' (-1) 'a' ]
        describe "node and Edge map" $
            it "is described by node map and edge map" $ True `shouldBe` True

    -- Folds
    describe "Folds" $
        describe "foldr" $
            prop "works like a list" $
                \ns ->
                    let graph = G.mkGraph [] ns :: TestGraph in
                    length (G.foldr (:) [] graph)
                          === length (foldr (:) [] (nub ns))

    -- Queries
    describe "Queries" $ do
        let edgeList ns = (\x y -> G.Edge x 1 y) <$> ns <*> ns
            gr = G.mkGraph (tail (edgeList "abcde")) "abcde"
        describe "member" $ do
            it "returns True for a node in the graph" $
                G.member 'a' gr `shouldBe` True
            it "returns False for a node not in the graph" $
                G.member 'z' gr `shouldBe` False
        describe "neighbors" $ do
            it "returns `Just lst` for a node in the graph" $
                G.neighbors 'a' gr `shouldBe` Just "bcde"
            it "returns `Nothing` for a node not in the graph" $
                G.neighbors 'z' gr `shouldBe` Nothing
        describe "preds" $ do
            it "returns `Just lst` for a node in the graph" $
                G.preds 'a' gr `shouldBe` Just "bcde"
            it "returns `Nothing` for a node not in the graph" $
                G.preds 'z' gr `shouldBe` Nothing
        describe "succs" $ do
            it "returns `Just lst` for a node in the graph" $
                G.succs 'a' gr `shouldBe` Just "bcde"
            it "returns `Nothing` for a node not in the graph" $
                G.succs 'z' gr `shouldBe` Nothing
        describe "inEdges" $ do
            it "returns `Just lst` for a node in the graph" $
                G.inEdges 'a' gr `shouldBe` (Just [ G.Edge 'b' 1 'a'
                                                  , G.Edge 'c' 1 'a'
                                                  , G.Edge 'd' 1 'a'
                                                  , G.Edge 'e' 1 'a' ] :: Maybe [G.Edge Int Char])
            it "returns `Nothing` for a node not in the graph" $
                G.inEdges 'z' gr `shouldBe` Nothing
        describe "outEdges" $ do
            it "returns `Just lst` for a node in the graph" $
                G.outEdges 'a' gr `shouldBe` (Just [ G.Edge 'a' 1 'b'
                                                   , G.Edge 'a' 1 'c'
                                                   , G.Edge 'a' 1 'd'
                                                   , G.Edge 'a' 1 'e' ] :: Maybe [G.Edge Int Char])
            it "returns `Nothing` for a node not in the graph" $
                G.outEdges 'z' gr `shouldBe` Nothing
        describe "inDegree" $ do
            it "returns `Just int` for a node in the graph" $
                G.inDegree 'b' gr `shouldBe` Just 5
            it "returns `Nothing` for a node not in the graph" $
                G.inDegree 'z' gr `shouldBe` Nothing
        describe "outDegree" $ do
            it "returns `Just int` for a node in the graph" $
                G.outDegree 'b' gr `shouldBe` Just 5
            it "returns `Nothing` for a node not in the graph" $
                G.outDegree 'z' gr `shouldBe` Nothing
        describe "degree" $ do
            it "returns `Just int` for a node in the graph" $
                G.degree 'b' gr `shouldBe` Just 10
            it "returns `Nothing` for a node not in the graph" $
                G.degree 'z' gr `shouldBe` Nothing
        describe "hasEdge" $ do
            it "returns `True` for an edge in the graph" $
                G.hasEdge (G.Edge 'a' 1 'b' :: G.Edge Int Char) gr `shouldBe` True
            it "returns `False` for an edge not in the graph" $
                G.hasEdge (G.Edge 'a' 1 'a' :: G.Edge Int Char) gr `shouldBe` False
        describe "hasNeighbor" $ do
            it "returns `True` if the second node is a neighbor of the first" $
                G.hasNeighbor 'a' 'b' gr `shouldBe` True
            it "returns `False` otherwise" $
                G.hasNeighbor 'z' 'a' gr `shouldBe` False

    -- Filters
    describe "Filters" $ do
        let edgeList ns = (\x y -> G.Edge x 1 y) <$> ns <*> ns
            gr = G.mkGraph (tail (edgeList "abcde")) "abcde" :: TestGraph
        describe "node filter" $
            prop "filters" $
                \ns -> sort (G.nodes (G.nfilter (/= 'a') (G.mkGraph [] ns :: TestGraph))) === filter (/= 'a') (sort (nub ns))
        describe "edge filter" $
            it "filters edges" $
                G.edges (G.efilter (\(G.Edge x _ _) -> x == 'a') gr) `shouldBe` [ G.Edge 'a' 1 'b'
                                                                                , G.Edge 'a' 1 'c'
                                                                                , G.Edge 'a' 1 'd'
                                                                                , G.Edge 'a' 1 'e'
                                                                                ]
{-
    -- Insertion and Deletion
    describe "Insertion and Deletion" $ do
        describe "insNode" $ do
        describe "safeInsNode" $ do
        describe "delNode" $ do
        describe "insEdge" $ do
-}

algos :: Spec
algos = describe "algorithms" $ do
    let es = [ G.Edge 'a' 1 'b', G.Edge 'a' 1 'c', G.Edge 'b' 1 'd', G.Edge 'b' 1 'e', G.Edge 'c' 1 'f', G.Edge 'c' 1 'g' ]
        lineEs = [ G.Edge 'a' 1 'b', G.Edge 'b' 1 'c', G.Edge 'c' 1 'd' ]
        treeGraph = G.mkGraph es "abcdefg" :: TestGraph
        lineGraph = G.mkGraph lineEs "abcd" :: TestGraph
    describe "bfs" $
        it "creates the correct list" $
            GA.bfs treeGraph `shouldBe` ['a','b','c','f','g','d','e']
    describe "dfs" $
        it "creates the correct list" $
            GA.dfs treeGraph `shouldBe` ['a','b','d','e','c','f','g']
    describe "topSort" $
        it "sorts a line" $
            GA.topSort lineGraph `shouldBe` Just ['a','b','c','d']
    describe "topSort" $
        it "sorts a tree" $
            GA.topSort treeGraph `shouldBe` Just ['a','c','g','f','b','e','d']
