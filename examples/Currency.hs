{-# LANGUAGE DataKinds #-}

-- This module demonstrates an exchange rate graph
-- as discussed in issues #3 and #4.

module Main where

import Data.HashGraph.Strict
import Data.HashGraph.Algorithms (pathTree, primAt)

import Data.Maybe (fromJust)
import Money (ExchangeRate, SomeExchangeRate, exchangeRate, toSomeExchangeRate)

main :: IO ()
main = do
    let graph = mkGraph es ns
    print $ pathTree "EUR" graph
    putStr "\nAlternatively\n"
    print $ minPath "EUR" "GBP" graph


-- Nodes == Currencies
ns :: [String]
ns = [ "EUR", "GBP", "JPY", "USD"]

-- Edges == ExchangeRates
es :: [Edge SomeExchangeRate String]
es = [ Edge "EUR" eToU "USD"
     , Edge "USD" uToG "GBP"
     , Edge "EUR" eToJ "JPY"
     , Edge "JPY" jToG "GBP"
     ]

eToU :: SomeExchangeRate
eToU =
    toSomeExchangeRate $ fromJust $ (exchangeRate 1.2   :: Maybe (ExchangeRate "EUR" "USD"))

uToG :: SomeExchangeRate
uToG =
    toSomeExchangeRate $ fromJust $ (exchangeRate 0.6   :: Maybe (ExchangeRate "USD" "GBP"))

eToJ :: SomeExchangeRate
eToJ =
    toSomeExchangeRate $ fromJust $ (exchangeRate 136   :: Maybe (ExchangeRate "EUR" "JPY"))

jToG :: SomeExchangeRate
jToG =
    toSomeExchangeRate $ fromJust $ (exchangeRate 0.006 :: Maybe (ExchangeRate "JPY" "GBP"))

minPath :: String -> String -> Gr SomeExchangeRate String -> [Edge SomeExchangeRate String]
minPath src dst g = go dst []
  where
    go n pth = case inEdges n minG of
        Just [] -> pth
        Just [e@(Edge p _ _)] -> go p (e : pth)
        _ -> error "primAt is broken"
    minG = primAt src g
