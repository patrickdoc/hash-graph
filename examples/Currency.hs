{-# LANGUAGE DataKinds #-}

-- Given a list of exchange rates, calculate all possible paths
--  from "EUR" (Euro) to "AUD" (Australian dollar).
-- With the given list of exchange rates, there will be multiple possible paths,
--  but one will offer a lower exchange rate than the others.
-- See also issues #3 and #4.

module Main where

import Data.HashGraph.Strict        (Edge(..), mkGraph)
import Data.HashGraph.Algorithms    (pathTree)

import Data.Maybe                   (fromJust, fromMaybe)
import Data.List                    (nub, sortOn, intercalate)
import qualified Money              as M
import GHC.TypeLits                 (KnownSymbol)
import Text.Printf                  (printf)


main :: IO ()
main = do
    let graph = mkGraph edges (nodes edges)
        paths = filter audEndPath (pathTree "EUR" graph)    -- All paths going from "EUR" to "AUD"
        audEndPath = (\(Edge _ _ dst) -> dst == "AUD") . last
        edgeData (Edge _ ed _) = ed
        -- Fail if, for two adjacent edges, the destination edge of the first edge
        --  doesn't match the source edge of the second edge
        nothingFail eL = fromMaybe (error $ "Edges not in proper order: " ++ show eL)
        composedRate edgeL = nothingFail edgeL $ composeRates $ map edgeData edgeL
        prettyPath edgeL   = intercalate "->" $ nodes edgeL          -- Pretty path, e.g. "EUR->JPY->GBP"
        pathRatePair edgeL = (composedRate edgeL, prettyPath edgeL)  -- Pair of rate and path
        sortedPairs = reverse $ sortOn fst (map pathRatePair paths)
        prettyPair (er, path) = printf "%.8g: %s" (fromRational $ M.someExchangeRateRate er :: Double) path :: String
    putStrLn "Final exchange rate, and exchange rate path, in order of highest exchange rate:"
    mapM_ putStrLn (map prettyPair sortedPairs)

-- | Our exchange rates of interest
exchangeRates :: [M.SomeExchangeRate]
exchangeRates =
   [ toER (M.exchangeRate 1.25   :: Maybe (M.ExchangeRate "EUR" "USD"))
   , toER (M.exchangeRate 0.71   :: Maybe (M.ExchangeRate "USD" "GBP"))
   , toER (M.exchangeRate 132.74 :: Maybe (M.ExchangeRate "EUR" "JPY"))
   , toER (M.exchangeRate 0.0067 :: Maybe (M.ExchangeRate "JPY" "GBP"))
   , toER (M.exchangeRate 1.76   :: Maybe (M.ExchangeRate "GBP" "CAD"))
   , toER (M.exchangeRate 8.95   :: Maybe (M.ExchangeRate "GBP" "CNY"))
   , toER (M.exchangeRate 0.2    :: Maybe (M.ExchangeRate "CNY" "AUD"))
   , toER (M.exchangeRate 1.01   :: Maybe (M.ExchangeRate "CAD" "AUD"))
   ] where toER :: (KnownSymbol src, KnownSymbol dst)
                => Maybe (M.ExchangeRate src dst)
                -> M.SomeExchangeRate
           toER = M.toSomeExchangeRate . fromJust

-- | Get a list of nodes along a path of non-cyclical edges
nodes :: [Edge M.SomeExchangeRate String] -> [String]
nodes = nub . concat . map toVertices -- Get all vertices and remove duplicates
   where toVertices (Edge v1 _ v2) = [v1, v2]

-- | Edges = ExchangeRates
edges :: [Edge M.SomeExchangeRate String]
edges = map toEdge exchangeRates

-- | Convert an exchange rate to an edge that goes from source currency to destination currency
toEdge :: M.SomeExchangeRate -> Edge M.SomeExchangeRate String
toEdge se = Edge
   (M.someExchangeRateSrcCurrency se)
   se
   (M.someExchangeRateDstCurrency se)

-- | Compose two 'M.SomeExchangeRate's (if they're compatible).
-- Given an exchange rate from EUR to USD, and another exchange rate from USD to JPY,
--  return an exchange rate from EUR to JPY.
-- Returns 'Nothing' in case the destination currency of the first exchange rate
--  doesn't match the source currency of the second exchange rate.
compose :: M.SomeExchangeRate -> M.SomeExchangeRate -> Maybe M.SomeExchangeRate
compose se1 se2
   | M.someExchangeRateDstCurrency se1 /= M.someExchangeRateSrcCurrency se2 = Nothing
   | otherwise = Just . fromJust $ M.mkSomeExchangeRate
      (M.someExchangeRateSrcCurrency se1)
      (M.someExchangeRateDstCurrency se2)
      (M.someExchangeRateRate se1 * M.someExchangeRateRate se2)   -- Both positive and non-zero

-- | Compose a list of 'M.SomeExchangeRate's.
--  Using 'compose' to return e.g. an exchange rate from "USD" to "JPY" from a list of
--   exchange rates from ["USD->EUR", "EUR->GBP", "GBP->JPY"].
composeRates :: [M.SomeExchangeRate] -> Maybe M.SomeExchangeRate
composeRates [] = Nothing
composeRates (first:erLst) = foldl go (Just first) erLst
   where go Nothing _ = Nothing
         go (Just er1) er2 = er1 `compose` er2
