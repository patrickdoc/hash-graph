module Main where

import Impl.Function ( funcTests )

import Test.Hspec

main :: IO ()
main = hspec $ do
  funcTests
