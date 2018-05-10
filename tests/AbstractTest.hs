module Main where

import Impl.Function ( funcTests )
import Impl.Hash ( hashTests )

import Test.Hspec

main :: IO ()
main = hspec $ do
  funcTests
  hashTests
