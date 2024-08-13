module Main (main) where

import DeepEmbeddingTests (deepTests)
import ShallowEmbeddingTests (shallowTests)
import ShowTests (showTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [shallowTests, deepTests, showTests]
