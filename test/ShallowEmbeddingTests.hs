{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ShallowEmbeddingTests (shallowTests) where

import ShallowEmbedding (Expr (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (and, not, or)

-- | Pruebas para el shallow embedding de comparaciones
shallowTests :: TestTree
shallowTests =
  testGroup
    "Shallow Embedding"
    [ testCase "val" $
        eval (val 42)
          @?= 42,
      testCase "eq1" $
        eval (val 42 `eq` val 42)
          @?= True,
      testCase "eq2" $
        eval (val 42 `eq` val 43)
          @?= False,
      testCase "lt1" $
        eval (val 42 `lt` val 43)
          @?= True,
      testCase "lt2" $
        eval (val 42 `lt` val 42)
          @?= False,
      testCase "not1" $
        eval (not trueExpr)
          @?= False,
      testCase "not2" $
        eval (not falseExpr)
          @?= True,
      testCase "and1" $
        eval (trueExpr `and` trueExpr)
          @?= True,
      testCase "and2" $
        eval (trueExpr `and` falseExpr)
          @?= False,
      testCase "and3" $
        eval (falseExpr `and` trueExpr)
          @?= False,
      testCase "and4" $
        eval (falseExpr `and` falseExpr)
          @?= False,
      testCase "or1" $
        eval (trueExpr `or` trueExpr)
          @?= True,
      testCase "or2" $
        eval (trueExpr `or` falseExpr)
          @?= True,
      testCase "or3" $
        eval (falseExpr `or` trueExpr)
          @?= True,
      testCase "or4" $
        eval (falseExpr `or` falseExpr)
          @?= False
    ]
  where
    falseExpr = val 42 `eq` val 43
    trueExpr = val 42 `eq` val 42
