{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ShallowEmbeddingTests (shallowTests) where

import qualified ShallowEmbedding as SE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el shallow embedding de comparaciones
shallowTests :: TestTree
shallowTests =
  testGroup
    "Shallow Embedding"
    [ testCase "val" $
        SE.eval (SE.val 42)
          @?= 42,
      testCase "eq1" $
        SE.eval (SE.val 42 `SE.eq` SE.val 42)
          @?= True,
      testCase "eq2" $
        SE.eval (SE.val 42 `SE.eq` SE.val 43)
          @?= False,
      testCase "lt1" $
        SE.eval (SE.val 42 `SE.lt` SE.val 43)
          @?= True,
      testCase "lt2" $
        SE.eval (SE.val 42 `SE.lt` SE.val 42)
          @?= False,
      testCase "not1" $
        SE.eval (SE.not trueExpr)
          @?= False,
      testCase "not2" $
        SE.eval (SE.not falseExpr)
          @?= True,
      testCase "and1" $
        SE.eval (trueExpr `SE.and` trueExpr)
          @?= True,
      testCase "and2" $
        SE.eval (trueExpr `SE.and` falseExpr)
          @?= False,
      testCase "and3" $
        SE.eval (falseExpr `SE.and` trueExpr)
          @?= False,
      testCase "and4" $
        SE.eval (falseExpr `SE.and` falseExpr)
          @?= False,
      testCase "or1" $
        SE.eval (trueExpr `SE.or` trueExpr)
          @?= True,
      testCase "or2" $
        SE.eval (trueExpr `SE.or` falseExpr)
          @?= True,
      testCase "or3" $
        SE.eval (falseExpr `SE.or` trueExpr)
          @?= True,
      testCase "or4" $
        SE.eval (falseExpr `SE.or` falseExpr)
          @?= False
    ]
  where
    falseExpr = SE.val 42 `SE.eq` SE.val 43
    trueExpr = SE.val 42 `SE.eq` SE.val 42
