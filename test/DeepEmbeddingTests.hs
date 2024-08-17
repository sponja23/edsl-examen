module DeepEmbeddingTests (deepTests) where

import DeepEmbedding (Expr (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (Eq)

-- | Pruebas para el deep embedding de comparaciones
deepTests :: TestTree
deepTests =
  testGroup
    "Deep Embedding"
    [ testCase "val" $
        eval (Val 42)
          @?= 42,
      testCase "eq1" $
        eval (Val 42 `Eq` Val 42)
          @?= True,
      testCase "eq2" $
        eval (Val 42 `Eq` Val 43)
          @?= False,
      testCase "lt1" $
        eval (Val 42 `Lt` Val 43)
          @?= True,
      testCase "lt2" $
        eval (Val 42 `Lt` Val 42)
          @?= False,
      testCase "not1" $
        eval (Not trueExpr)
          @?= False,
      testCase "not2" $
        eval (Not falseExpr)
          @?= True,
      testCase "and1" $
        eval (trueExpr `And` trueExpr)
          @?= True,
      testCase "and2" $
        eval (trueExpr `And` falseExpr)
          @?= False,
      testCase "and3" $
        eval (falseExpr `And` trueExpr)
          @?= False,
      testCase "and4" $
        eval (falseExpr `And` falseExpr)
          @?= False,
      testCase "or1" $
        eval (trueExpr `Or` trueExpr)
          @?= True,
      testCase "or2" $
        eval (trueExpr `Or` falseExpr)
          @?= True,
      testCase "or3" $
        eval (falseExpr `Or` trueExpr)
          @?= True,
      testCase "or4" $
        eval (falseExpr `Or` falseExpr)
          @?= False
    ]
  where
    falseExpr = Eq (Val 42) (Val 43)
    trueExpr = Eq (Val 42) (Val 42)
