module DeepEmbeddingTests (deepTests) where

import qualified DeepEmbedding as DE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el deep embedding de comparaciones
deepTests :: TestTree
deepTests =
  testGroup
    "Deep Embedding"
    [ testCase "val" $
        DE.eval (DE.Val 42)
          @?= 42,
      testCase "eq1" $
        DE.eval (DE.Val 42 `DE.Eq` DE.Val 42)
          @?= True,
      testCase "eq2" $
        DE.eval (DE.Val 42 `DE.Eq` DE.Val 43)
          @?= False,
      testCase "lt1" $
        DE.eval (DE.Val 42 `DE.Lt` DE.Val 43)
          @?= True,
      testCase "lt2" $
        DE.eval (DE.Val 42 `DE.Lt` DE.Val 42)
          @?= False,
      testCase "not1" $
        DE.eval (DE.Not trueExpr)
          @?= False,
      testCase "not2" $
        DE.eval (DE.Not falseExpr)
          @?= True,
      testCase "and1" $
        DE.eval (trueExpr `DE.And` trueExpr)
          @?= True,
      testCase "and2" $
        DE.eval (trueExpr `DE.And` falseExpr)
          @?= False,
      testCase "and3" $
        DE.eval (falseExpr `DE.And` trueExpr)
          @?= False,
      testCase "and4" $
        DE.eval (falseExpr `DE.And` falseExpr)
          @?= False,
      testCase "or1" $
        DE.eval (trueExpr `DE.Or` trueExpr)
          @?= True,
      testCase "or2" $
        DE.eval (trueExpr `DE.Or` falseExpr)
          @?= True,
      testCase "or3" $
        DE.eval (falseExpr `DE.Or` trueExpr)
          @?= True,
      testCase "or4" $
        DE.eval (falseExpr `DE.Or` falseExpr)
          @?= False
    ]
  where
    falseExpr = DE.Eq (DE.Val 42) (DE.Val 43)
    trueExpr = DE.Eq (DE.Val 42) (DE.Val 42)
