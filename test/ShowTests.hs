module ShowTests (showTests) where

import qualified DeepEmbedding as DE
import qualified ShallowEmbedding as SE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el pretty printing de comparaciones
showTests :: TestTree
showTests = testGroup "Show Tests" [showShallowTests, showDeepTests]

-- | Pruebas para el pretty printing de comparaciones en el shallow embedding
showShallowTests :: TestTree
showShallowTests =
  testGroup
    "Shallow Embedding"
    [ testCase "val" $
        SE.showExpr (SE.val 42)
          @?= "42",
      testCase "eq" $
        SE.showExpr eq42_42
          @?= "(42=42)",
      testCase "lt" $
        SE.showExpr lt42_43
          @?= "(42<43)",
      testCase "not" $
        SE.showExpr (SE.not eq42_42)
          @?= "~(42=42)",
      testCase "and" $
        SE.showExpr (eq42_42 `SE.and` lt42_43)
          @?= "((42=42)/\\(42<43))",
      testCase "or" $
        SE.showExpr (eq42_42 `SE.or` lt42_43)
          @?= "((42=42)\\/(42<43))",
      testCase "complex" $
        SE.showExpr (eq42_42 `SE.and` (eq42_42 `SE.or` SE.not lt42_43))
          @?= "((42=42)/\\((42=42)\\/~(42<43)))"
    ]
  where
    val42 = SE.val 42
    eq42_42 = val42 `SE.eq` val42
    lt42_43 = val42 `SE.lt` SE.val 43

showDeepTests :: TestTree
showDeepTests =
  testGroup
    "Deep Embedding"
    [ testCase "val" $
        DE.showExpr (DE.Val 42)
          @?= "42",
      testCase "eq" $
        DE.showExpr eq42_42
          @?= "(42=42)",
      testCase "lt" $
        DE.showExpr lt42_43
          @?= "(42<43)",
      testCase "not" $
        DE.showExpr (DE.Not eq42_42)
          @?= "~(42=42)",
      testCase "and" $
        DE.showExpr (eq42_42 `DE.And` lt42_43)
          @?= "((42=42)/\\(42<43))",
      testCase "or" $
        DE.showExpr (eq42_42 `DE.Or` lt42_43)
          @?= "((42=42)\\/(42<43))",
      testCase "complex" $
        DE.showExpr (eq42_42 `DE.And` (eq42_42 `DE.Or` DE.Not lt42_43))
          @?= "((42=42)/\\((42=42)\\/~(42<43)))"
    ]
  where
    val42 = DE.Val 42
    eq42_42 = val42 `DE.Eq` val42
    lt42_43 = val42 `DE.Lt` DE.Val 43
