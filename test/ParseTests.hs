module ParseTests (parseTests, parseInverseTests) where

import qualified DeepEmbedding as DE
import qualified ShallowEmbedding as SE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el parser de comparaciones
--
-- Pruebo sólo el parser de deep embedding, ya que está implementado en términos del parser de
-- shallow embedding.
parseTests :: TestTree
parseTests =
  testGroup
    "Parse"
    [ testCase "parse eq" $
        DE.parseExpr "(42=42)"
          @?= Just eq42_42,
      testCase "parse lt" $
        DE.parseExpr "(42<43)"
          @?= Just lt42_43,
      testCase "parse not" $
        DE.parseExpr "~(42=43)"
          @?= Just (DE.Not eq42_43),
      testCase "parse and" $
        DE.parseExpr "(42=42)/\\(42<43)"
          @?= Just (eq42_42 `DE.And` lt42_43),
      testCase "parse or" $
        DE.parseExpr "(42=42)\\/(42<43)"
          @?= Just (eq42_42 `DE.Or` lt42_43),
      testCase "parse and chain" $
        DE.parseExpr "(42=42)/\\(42=42)/\\(42=42)"
          @?= Just (eq42_42 `DE.And` eq42_42 `DE.And` eq42_42),
      testCase "parse or chain" $
        DE.parseExpr "(42=42)\\/(42=42)\\/(42=42)"
          @?= Just
            (eq42_42 `DE.Or` eq42_42 `DE.Or` eq42_42),
      testCase "parse complex" $
        DE.parseExpr "(42=42)/\\(42=42)\\/~(42<43)"
          @?= Just ((eq42_42 `DE.And` eq42_42) `DE.Or` DE.Not lt42_43),
      testCase "parse fail 1" $
        DE.parseExpr "(42=42)\\/(42=42)\\/(42=42)\\"
          @?= Nothing,
      testCase "parse fail 2" $
        DE.parseExpr "(42=42)\\/(42=42)\\/(42=42"
          @?= Nothing
    ]
  where
    val42 = DE.Val 42
    eq42_42 = val42 `DE.Eq` val42
    lt42_43 = val42 `DE.Lt` DE.Val 43
    eq42_43 = val42 `DE.Eq` DE.Val 43

-- | Tests que verifican que el parser de comparaciones es el inverso por derecha de la
-- representación de comparaciones como string.
parseInverseTests :: TestTree
parseInverseTests = testGroup "ParseInverse" (map mkTest cases)
  where
    mkTest e = testCase (show e) $ (SE.parseExpr . show) e @?= Just e
    cases =
      [ DE.Val 42 `DE.Eq` DE.Val 42,
        DE.Val 42 `DE.Lt` DE.Val 43,
        DE.Not (DE.Val 42 `DE.Eq` DE.Val 43),
        (DE.Val 42 `DE.Eq` DE.Val 42) `DE.And` (DE.Val 42 `DE.Eq` DE.Val 42) `DE.Or` DE.Not (DE.Val 42 `DE.Lt` DE.Val 43)
      ]
