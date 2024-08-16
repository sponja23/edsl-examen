module ParseTests (parseTests) where

import Data.Maybe (fromJust)
import qualified DeepEmbedding as DE
import Parsing.ExprParser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

-- | Pruebas de los parsers.
parseTests :: TestTree
parseTests =
  testGroup
    "Parsers"
    [ parseUntypedTests,
      parseWellTypedTests,
      parseEquivalenceTests,
      parseInverseTests
    ]

-- | Pruebas para el parser de comparaciones general (no necesariamente bien tipadas).
parseUntypedTests :: TestTree
parseUntypedTests =
  testGroup
    "Parser no tipado"
    [ testCase "parse val" $
        parseExprUntyped "42"
          @?= Just val42,
      testCase "parse eq" $
        parseExprUntyped "(42=42)"
          @?= Just eq42_42,
      testCase "parse lt" $
        parseExprUntyped "(42<43)"
          @?= Just lt42_43,
      testCase "parse not" $
        parseExprUntyped "~(42=43)"
          @?= Just (UNot eq42_43),
      testCase "parse and" $
        parseExprUntyped "(42=42)/\\(42<43)"
          @?= Just (eq42_42 `UAnd` lt42_43),
      testCase "parse or" $
        parseExprUntyped "(42=42)\\/(42<43)"
          @?= Just (eq42_42 `UOr` lt42_43),
      testCase "parse and chain" $
        parseExprUntyped "(42=42)/\\(42=42)/\\(42=42)"
          @?= Just (eq42_42 `UAnd` eq42_42 `UAnd` eq42_42),
      testCase "parse or chain" $
        parseExprUntyped "(42=42)\\/(42=42)\\/(42=42)"
          @?= Just
            (eq42_42 `UOr` eq42_42 `UOr` eq42_42),
      testCase "parse complex" $
        parseExprUntyped "(42=42)/\\(42=42)\\/~(42<43)"
          @?= Just ((eq42_42 `UAnd` eq42_42) `UOr` UNot lt42_43),
      testCase "parse fail 1" $
        parseExprUntyped "(42=42)\\/(42=42)\\/(42=42)\\"
          @?= Nothing,
      testCase "parse fail 2" $
        parseExprUntyped "(42=42)\\/(42=42)\\/(42=42"
          @?= Nothing,
      testCase "parse untyped" $
        parseExprUntyped "((1/\\2)<(3\\/4))"
          @?= Just ((UVal 1 `UAnd` UVal 2) `ULt` (UVal 3 `UOr` UVal 4))
    ]
  where
    val42 = UVal 42
    eq42_42 = val42 `UEq` val42
    lt42_43 = val42 `ULt` UVal 43
    eq42_43 = val42 `UEq` UVal 43

-- | Pruebas para el parser de comparaciones bien tipadas.
--
-- Pruebo sólo el parser de deep embedding, ya que está implementado en términos del parser de
-- shallow embedding.
parseWellTypedTests :: TestTree
parseWellTypedTests =
  testGroup
    "Parser bien tipado"
    [ testCase "parse eq" $
        parseExpr "(42=42)"
          @?= Just eq42_42,
      testCase "parse lt" $
        parseExpr "(42<43)"
          @?= Just lt42_43,
      testCase "parse not" $
        parseExpr "~(42=43)"
          @?= Just (DE.Not eq42_43),
      testCase "parse and" $
        parseExpr "(42=42)/\\(42<43)"
          @?= Just (eq42_42 `DE.And` lt42_43),
      testCase "parse or" $
        parseExpr "(42=42)\\/(42<43)"
          @?= Just (eq42_42 `DE.Or` lt42_43),
      testCase "parse and chain" $
        parseExpr "(42=42)/\\(42=42)/\\(42=42)"
          @?= Just (eq42_42 `DE.And` eq42_42 `DE.And` eq42_42),
      testCase "parse or chain" $
        parseExpr "(42=42)\\/(42=42)\\/(42=42)"
          @?= Just
            (eq42_42 `DE.Or` eq42_42 `DE.Or` eq42_42),
      testCase "parse complex" $
        parseExpr "(42=42)/\\(42=42)\\/~(42<43)"
          @?= Just ((eq42_42 `DE.And` eq42_42) `DE.Or` DE.Not lt42_43),
      testCase "parse fail 1" $
        parseExpr "(42=42)\\/(42=42)\\/(42=42)\\"
          @?= (Nothing :: Maybe (DE.Expr Bool)),
      testCase "parse fail 2" $
        parseExpr "(42=42)\\/(42=42)\\/(42=42"
          @?= (Nothing :: Maybe (DE.Expr Bool)),
      testCase "parse fail 3" $
        parseExpr "((1/\\2)<(3\\/4))"
          @?= (Nothing :: Maybe (DE.Expr Bool))
    ]
  where
    val42 = DE.Val 42
    eq42_42 = val42 `DE.Eq` val42
    lt42_43 = val42 `DE.Lt` DE.Val 43
    eq42_43 = val42 `DE.Eq` DE.Val 43

-- | Tests que verifican que, para términos bien tipados, ambos parsers tienen resultados equivalentes.
parseEquivalenceTests :: TestTree
parseEquivalenceTests = testGroup "Parsers equivalentes" (map mkTest cases)
  where
    mkTest e =
      testCase (show e) $
        (fromJust . parseExprUntyped . show) e `eqTyped` (fromJust . parseExpr . show) e @? ""
    cases =
      [ DE.Val 42 `DE.Eq` DE.Val 42,
        DE.Val 42 `DE.Lt` DE.Val 43,
        DE.Not (DE.Val 42 `DE.Eq` DE.Val 43),
        (DE.Val 42 `DE.Eq` DE.Val 42) `DE.And` (DE.Val 42 `DE.Lt` DE.Val 43),
        (DE.Val 42 `DE.Eq` DE.Val 42) `DE.And` (DE.Val 42 `DE.Eq` DE.Val 42) `DE.Or` DE.Not (DE.Val 42 `DE.Lt` DE.Val 43)
      ]

-- | Tests que verifican que el parser de comparaciones es el inverso por derecha de la
-- representación de comparaciones como string.
parseInverseTests :: TestTree
parseInverseTests = testGroup "Parser inverso" (map mkTest cases)
  where
    mkTest e =
      testCase (show e) $
        (fromJust . parseExpr . show) e @?= e
    cases =
      [ DE.Val 42 `DE.Eq` DE.Val 42,
        DE.Val 42 `DE.Lt` DE.Val 43,
        DE.Not (DE.Val 42 `DE.Eq` DE.Val 43),
        (DE.Val 42 `DE.Eq` DE.Val 42) `DE.And` (DE.Val 42 `DE.Lt` DE.Val 43),
        (DE.Val 42 `DE.Eq` DE.Val 42) `DE.And` (DE.Val 42 `DE.Eq` DE.Val 42) `DE.Or` DE.Not (DE.Val 42 `DE.Lt` DE.Val 43)
      ]
