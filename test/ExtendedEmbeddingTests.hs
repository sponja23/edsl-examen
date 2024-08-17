{-# LANGUAGE LambdaCase #-}

module ExtendedEmbeddingTests (extendedTests) where

import Extended.DeepEmbedding (Expr (..), eval, evalMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el deep embedding extendido
--
-- No pruebo el shallow embedding porque el deep está implementado en términos
-- del shallow.
extendedTests :: TestTree
extendedTests =
  testGroup
    "Deep Embedding Extendido"
    [totalEvalTests, partialEvalTests]

-- | Pruebas de evaluación del deep embedding extendido con función de valuación total
totalEvalTests :: TestTree
totalEvalTests =
  testGroup
    "Evaluación con valuación total"
    [ testCase
        "x /\\ y (x=True, y=True)"
        $ eval (Var "x" `And` Var "y") envAllTrue @?= True,
      testCase
        "x /\\ y (x=False, y=False)"
        $ eval (Var "x" `And` Var "y") envAllFalse @?= False,
      testCase
        "x \\/ y (x=True, y=False)"
        $ eval (Var "x" `Or` Var "y") envXTrueYFalse @?= True,
      testCase
        "x \\/ y (x=False, y=True)"
        $ eval (Var "x" `Or` Var "y") envXFalseYTrue @?= True
    ]
  where
    envAllTrue _ = True
    envAllFalse _ = False
    envXTrueYFalse = \case
      "x" -> True
      "y" -> False
      _ -> False
    envXFalseYTrue = \case
      "x" -> False
      "y" -> True
      _ -> False

-- | Pruebas de evaluación del deep embedding extendido con función de valuación parcial
partialEvalTests :: TestTree
partialEvalTests =
  testGroup
    "Evaluación con valuación parcial"
    [ testCase
        "x /\\ y (x=True, y=False)"
        $ evalMaybe (Var "x" `And` Var "y") envXTrueYFalse @?= Just False,
      testCase
        "x /\\ y (x=False, y=True)"
        $ evalMaybe (Var "x" `And` Var "y") envXFalseYTrue @?= Just False,
      testCase
        "x /\\ y (x=Nothing, y=True)"
        $ evalMaybe (Var "x" `And` Var "y") envXNothingYTrue @?= Nothing
    ]
  where
    envXTrueYFalse = \case
      "x" -> Just True
      "y" -> Just False
      _ -> Nothing
    envXFalseYTrue = \case
      "x" -> Just False
      "y" -> Just True
      _ -> Nothing
    envXNothingYTrue = \case
      "y" -> Just True
      _ -> Nothing
