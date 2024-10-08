{-# LANGUAGE LambdaCase #-}

module ExtendedEmbeddingTests (extendedTests) where

import Extended.DeepEmbedding (Expr (..), eval, evalMaybe)
import Extended.ExprParser (Ty (..), UProp (..), parseExprUntyped, typeProp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Pruebas para el deep embedding extendido
--
-- No pruebo el shallow embedding porque el deep está implementado en términos
-- del shallow.
extendedTests :: TestTree
extendedTests =
  testGroup
    "Embedding Extendido"
    [ totalEvalTests,
      partialEvalTests,
      parseExprTests,
      typePropTests
    ]

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

-- | Pruebas de parsing de expresiones de comparación con variables
parseExprTests :: TestTree
parseExprTests =
  testGroup
    "Parsing de expresiones con variables"
    [ testCase "parse var" $
        parseExprUntyped "x" @?= Just (UVar "x"),
      testCase "parse and" $
        parseExprUntyped "x/\\y" @?= Just (UVar "x" `UAnd` UVar "y"),
      testCase "parse or" $
        parseExprUntyped "x\\/y" @?= Just (UVar "x" `UOr` UVar "y"),
      testCase "parse not" $
        parseExprUntyped "~x" @?= Just (UNot (UVar "x")),
      testCase "parse eq" $
        parseExprUntyped "(x=y)" @?= Just (UVar "x" `UEq` UVar "y"),
      testCase "parse lt" $
        parseExprUntyped "(x<y)" @?= Just (UVar "x" `ULt` UVar "y"),
      testCase "parse complex" $
        parseExprUntyped "x/\\y\\/~z" @?= Just (UVar "x" `UAnd` UVar "y" `UOr` UNot (UVar "z")),
      testCase "parse fail" $
        parseExprUntyped "x/\\y\\/~z\\" @?= Nothing
    ]

-- | Pruebas para el tipado de expresiones de comparación
typePropTests :: TestTree
typePropTests =
  testGroup
    "Tipado de expresiones de comparación"
    [ testCase "type val" $
        typeProp TInt (UVal 1) @?= Val 1,
      testCase "type eq" $
        typeProp TBool (UVal 1 `UEq` UVal 2) @?= Val 1 `Eq` Val 2,
      testCase "type lt" $
        typeProp TBool (UVal 1 `ULt` UVal 2) @?= Val 1 `Lt` Val 2,
      testCase "type not" $
        typeProp TBool (UNot (UVar "x")) @?= Not (Var "x"),
      testCase "type and" $
        typeProp TBool (UVar "x" `UAnd` UVar "y") @?= Var "x" `And` Var "y",
      testCase "type or" $
        typeProp TBool (UVar "x" `UOr` UVar "y") @?= Var "x" `Or` Var "y",
      testCase "type var" $
        typeProp TBool (UVar "x") @?= Var "x"
    ]