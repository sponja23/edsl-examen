{-# LANGUAGE GADTs #-}

module Parsing.ExprParser (parseExprUntyped, parseExpr, UProp (..), eqTyped) where

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import qualified DeepEmbedding as DE
import Parsing.Parser
import qualified ShallowEmbedding as SE

-- | Tipo del árbol de sintaxis de comparaciones.
--
-- Las expresiones representadas no necesariamente son bien tipadas.
data UProp
  = UVal Int
  | UEq UProp UProp
  | ULt UProp UProp
  | UNot UProp
  | UAnd UProp UProp
  | UOr UProp UProp
  deriving (Show, Eq)

-- | Comparación de expresiones no tipadas con el deep embedding.
eqTyped :: UProp -> DE.Expr t -> Bool
eqTyped (UVal x) (DE.Val x') = x == x'
eqTyped (UEq x y) (DE.Eq x' y') = eqTyped x x' && eqTyped y y'
eqTyped (ULt x y) (DE.Lt x' y') = eqTyped x x' && eqTyped y y'
eqTyped (UNot x) (DE.Not x') = eqTyped x x'
eqTyped (UAnd x y) (DE.And x' y') = eqTyped x x' && eqTyped y y'
eqTyped (UOr x y) (DE.Or x' y') = eqTyped x x' && eqTyped y y'
eqTyped _ _ = False

-- | Parser de expresiones de comparación.
--
-- Parsea la siguiente gramática:
--
--    prop ::= term '\/' prop | term
--
--    term ::= factor '/\' term | factor
--
--    factor ::= '~' prop | '(' prop ')' | '(' prop '=' prop ')' | '(' prop '<' prop ')' | N
parseExprUntyped :: String -> Maybe UProp
parseExprUntyped = runParser pProp
  where
    pProp =
      foldl1 UOr
        <$> pChain pTerm "\\/"
    pTerm =
      foldl1 UAnd
        <$> pChain pFactor "/\\"
    pFactor = pNot <|> pParens <|> pVal
    pNot =
      UNot
        <$> (pStr "~" *> pProp)
    pParens = pSym '(' *> (pForm <|> pProp) <* pSym ')'
    pForm =
      toComp
        <$> pProp
        <*> (pSym '=' $> UEq <|> pSym '<' $> ULt)
        <*> pProp
    toComp x op y = x `op` y
    pVal = UVal <$> number

-- | Parser de expresiones de comparación. Sólo admite expresiones bien tipadas.
--
-- Para lograr esto, se define una gramática de expresiones alternativa:
--
--    prop ::= term '\/' prop | term
--
--    term ::= factor '/\' term | factor
--
--    factor ::= '~' prop | '(' form ')' | '(' prop ')'
--
--    form ::= N '=' N | N '<' N
--
-- Éste es levemente distinto al de la consigna, ya que ese otro permite expresiones
-- como ((1/\2)<(3\/4)), que no tienen sentido (ni tipan) en el lenguaje de comparaciones.
--
-- El tipo del resultado se debe poder inferir en cada llamada a la función.
parseExpr :: (SE.Expr e) => String -> Maybe (e Bool)
parseExpr = runParser pProp
  where
    pProp =
      foldl1 SE.or
        <$> pChain pTerm "\\/"
    pTerm =
      foldl1 SE.and
        <$> pChain pFactor "/\\"
    pFactor = pNot <|> pParens
    pNot =
      SE.not
        <$> (pStr "~" *> pProp)
    pParens = pSym '(' *> (pForm <|> pProp) <* pSym ')'
    pForm =
      toComp
        <$> number
        <*> (pSym '=' $> SE.eq <|> pSym '<' $> SE.lt)
        <*> number
    toComp x op y = SE.val x `op` SE.val y
