module Extended.ExprParser where

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import qualified Extended.ShallowEmbedding as ESE
import Parsing.Parser
import qualified ShallowEmbedding as SE

data UProp
  = UVal Int
  | UEq UProp UProp
  | ULt UProp UProp
  | UNot UProp
  | UAnd UProp UProp
  | UOr UProp UProp
  | UVar String
  deriving (Show, Eq)

-- | Parser de expresiones de comparación.
--
-- Parsea la siguiente gramática:
--
--    prop ::= term '\/' prop | term
--
--    term ::= factor '/\' term | factor
--
--    factor ::= '~' prop | '(' prop ')' | '(' prop '=' prop ')' | '(' prop '<' prop ')' | N | W
--
-- Donde W es una palabra formada por letras minúsculas.
parseExprUntyped :: String -> Maybe UProp
parseExprUntyped = runParser pProp
  where
    pProp =
      foldl1 UOr
        <$> pChain pTerm "\\/"
    pTerm =
      foldl1 UAnd
        <$> pChain pFactor "/\\"
    pFactor = pNot <|> pParens <|> pVal <|> pVar
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
    pVar = UVar <$> word

-- | Parser de expresiones de comparación. Sólo admite expresiones bien tipadas.
--
-- Para lograr esto, se define una gramática de expresiones alternativa:
--
--    prop ::= term '\/' prop | term
--
--    term ::= factor '/\' term | factor
--
--    factor ::= '~' prop | '(' form ')' | '(' prop ')' | W
--
--    form ::= N '=' N | N '<' N
--
-- Éste es levemente distinto al de la consigna, ya que ese otro permite expresiones
-- como ((1/\2)<(3\/4)), que no tienen sentido (ni tipan) en el lenguaje de comparaciones.
--
-- El tipo del resultado se debe poder inferir en cada llamada a la función.
parseExpr :: (ESE.VarExpr e) => String -> Maybe (e Bool)
parseExpr = runParser pProp
  where
    pProp =
      foldl1 SE.or
        <$> pChain pTerm "\\/"
    pTerm =
      foldl1 SE.and
        <$> pChain pFactor "/\\"
    pFactor = pNot <|> pParens <|> pVar
    pNot =
      SE.not
        <$> (pStr "~" *> pProp)
    pParens = pSym '(' *> (pForm <|> pProp) <* pSym ')'
    pForm =
      toComp
        <$> number
        <*> (pSym '=' $> SE.eq <|> pSym '<' $> SE.lt)
        <*> number
    pVar = ESE.var <$> word
    toComp x op y = SE.val x `op` SE.val y
