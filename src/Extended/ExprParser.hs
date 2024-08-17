{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Extended.ExprParser (UProp (..), parseExprUntyped, parseExpr, Ty (..), typeProp) where

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import Data.Kind
import qualified Extended.DeepEmbedding as EDE
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

data Ty :: (Type -> Type) where
  TInt :: Ty Int
  TBool :: Ty Bool

-- | Chequea el tipo de un UProp.
--
-- Si la expresión está bien tipada, devuelve el tipo de la misma.
typeProp :: Ty t -> UProp -> EDE.Expr t
typeProp TInt (UVal n) = EDE.Val n
typeProp TBool (UEq x y) = typeProp TInt x `EDE.Eq` typeProp TInt y
typeProp TBool (ULt x y) = typeProp TInt x `EDE.Lt` typeProp TInt y
typeProp TBool (UNot x) = EDE.Not (typeProp TBool x)
typeProp TBool (UAnd x y) = typeProp TBool x `EDE.And` typeProp TBool y
typeProp TBool (UOr x y) = typeProp TBool x `EDE.Or` typeProp TBool y
typeProp TBool (UVar s) = EDE.Var s
typeProp _ _ = error "typeProp: tipo incorrecto"
