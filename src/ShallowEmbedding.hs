{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShallowEmbedding (Expr (..), eval, showExpr, parseExpr) where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Data.Functor (($>))
import Parser
import Prelude hiding (and, not, or)
import qualified Prelude (not)

-- | Implementación tagless-final del lenguaje de comparaciones
class Expr e where
  val :: Int -> e Int
  eq :: e Int -> e Int -> e Bool
  lt :: e Int -> e Int -> e Bool
  not :: e Bool -> e Bool
  and :: e Bool -> e Bool -> e Bool
  or :: e Bool -> e Bool -> e Bool

-- | Intérprete de comparaciones
newtype Eval t = Eval t deriving (Functor)

instance Applicative Eval where
  pure = Eval

  -- El tipo Eval (a -> b) es inválido en términos de la clase Expr, pero es
  -- cómodo para definir la instancia Expr Eval. Además, valores de ese tipo
  -- no se pueden construir, ya que no se exporta el constructor de Eval.
  Eval f <*> Eval x = Eval (f x)

instance Expr Eval where
  val = Eval
  eq = liftA2 (==)
  lt = liftA2 (<)
  not = fmap Prelude.not
  and = liftA2 (&&)
  or = liftA2 (||)

-- | Evaluar una expresión
eval :: Eval t -> t
eval (Eval x) = x

-- | Representación de expresión como string
newtype ShowExpr t = ShowExpr String deriving (Functor)

instance Expr ShowExpr where
  val = ShowExpr . show
  (ShowExpr x) `eq` (ShowExpr y) = ShowExpr $ "(" ++ x ++ "=" ++ y ++ ")"
  (ShowExpr x) `lt` (ShowExpr y) = ShowExpr $ "(" ++ x ++ "<" ++ y ++ ")"
  not (ShowExpr x) = ShowExpr $ "~" ++ x
  (ShowExpr x) `and` (ShowExpr y) = ShowExpr $ "(" ++ x ++ "/\\" ++ y ++ ")"
  (ShowExpr x) `or` (ShowExpr y) = ShowExpr $ "(" ++ x ++ "\\/" ++ y ++ ")"

-- | Mostrar una expresión
showExpr :: ShowExpr t -> String
showExpr (ShowExpr x) = x

-- | Parser de expresiones de comparación.
--
-- Se parsea el siguiente lenguaje:
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
parseExpr :: (Expr e) => String -> Maybe (e Bool)
parseExpr = runParser pProp
  where
    pProp = foldl1 or <$> pChain pTerm "\\/"
    pTerm = foldl1 and <$> pChain pFactor "/\\"
    pFactor = pNot <|> pParens
    pNot = not <$> (pStr "~" *> pProp)
    pParens = pSym '(' *> (pForm <|> pProp) <* pSym ')'
    pForm = toComp <$> number <*> (pSym '=' $> eq <|> pSym '<' $> lt) <*> number
    toComp x op y = val x `op` val y
