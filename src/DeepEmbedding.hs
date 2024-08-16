{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DeepEmbedding (Expr (..), showExpr, eval) where

import Data.Kind (Type)
import qualified ShallowEmbedding as SE

-- | Deep embedding de expresiones de comparación
data Expr :: (Type -> Type) where
  Val :: Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  Lt :: Expr Int -> Expr Int -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool

-- Instancia de Eq para comparar expresiones, usada en los tests
instance Eq (Expr t) where
  Val x == Val y = x == y
  x `Eq` y == x' `Eq` y' = x == x' && y == y'
  x `Lt` y == x' `Lt` y' = x == x' && y == y'
  Not x == Not y = x == y
  x `And` y == x' `And` y' = x == x' && y == y'
  x `Or` y == x' `Or` y' = x == x' && y == y'
  _ == _ = False

-- | Intérprete de shallow embedding que produce un deep embedding
instance SE.Expr Expr where
  val = Val
  eq = Eq
  lt = Lt
  not = Not
  and = And
  or = Or

-- | Permite "re-interpretar" una expresión de deep embedding como usando alguna
-- implementación de shallow embedding. Esto implica que se puede re-utilizar
-- el código de éste último para implementar funciones sobre deep embedding.
toShallow :: (SE.Expr e) => Expr t -> e t
toShallow (Val x) = SE.val x
toShallow (x `Eq` y) = toShallow x `SE.eq` toShallow y
toShallow (x `Lt` y) = toShallow x `SE.lt` toShallow y
toShallow (Not x) = SE.not (toShallow x)
toShallow (x `And` y) = toShallow x `SE.and` toShallow y
toShallow (x `Or` y) = toShallow x `SE.or` toShallow y

-- | Evaluar una expresión
eval :: Expr t -> t
eval = SE.eval . toShallow

-- | Mostrar una expresión
showExpr :: Expr t -> String
showExpr = SE.showExpr . toShallow

instance Show (Expr t) where
  show = showExpr
