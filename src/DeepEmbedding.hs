{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DeepEmbedding (Expr (..), eval, showExpr, parseExpr) where

import Data.Kind (Type)
import qualified ShallowEmbedding as SE

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
  Eq x y == Eq x' y' = x == x' && y == y'
  Lt x y == Lt x' y' = x == x' && y == y'
  Not x == Not x' = x == x'
  And x y == And x' y' = x == x' && y == y'
  Or x y == Or x' y' = x == x' && y == y'
  _ == _ = False

-- | Intérprete de comparaciones
eval :: Expr t -> t
eval (Val x) = x
eval (Eq x y) = eval x == eval y
eval (Lt x y) = eval x < eval y
eval (Not x) = not (eval x)
eval (And x y) = eval x && eval y
eval (Or x y) = eval x || eval y

-- | Representación de expresión como string
showExpr :: Expr t -> String
showExpr (Val x) = show x
showExpr (Eq x y) = "(" ++ showExpr x ++ "=" ++ showExpr y ++ ")"
showExpr (Lt x y) = "(" ++ showExpr x ++ "<" ++ showExpr y ++ ")"
showExpr (Not x) = "~" ++ showExpr x
showExpr (And x y) = "(" ++ showExpr x ++ "/\\" ++ showExpr y ++ ")"
showExpr (Or x y) = "(" ++ showExpr x ++ "\\/" ++ showExpr y ++ ")"

instance Show (Expr t) where
  show = showExpr

-- | Intérprete de shallow embedding que produce un deep embedding
instance SE.Expr Expr where
  val = Val
  eq = Eq
  lt = Lt
  not = Not
  and = And
  or = Or

-- | Parser de expresiones de comparación
--
-- La implementación es una especialización de la implementación de shallow
-- embedding, ya que el parser de shallow embedding es más general.
parseExpr :: String -> Maybe (Expr Bool)
parseExpr = SE.parseExpr
