{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DeepEmbedding (Expr (..), eval) where

import Data.Kind (Type)

data Expr :: (Type -> Type) where
  Val :: Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  Lt :: Expr Int -> Expr Int -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool

-- | Intérprete de comparaciones
eval :: Expr t -> t
eval (Val x) = x
eval (Eq x y) = eval x == eval y
eval (Lt x y) = eval x < eval y
eval (Not x) = not (eval x)
eval (And x y) = eval x && eval y
eval (Or x y) = eval x || eval y
