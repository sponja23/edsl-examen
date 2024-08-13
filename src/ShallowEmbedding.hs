{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShallowEmbedding (Expr (..), eval) where

import Control.Applicative (Applicative (liftA2))

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
