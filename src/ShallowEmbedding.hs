{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShallowEmbedding (Expr (..), eval, showExpr) where

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
  eq (ShowExpr x) (ShowExpr y) = ShowExpr $ "(" ++ x ++ "=" ++ y ++ ")"
  lt (ShowExpr x) (ShowExpr y) = ShowExpr $ "(" ++ x ++ "<" ++ y ++ ")"
  not (ShowExpr x) = ShowExpr $ "~" ++ x
  and (ShowExpr x) (ShowExpr y) = ShowExpr $ "(" ++ x ++ "/\\" ++ y ++ ")"
  or (ShowExpr x) (ShowExpr y) = ShowExpr $ "(" ++ x ++ "\\/" ++ y ++ ")"

-- | Mostrar una expresión
showExpr :: ShowExpr t -> String
showExpr (ShowExpr x) = x
