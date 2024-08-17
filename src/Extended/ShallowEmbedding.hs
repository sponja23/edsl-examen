module Extended.ShallowEmbedding (VarExpr (..), eval, evalMaybe, evalWithVarMap, VariableEnv, PartialVariableEnv) where

import Control.Applicative (Applicative (liftA2))
import ShallowEmbedding (Expr (..))

-- | Extensión de la clase Expr para soportar variables
class (Expr e) => VarExpr e where
  var :: String -> e Bool

-- | Valuación de las variables de una expresión
type VariableEnv = String -> Bool

-- | Intérprete de comparaciones con variables
--
-- La función de evaluación de variables es **total**, así que todas las variables
-- tienen un valor definido.
newtype EvalVar t = EV (VariableEnv -> t)

-- Instancias necesarias para que EvalVar sea un Functor y Applicative
instance Functor EvalVar where
  fmap f (EV x) = EV (f . x)

instance Applicative EvalVar where
  pure x = EV (const x)
  (EV f) <*> (EV x) = EV (\env -> f env (x env))

instance Expr EvalVar where
  val = pure
  eq = liftA2 (==)
  lt = liftA2 (<)
  not = fmap Prelude.not
  and = liftA2 (&&)
  or = liftA2 (||)

instance VarExpr EvalVar where
  var x = EV ($ x)

-- | Valuación parcial de las variables de una expresión
--
-- Algunas variables pueden no tener un valor definido.
type PartialVariableEnv = String -> Maybe Bool

-- | Intérprete de comparaciones con variables parciales
--
-- La función de evaluación de variables es **parcial**, así que algunas variables
-- pueden no tener un valor definido.
newtype MaybeEvalVar t = MEV (PartialVariableEnv -> Maybe t)

instance Functor MaybeEvalVar where
  fmap f (MEV x) = MEV (fmap (fmap f) x)

instance Applicative MaybeEvalVar where
  pure x = MEV (pure (pure x))
  (MEV f) <*> (MEV x) = MEV (liftA2 (<*>) f x)

instance Expr MaybeEvalVar where
  val = pure
  eq = liftA2 (==)
  lt = liftA2 (<)
  not = fmap Prelude.not
  and = liftA2 (&&)
  or = liftA2 (||)

instance VarExpr MaybeEvalVar where
  var x = MEV ($ x)

-- | Evaluar una expresión con variables
eval :: EvalVar t -> VariableEnv -> t
eval (EV x) = x

-- | Evaluar una expresión con variables, donde algunas variables pueden no tener un valor definido
evalMaybe :: MaybeEvalVar t -> PartialVariableEnv -> Maybe t
evalMaybe (MEV x) = x

-- | Evaluar una expresión con variables, donde se provee un mapeo de variables a valores
evalWithVarMap :: MaybeEvalVar t -> [(String, Bool)] -> Maybe t
evalWithVarMap expr vars = evalMaybe expr (`lookup` vars)
