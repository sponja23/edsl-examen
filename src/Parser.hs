{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module Parser
  ( Parser,
    runParser,
    pFail,
    pSucceed,
    pSat,
    pSym,
    pList,
    pListP,
    pChain,
    digit,
    number,
    pStr,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (guard, (>=>))
import Data.Char (isDigit)
import Data.Foldable (Foldable (foldl'))

-- | Parser aplicativo de listas de elementos de tipo s que produce valores de tipo a
--
-- El parser es determinístico.
newtype Parser s a = Parser ([s] -> Maybe (a, [s]))

instance Functor (Parser s) where
  fmap f (Parser p) = Parser $ p >=> \(x, s) -> Just (f x, s)

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser px = Parser $ \s -> do
    (f, s') <- pf s
    (x, s'') <- px s'
    Just (f x, s'')

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

runParser :: Parser s a -> [s] -> Maybe a
runParser (Parser p) s = case p s of
  -- Sólo se acepta el resultado si se consumió toda la entrada
  Just (x, []) -> Just x
  _ -> Nothing

-- | Parser que falla
pFail :: Parser s a
pFail = empty

-- | Parser que tiene éxito sin consumir nada
pSucceed :: a -> Parser s a
pSucceed = pure

-- | Parser que tiene éxito si el primer elemento de la entrada satisface el predicado p
pSat :: (s -> Bool) -> Parser s s
pSat p = Parser $ \case
  [] -> Nothing
  x : xs -> guard (p x) >> Just (x, xs)

-- | Parser que tiene éxito si el primer elemento de la entrada es x
pSym :: (Eq s) => s -> Parser s s
pSym x = pSat (== x)

-- | Parser que tiene éxito si la entrada empieza con una secuencia de elementos dada
pStr :: (Eq s) => [s] -> Parser s [s]
pStr = traverse pSym

-- | Parser que, dado un parser que parsea la expresión `p`, parsea `p^*`
pList :: Parser s a -> Parser s [a]
pList = many

-- | Parser que, dado un parser que parsea la expresión `p`, parsea `p^+`
pListP :: Parser s a -> Parser s [a]
pListP = some

-- | Parser que, dado un parser que parsea la expresión `p` y un separador `sep`, parsea `p (sep p)^*`
pChain :: (Eq s) => Parser s a -> [s] -> Parser s [a]
pChain p sep = (:) <$> p <*> many (pStr sep *> p)

type StringParser a = Parser Char a

-- | Parsea un dígito
digit :: StringParser Int
digit = toInt <$> pSat isDigit
  where
    toInt c = fromEnum c - fromEnum '0'

-- | Parsea una secuencia de dígitos
number :: StringParser Int
number = foldl' addDigit 0 <$> some digit
  where
    addDigit n d = n * 10 + d
