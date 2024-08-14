{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser,
    runParser,
    pFail,
    pSucceed,
    pSat,
    pSym,
    pList,
    pListP,
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
-- El parser es determinístico
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

pFail :: Parser s a
pFail = empty

pSucceed :: a -> Parser s a
pSucceed = pure

pSat :: (s -> Bool) -> Parser s s
pSat p = Parser $ \case
  [] -> Nothing
  x : xs -> guard (p x) >> Just (x, xs)

pSym :: (Eq s) => s -> Parser s s
pSym x = pSat (== x)

pStr :: (Eq s) => [s] -> Parser s [s]
pStr = traverse pSym

pList :: Parser s a -> Parser s [a]
pList = many

pListP :: Parser s a -> Parser s [a]
pListP = some

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
