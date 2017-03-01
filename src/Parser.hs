module Parser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser p s =
  case parse p s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream"
    _           -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [ (f a, b) | (a, b) <- p s]

instance Applicative Parser where
  pure = unit
  (Parser cs1) <*> (Parser cs2) = Parser $ \s -> [ (f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Monad Parser where
  return = unit
  (>>=) = bind  


failure :: Parser a
failure = Parser (\cs -> [])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case parse p s of
                              [] -> parse q s
                              res -> res

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

-- | One or more.
-- some :: f a -> f [a]
-- some v = some_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- | Zero or more.
-- many :: f a -> f [a]
-- many v = many_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then return c
  else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

alphaNum :: Parser String
alphaNum = many $ satisfy isAlphaNum

alpha :: Parser String
alpha = many $ satisfy isAlpha
