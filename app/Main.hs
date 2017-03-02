module Main where

import Parser
import System.Environment
import System.IO
import Data.Char
import Control.Applicative

data Doc = Doc {
                 docLevel :: Int,
                 docID :: Maybe String,
                 docTag :: String,
                 docValue :: Maybe String
               } deriving Show

level :: Parser Int
level = read <$> some digit


idd :: Parser String
idd = do
  char '@'
  id <- alphaNum
  char '@'
  return id

tag :: Parser String
tag = some $ satisfy isUpper

value :: Parser String
value = many $ satisfy isPrint

toMaybe :: String -> Maybe String
toMaybe [] = Nothing
toMaybe xs = Just xs

--docParser :: Parser Doc
docParser = do
  l <- level
  spaces
  id <- idd <|>  optSpaces
  optSpaces
  t <- tag
  optSpaces
  val <- value
  optNewLine
  return (Doc l (toMaybe id) t (toMaybe val))
  
  

printContents s = putStrLn s
    
main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then putStrLn "Give Input file as an argument"
  else do
       let fileName =  args !! 0
       cont <- readFile fileName
       printContents cont
