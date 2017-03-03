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

data Elem = Elem {
                   elemTag :: String,
                   elemID :: Maybe String,
                   elemValue :: Maybe String,
                   elemChildren :: [Elem]
                  } deriving Show

type GEDCOM = [Elem]

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

docParser :: Int -> Parser Doc
docParser lev = do
  string (show lev)
  spaces
  id <- idd <|>  optSpaces
  optSpaces
  t <- tag
  optSpaces
  val <- value
  optNewLine
  return (Doc lev (toMaybe id) t (toMaybe val))
  
elemParser :: Int -> Parser Elem
elemParser lev = do
  doc <- docParser lev
  let Doc dl di dt dv = doc
  children <- many (elemParser $ lev + 1)
  return $ Elem dt di dv children

document :: Parser GEDCOM
document = some $ elemParser 0

lower :: String -> String
lower = map toLower

indentLevel :: Int
indentLevel = 4

indentation :: Int -> String
indentation n = concat $ replicate n " "

increaseIndent :: Int -> Int
increaseIndent n = n + indentLevel

getIDattrib :: Maybe String -> String
getIDattrib mv = maybe "" (\i -> " id=\"@" ++ i ++ "@\"" ) mv

getValueAttrib :: Maybe String -> String
getValueAttrib mv = maybe "" (\v -> " value=\"" ++ v ++ "\"" ) mv

genClosingTag :: String -> String
genClosingTag tag = "</" ++ lower tag ++ ">"

elemToXML :: Int -> Elem -> String
elemToXML indent (Elem et ei ev ec) = indentation indent
  ++ "<" ++ lower et
  ++ getIDattrib ei
  ++ case ec of
       [] -> ">" ++ maybe "" id  ev ++ genClosingTag et
       _  -> getValueAttrib ev ++ ">\n"
             ++ unlines (map (elemToXML (increaseIndent indent)) ec)
             ++ indentation indent
             ++ genClosingTag et

gedcomToXML :: GEDCOM -> String
gedcomToXML gc = "<gedcom>\n"
  ++ unlines (map (elemToXML indentLevel) gc)
  ++ "</gedcom>\n"

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then putStrLn "Give Input file as an argument"
  else do
      let fileName =  args !! 0
      cont <- readFile fileName
      case parse document cont of
          []        -> putStrLn "Nothing parsed"
          [(gc, _)] -> putStrLn $ gedcomToXML gc
