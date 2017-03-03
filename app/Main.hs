module Main where

import System.Environment
import System.IO
import Gedcom
import Parser

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
