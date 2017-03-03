module Main where

import Test.Hspec
import Gedcom
import Parser

runParser :: Parser a -> String -> Maybe a
runParser m s =
  case parse m s of
    [(res, [])] -> Just res
    [(_, rs)]   -> Nothing
    _           -> Nothing

main :: IO ()
main = hspec $ do
  describe "ID parser" $ do
    it "@1234@ is valid id" $ do
      runParser idd "@1234@" `shouldBe` (Just "1234")
    it "1234@ is invalid id" $ do
      runParser idd "1234@" `shouldBe` Nothing
    it "abc is invalid id" $ do
      runParser idd "abc" `shouldBe` Nothing

  describe "TAG parser" $ do
    it "ABCD is valid tag" $ do
      runParser tag "ABCD" `shouldBe` (Just "ABCD")
    it "abcd is invalid tag" $ do
      runParser tag "abcd" `shouldBe` Nothing

  describe "Value parser" $ do
    it "'Abc 12 JKJ' should parse to 'Abc 12 JKJ'" $ do
      runParser value "Abc 12 JKJ" `shouldBe` (Just "Abc 12 JKJ")

  describe "Document parser" $ do
    it "0 @I1@ INDI should parse to level 0" $ do
      fmap docLevel (runParser (docParser 0) "0 @I1@ INDI") `shouldBe` (Just 0)
    it "0 @I1@ INDI should parse to id I1" $ do
      fmap docID (runParser (docParser 0) "0 @I1@ INDI") `shouldBe` (Just (Just "I1"))
    it "0 @I1@ INDI should parse to tag INDI" $ do
      fmap docTag (runParser (docParser 0) "0 @I1@ INDI") `shouldBe` (Just "INDI")
