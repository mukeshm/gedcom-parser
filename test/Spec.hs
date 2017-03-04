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

  describe "Parser combinators" $ do

    describe "Item parser" $ do
      it "Should consume first character" $ do
        let [(c, cs)] = parse item "test"
        c `shouldBe` 't'
      it "Should give empty for empty string" $ do
        parse item "" `shouldBe` []

    describe "Monadic bind" $ do
      it "should extract the value from first parser and give it to next" $ do
        let [(c, cs)] = parse (item `bind` (\c -> do {item; return c})) "test"
        c `shouldBe` 't'
      it "Should be able to compose parsers" $ do
        let [(c, cs)] = parse (item >> item >> item >> item >> item) "tessel"
        c `shouldBe` 'e'

    describe "Monadic return" $ do
      it "should wrap a char into parser context" $ do
        let [(c, cs)] = parse (unit 'a') "test"
        c `shouldBe` 'a'
        cs `shouldBe` "test"

    describe "Failure parser" $ do
      it "Should always fail" $ do
       parse (failure :: Parser Int) "test" `shouldBe` []

    describe "option parser" $ do
      it "Should use first parser if first parser succedes" $ do
       let [(c, cs)] =  parse (item `option` failure) "test"
       c `shouldBe` 't'
       cs `shouldBe` "est"
      it "Should use second parser if first parser fails" $ do
       let [(c, cs)] =  parse (failure `option` item) "test"
       c `shouldBe` 't'
       cs `shouldBe` "est"
      it "Should fail if both first and second parser fails" $ do
       parse ((failure `option` failure) :: Parser Int) "test" `shouldBe` []

    describe "satisfy parser" $ do
      it "Should only parse @ for input \"@\"" $ do
       runParser (satisfy ('@' ==)) "@" `shouldBe` (Just '@')
      it "Should fail to parse @ for input \"t\"" $ do
       runParser (satisfy ('@' ==)) "t" `shouldBe` Nothing

  
  describe "Gedcom parser" $ do
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
        fmap docLevel (runParser (docParser 0) "0 @I1@ INDI\n") `shouldBe` (Just 0)
      it "0 @I1@ INDI should parse to id I1" $ do
        fmap docID (runParser (docParser 0) "0 @I1@ INDI\n") `shouldBe` (Just (Just "I1"))
      it "0 @I1@ INDI should parse to tag INDI" $ do
        fmap docTag (runParser (docParser 0) "0 @I1@ INDI\n") `shouldBe` (Just "INDI")

    describe "getIDattrib" $ do
      it "should give empty string for Nothing " $ do
        getIDattrib Nothing `shouldBe` []
      it "should give id value for Just \"I0001\" " $ do
        getIDattrib (Just "I0001") `shouldBe` " id=\"@I0001@\""

    describe "getIDattrib" $ do
      it "should give empty string for Nothing " $ do
        getIDattrib Nothing `shouldBe` []
      it "should give id value for Just \"I0001\"" $ do
        getIDattrib (Just "I0001") `shouldBe` " id=\"@I0001@\""
