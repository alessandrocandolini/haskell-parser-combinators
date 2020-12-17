module ParserSpec where

import Data.Maybe
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Parser" $ do
  describe "stupidParser" $ do
    prop "should always parse successfully every non-empty string" $
      \s -> not (null s) ==> isJust (runParser stupidParser s)

  describe "char" $ do
    prop "should always parse successfully every non-empty string starting with the char that the parser is capable of parsing" $
      \c s -> runParser (charParser c) (c : s) == Just (s, c)

  describe "pfilter" $ do
    prop "should behave like the original parser when the predicate is identitcally True" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const True) originalParser
         in runParser originalParser s == runParser filteredParser s

    prop "should fail if predicate == False" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const False) originalParser
         in isNothing (runParser filteredParser s)
