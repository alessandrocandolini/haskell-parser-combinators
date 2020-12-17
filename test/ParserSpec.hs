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

  describe "charParser" $ do
    prop "should always parse successfully every non-empty string starting with the char that the parser is capable of parsing" $
      \c s -> runParser (charParser c) (c : s) == Just (s, c)

    prop "should always fail to parse every string that is either empty or it does not start with the char the parser can parsr" $
      \c s -> (null s) || (head s /= c) ==> isNothing $ runParser (charParser c) s

  describe "pfilter" $ do
    prop "should behave like the original parser when the predicate is identically True" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const True) originalParser
         in runParser originalParser s == runParser filteredParser s

    prop "should be idempotent" $
      \s (Fun _ p) (Fun _ f) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            p' = pfilter (f :: Integer -> Bool) originalParser
            p'' = pfilter f p'
         in runParser p' s == runParser p'' s

    prop "should always fail if predicate is identically False" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const False) originalParser
         in isNothing (runParser filteredParser s)
