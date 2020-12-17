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
    prop "should fail to parse the input if and only if the input is the empty string" $
      \s -> isNothing (runParser stupidParser s) == null s

    prop "when parsing a non-empty string should always parse successfully the first character" $
      \c s -> runParser stupidParser (c : s) == Just (s, c)

  describe "charParser" $ do
    prop "should always parse successfully every non-empty string starting with the char that the parser is capable of parsing" $
      \c s -> runParser (charParser c) (c : s) == Just (s, c)

    prop "should always fail to parse every string that is either empty or it does not start with the char the parser can parsr" $
      \c s -> (null s) || (head s /= c) ==> isNothing $ runParser (charParser c) s

  describe "stringParser" $ do
    prop "should always parse successfully every non-empty string starting with the string that the parser is capable of parsing" $
      \s s' -> runParser (stringParser s) (s ++ s') == Just (s', s)

  describe "Filterable parser" $ do
    prop "pfilter should behave like the original parser when the predicate is identically True" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const True) originalParser
         in runParser originalParser s == runParser filteredParser s

    prop "pfilter should be idempotent" $
      \s (Fun _ p) (Fun _ f) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            p' = pfilter (f :: Integer -> Bool) originalParser
            p'' = pfilter f p'
         in runParser p' s == runParser p'' s

    prop "pfiler should always fail if predicate is identically False" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            filteredParser = pfilter (const False) originalParser
         in isNothing (runParser filteredParser s)

  describe "Proof that parser is functor" $ do
    prop "identity law: fmap id == id" $
      \s (Fun _ p) ->
        let originalParser :: Parser String Maybe Integer
            originalParser = Parser p
            mappedParser = fmap id originalParser
         in runParser originalParser s == runParser mappedParser s

    prop "composition law: fmap (f . g) == fmap f . fmap g" $
      \s (Fun _ p) (Fun _ f) (Fun _ g) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = (fmap (g :: String -> [Bool]) . fmap (f :: Integer -> String)) p'
            p''' = fmap (g . f) p'
         in runParser p'' s == runParser p''' s

    prop "a mapped parser is successful on an input if and only if the original parser is (practical implication of functor laws for parsers)" $
      \s (Fun _ p) (Fun _ f) ->
        let p' :: Parser String [] Integer
            p' = Parser p
            p'' = fmap (f :: Integer -> String) p'
         in null (runParser p' s) == null (runParser p'' s)

  describe "Proof that parser is applicative" $ do
    prop "identity" $
      \s (Fun _ p) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = pure id <*> p'
         in runParser p' s == runParser p'' s

    prop "homomorphism" $
      \s (Fun _ f) a ->
        let p' :: Parser String Maybe String
            p' = pure (f :: Integer -> String) <*> pure (a :: Integer)
            p'' :: Parser String Maybe String
            p'' = pure (f a)
         in runParser p'' s == runParser p' s
