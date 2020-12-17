module ParserSpec where

import Data.Maybe
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Parser" $ do
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

  describe "Laws of functor" $ do
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

  describe "(Some) laws of applicative" $ do
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
