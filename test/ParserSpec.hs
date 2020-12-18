module ParserSpec where

import Data.Maybe
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Parser" $ do
  describe "Filterable parser" $ do
    prop "pfilter should generate a parser that behaves exactly like the original one when the predicate is identically True" $
      \s (Fun _ p) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = pfilter (const True) p'
         in runParser p'' s == runParser p' s

    prop "pfilter should be idempotent" $
      \s (Fun _ p) (Fun _ f) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = pfilter (f :: Integer -> Bool) p'
            p''' = pfilter f p''
         in runParser p''' s == runParser p'' s

    prop "pfiler should always fail if predicate is identically False" $
      \s (Fun _ p) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = pfilter (const False) p'
         in isNothing (runParser p'' s)

  describe "Functor laws for parsers" $ do
    prop "identity law: fmap id == id" $
      \s (Fun _ p) ->
        let p' :: Parser String Maybe Integer
            p' = Parser p
            p'' = fmap id p'
         in runParser p' s == runParser p'' s

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

  describe "(Some) applicative laws for parsers" $ do
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
