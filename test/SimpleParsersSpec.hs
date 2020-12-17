module SimpleParsersSpec where

import Data.Maybe
import Parser
import SimpleParsers
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property

spec :: Spec
spec = describe "SimpleParsers" $ do
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
