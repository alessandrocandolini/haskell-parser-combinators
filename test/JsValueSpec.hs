module JsValueSpec where

import JsValue
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "JsValueParser test" $ do
  it "should parse null" $
    runParser jsValueParser "null" `shouldBe` Just ("", JsNull)

  it "should parse true" $
    runParser jsValueParser "true" `shouldBe` Just ("", JsBool True)
