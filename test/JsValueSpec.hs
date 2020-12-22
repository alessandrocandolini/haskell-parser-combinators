module JsValueSpec where

import JsValue
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "JsValueParser test" $ do
  it "should parse null" $
    parseJsValue "null" `shouldBe` (Just JsNull)

  it "should parse true" $
    parseJsValue "true" `shouldBe` Just (JsBool True)

  it "should parse false" $
    parseJsValue "false" `shouldBe` Just (JsBool False)
