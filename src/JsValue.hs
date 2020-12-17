module JsValue where

import Control.Applicative
import Data.Map (Map)
import Parser
import SimpleParsers

data JsValue
  = JsNull
  | JsBool Bool
  | JsString String
  | JsNumber Double
  | JsObject (Map String JsValue)
  | JsArray [JsValue]
  deriving (Eq, Show)

jsNullParser :: Parser String Maybe JsValue
jsNullParser = fmap (const JsNull) (stringParser "null")

jsBoolParser :: Parser String Maybe JsValue
jsBoolParser = jsTrueParser <|> jsFalseParser
  where
    jsFalseParser = fmap (const (JsBool False)) (stringParser "false")
    jsTrueParser = fmap (const (JsBool True)) (stringParser "true")

jsValueParser :: Parser String Maybe JsValue
jsValueParser = jsNullParser <|> jsBoolParser
