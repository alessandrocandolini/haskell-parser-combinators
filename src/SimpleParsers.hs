module SimpleParsers where

import Control.Applicative
import Control.Monad
import Parser

stupidParser :: Parser String Maybe Char
stupidParser = Parser p
  where
    p [] = Nothing
    p (x : xs) = Just (xs, x)

charParser :: Char -> Parser String Maybe Char
charParser c = pfilter (== c) stupidParser

stringParser :: String -> Parser String Maybe String
stringParser = traverse charParser
