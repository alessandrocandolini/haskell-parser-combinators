module Parser where

import Control.Applicative
import Control.Monad

newtype Parser s m a = Parser
  { runParser :: s -> m (s, a)
  }

pfilter :: MonadPlus m => (a -> Bool) -> Parser s m a -> Parser s m a
pfilter f (Parser p) = Parser p'
  where
    p' s = mfilter (f . snd) (p s)

instance Functor m => Functor (Parser s m) where
  fmap f (Parser p) = Parser p'
    where
      p' s = fmap (fmap f) (p s)

instance Monad m => Applicative (Parser s m) where
  pure a = Parser $ \s -> pure (s, a)
  (Parser p1) <*> (Parser p2) = Parser p3
    where
      p3 s = do
        (s1, r1) <- p1 s
        (s2, r2) <- p2 s1
        return (s2, r1 r2)

instance (Monad m, Alternative m) => Alternative (Parser s m) where
  empty = Parser $ const empty
  (Parser p) <|> (Parser q) = Parser $ \s -> p s <|> q s

stupidParser :: Parser String Maybe Char
stupidParser = Parser p
  where
    p [] = Nothing
    p (x : xs) = Just (xs, x)

charParser :: Char -> Parser String Maybe Char
charParser c = pfilter (== c) stupidParser

stringParser :: String -> Parser String Maybe String
stringParser = traverse charParser
