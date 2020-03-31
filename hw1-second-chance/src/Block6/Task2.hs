{-# LANGUAGE LambdaCase #-}

module Block6.Task2 ( ok
                    , eof
                    , satisfy
                    , char
                    , string
                    , element
                    , stream
                    ) where

import Data.List (stripPrefix)

import Block6.Task1

-- |Parser that always succeds
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- |Succeds on eof
eof :: Parser s ()
eof = Parser $ \case
    [] -> Just ((), [])
    _  -> Nothing

-- |Succeds if first element satisfies given predicate
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

-- |Alias for element for Char type
char :: Char -> Parser Char Char
char = element

-- |Succeds if first element equal to given
element :: Eq s => s -> Parser s s
element p = satisfy (==p)

-- |Alias for stream for String type
string :: String -> Parser Char String
string = stream

-- |Succeds if input starts from given input
stream :: Eq s => [s] -> Parser s [s]
stream front = Parser $ \s -> do
  rest <- stripPrefix front s
  return (front, rest)
