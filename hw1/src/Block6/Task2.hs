{-# LANGUAGE LambdaCase #-}

module Block6.Task2 ( ok
                    , eof
                    , satisfy
                    , char
                    , string
                    , element
                    , stream
                    ) where

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

-- |Succeds if first chracter equal to given
char :: Char -> Parser Char Char
char p = satisfy (==p)

-- |Alias for char
element :: Char -> Parser Char Char
element = char

-- |Checks if string starts with given string
matchFront :: String -> String -> Maybe String
matchFront (cur1 : rest1) (cur2 : rest2)
  | cur1 == cur2 = matchFront rest1 rest2
  | otherwise = Nothing
matchFront [] rest = Just rest
matchFront _ _ = Nothing

-- |Succeds if input starts from given string
string :: String -> Parser Char String
string front = Parser $ \s -> do
  rest <- matchFront front s
  return (front, rest)

-- |Alias for string
stream :: String -> Parser Char String
stream = string
