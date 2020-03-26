module Block6.Task3 ( isPSP
                    , signedInteger
                    , isnot
                    , skipDelimitters
                    , integer
                    ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)

import Block6.Task1
import Block6.Task2

check :: (a -> Bool) -> a -> Parser s a
check f x = Parser $ \s -> case f x of
  True  -> Just (x, s)
  False -> Nothing

eofConst :: a -> Parser s a
eofConst res = res <$ eof

bracket :: (Integer, String) -> Parser Char (Integer, String)
bracket (balance, prev) =
  (\ch -> case ch of
    '(' -> (balance + 1, prev ++ [ch])
    _   -> (balance - 1, prev ++ [ch])
  )
  <$> (char '(' <|> char ')')
  >>= check (\(b,_) -> b >= 0)

-- |Monadic repeat parser until
repeatPM :: (a -> a)           -- transfrom value before transmissing
         -> a                  -- initial value
         -> (a -> Parser s a)  -- stop parser
         -> (a -> Parser s a)  -- parser to repeat
         -> Parser s a
repeatPM f val stop parser =
  (parser val) >>= (\x -> (stop x) <|> repeatPM f (f x) stop parser)

-- |Checks if given string is correct sequence of parentheses
isPSP :: Parser Char String
isPSP =
  snd
  <$> ((repeatPM id (0, "") eofConst bracket)
  >>= check (\(b,_) -> b == 0))

-- |Succeds if given parser fails
-- fails correspondigly
isnot :: Parser s a -> Parser s ()
isnot parser = Parser $ \s -> case runP parser s of
    Just _  -> Nothing
    Nothing -> Just ((), s)

oneOf :: (a -> Parser s a) -> [a] -> Parser s a
oneOf parser (cur : rest) = parser cur <|> oneOf parser rest
oneOf _ []                = empty

delimitter :: Parser Char Char
delimitter = oneOf char [' ', '\t', '\n']

-- |Repeat parser until
repeatP :: Parser s a  -- to repeat
        -> Parser s a  -- stop
        -> Parser s a
repeatP parser stop =
  parser >> stop <|> repeatP parser stop

-- |Skips delimitters
skipDelimitters :: Parser Char ()
skipDelimitters =
  (isnot delimitter)
  <|> repeatP delimitter_ (isnot delimitter)
  where
    delimitter_ :: Parser Char ()
    delimitter_ = (const ()) <$> delimitter

-- |Parses sequence of numbers into int
integer :: Parser Char Int
integer =  Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- |Parses integer with sign
signedInteger :: Parser Char Int
signedInteger =
  (skipDelimitters *> integer)
  <|> (\sign value -> case sign of
    '-' -> -value
    _   -> value
  )
  <$> (skipDelimitters *> (char '-' <|> char '+'))
  <*> (skipDelimitters *> integer)
