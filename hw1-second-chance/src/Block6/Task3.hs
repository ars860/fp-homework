module Block6.Task3 ( isPSP
                    , signedInteger
                    , isnot
                    , skipDelimitters
                    , integer
                    , isChanged
                    ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)

import Block6.Task1
import Block6.Task2

isChanged :: Eq s => Parser s a -> Parser s [s]
isChanged parser = Parser $ \s -> case runP parser s of
  Nothing -> Nothing
  Just (_, newS) ->
    if newS == s
    then Nothing
    else Just (take (length s - length newS) s, newS)

isPSPrec :: Parser Char ()
isPSPrec = ((char '(') *> isPSPrec <* (char ')') <|> ok)

-- |Checks if given string is correct sequence of parentheses
isPSP :: Parser Char String
isPSP = isChanged isPSPrec

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

-- |Skips delimitters
skipDelimitters :: Parser Char ()
skipDelimitters = ok <* (many delimitter)

-- |Parses sequence of numbers into int
integer :: Parser Char Int
integer = read <$> (some $ satisfy isDigit)

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
