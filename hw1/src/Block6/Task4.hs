module Block6.Task4 ( listlistParser ) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Control.Monad.Loops (untilM)

import Block6.Task1 (Parser (..))
import Block6.Task2 (char, eof)
import Block6.Task3 (isnot, signedInteger, skipDelimitters)

comma :: Parser Char Char
comma = char ','

listParser :: Int -> Parser Char [Int]
listParser amount =
  replicateM amount $
    skipDelimitters *> comma *> signedInteger

isParsed :: Parser a b -> Parser a Bool
isParsed parser = Parser $ \s -> case runP parser s of
    Just _  -> Just (True, s)
    Nothing -> Just (False, [])

-- |Parses list of lists of integer represented in integers
-- as sequence of thir lengths and elements
listlistParser :: Parser Char [[Int]]
listlistParser =
  untilM
    (do
      x <- signedInteger <* skipDelimitters
      listParser x
      <*
        (skipDelimitters *> comma *> isnot (skipDelimitters *> eof)
        <|> skipDelimitters *> eof)
    )
    (not
     <$> (isParsed
      $  isnot
      $  skipDelimitters *> eof)
    )
