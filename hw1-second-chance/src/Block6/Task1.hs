{-# LANGUAGE InstanceSigs #-}

module Block6.Task1 (Parser(..)) where

import Control.Applicative
import Data.Bifunctor (first)

-- |Parser, parsing input, given as list of tokens of type s,
-- returns res as a in Maybe
newtype Parser s a = Parser { runP :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \s -> pure (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser pf <*> Parser pa = Parser $ \s -> do
      (res1, rest1) <- pf s
      (res2, rest2) <- pa rest1
      return (res1 res2, rest2)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser pa >>= f = Parser $ \s -> do
    (res1, rest1) <- pa s
    runP (f res1) rest1

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pa <|> Parser pb = Parser $ \s -> (pa s) <|> (pb s)
