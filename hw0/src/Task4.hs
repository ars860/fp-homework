module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function

-- | error message if given integer is not positive.
errorMessage :: String
errorMessage = "Expected positive Integer."

iterateElement :: a -> [a]
iterateElement = fix $ \rec n -> n : (rec n)

fibonacci :: Integer -> Integer
fibonacci =
  fix $ \rec n -> case n of
    x | x == 0 || x == 1 -> 1
      | x < 0            -> error errorMessage
      | otherwise        -> (rec $ x - 1) + (rec $ x - 2)

factorial :: Integer -> Integer
factorial =
  fix $ \rec n -> case n of
    x | x == 0    -> 1
      | x < 0     -> error errorMessage
      | otherwise -> (rec $ x - 1) * x

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ \rec f arr -> case arr of
  []          -> []
  curr : rest -> (f curr) : (rec f rest)
