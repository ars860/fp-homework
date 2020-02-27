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

-- | function returning infinite array of given element
iterateElement :: a -> [a]
iterateElement = fix $ \rec n -> n : (rec n)

-- | function that returns n-th fibonacci number,
-- where 0-th and 1-th fibonacci numbers is 1 and 1
-- if given index is negative number then error is thrown
fibonacci :: Integer -> Integer
fibonacci =
  fix $ \rec n -> case n of
    x | x == 0 || x == 1 -> 1
      | x < 0            -> error errorMessage
      | otherwise        -> (rec $ x - 1) + (rec $ x - 2)

-- | function that returns n!
-- if given index is negative number then error is thrown
factorial :: Integer -> Integer
factorial =
  fix $ \rec n -> case n of
    x | x == 0    -> 1
      | x < 0     -> error errorMessage
      | otherwise -> (rec $ x - 1) * x

-- | function that applies given function to each member of given list
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ \rec f arr -> case arr of
  []          -> []
  curr : rest -> (f curr) : (rec f rest)
