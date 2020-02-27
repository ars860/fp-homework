module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function

iterateElement :: a -> [a]
iterateElement = fix (\rec n -> n : (rec n))

fibonacci :: Integer -> Integer
fibonacci = fix (\rec n -> if (n == 0 || n == 1) then 1 else (rec $ n - 1) + (rec $ n - 2))

factorial :: Integer -> Integer
factorial = fix (\rec n -> if n == 0 then 1 else (rec $ n - 1) * n)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ \rec f arr -> case arr of
  []          -> []
  curr : rest -> (f curr) : (rec f rest)
