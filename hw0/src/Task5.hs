module Task5
  ( churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch a = \f x -> a f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b = \f x -> a f (b f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult a b = \f x -> a (b f) x

churchToInt :: Nat Integer -> Integer
churchToInt a = a (\x -> x + 1) 0
