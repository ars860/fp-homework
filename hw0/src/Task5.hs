module Task5
  ( churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

-- | zero in Church calculus
zero :: Nat a
zero _ x = x

-- | (+1) in Church calculus
succChurch :: Nat a -> Nat a
succChurch a = \f x -> a f (f x)

-- | function that adds up two Church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b = \f x -> a f (b f x)

-- | function that multiplies up two Church numerals
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b = \f x -> a (b f) x

-- | function that convert given Church numeral to Integer
churchToInt :: Nat Integer -> Integer
churchToInt a = a (+ 1) 0
