{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 ( Nat (..)
                    , isEven
                    , natDiv
                    , natMod
                    , natToInteger
                    ) where

-- |Peano like natural number
data Nat = Z | S Nat
         deriving (Show)

-- |Converts Nat to Integer
natToInteger :: Nat -> Integer
natToInteger (S x) = 1 + natToInteger x
natToInteger Z     = 0

instance Num Nat where
  fromInteger :: Integer -> Nat
  fromInteger x | x <= 0 = Z
                | otherwise = S $ fromInteger (x - 1)

  (+) :: Nat -> Nat -> Nat
  a + S b = S $ a + b
  a + Z = a

  (-) :: Nat -> Nat -> Nat
  S a - S b = a - b
  a - Z = a
  _ - _ = Z

  (*) :: Nat -> Nat -> Nat
  a * S b = a + a * b
  _ * Z = Z

  abs :: Nat -> Nat
  abs = id

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Z == Z = True
  S a == S b = a == b
  _ == _ = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  a <= b = a - b == Z

-- |Is even
isEven :: Nat -> Bool
isEven = (== 0) . (`mod` 2) . natToInteger

-- |Div for Nat numbers
natDiv :: Nat -> Nat -> Nat
natDiv a b | a >= b = S $ natDiv (a - b) b
           | otherwise = Z

-- |Mod for Nat numbers
natMod :: Nat -> Nat -> Nat
natMod a b = a - (natDiv a b * b)
