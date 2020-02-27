module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | proof of a -> !!a
doubleNeg :: a -> Neg (Neg a)
doubleNeg f x = x f

-- | proof of !!(a | !a)
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = f $ Right (f . Left)

-- | Pierce law is impossible in institutional logic
-- and in haskell
-- Proof using T/N/F model
-- Let a = N, b = F
-- a -> b = !a ∨ b = F ∨ F = F
-- (a -> b) -> a = !F ∨ N = T ∨ N = T
-- ((a -> b) -> a) -> a = !T ∨ N = N
-- N != T => ! |- ((a -> b) -> a) -> a
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Double Negation Elimination is impossible in institutional logic
-- and in haskell
-- Proof using T/N/F model
-- Let a = N
-- !!a -> a
-- !a = !N = F
-- !!a = !F = T
-- !!a -> a = !T ∨ N = N
-- N != T => ! |- !!a -> a
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | proof of !!!a -> !a
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f a = f $ \g -> g a
