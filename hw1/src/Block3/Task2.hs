{-# LANGUAGE InstanceSigs #-}

module Block3.Task2 ( NonEmpty (..)
                    , ThisOrThat (..)
                    , Name (..)
                    , Endo (..)
                    ) where

-- |Type holding non empty list
data NonEmpty a = a :| [a]
                deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| restx) <> (y :| resty) = (x :| (restx ++ [y] ++ resty))

-- |Type holding a or b or both
data ThisOrThat a b = This a
                    | That b
                    | Both a b
                    deriving (Show, Eq)

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This x <> That y     = Both x y
  This x <> This _     = This x
  That x <> That _     = That x
  That x <> This y     = Both y x
  Both x y <> _        = Both x y
  This x1 <> Both _ y2 = Both x1 y2
  That y1 <> Both x2 _ = Both x2 y1

-- |Type holding names separated by point
newtype Name = Name String
             deriving (Show, Eq)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  Name s1 <> Name s2 | s1 /= "" && s2 /= "" = Name $ s1 ++ "." ++ s2
                     | otherwise            = Name $ s1 ++ s2

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

-- |Endo, I guess
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  Endo f1 <> Endo f2 = Endo $ \a -> f1 $ f2 a

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
