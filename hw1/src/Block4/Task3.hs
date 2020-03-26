{-# LANGUAGE InstanceSigs #-}

module Block4.Task3 (toList) where

import Data.Maybe (fromJust)

import Block3.Task2 (NonEmpty (..))

-- |Converts list to maybe NonEmpty
fromListMaybe :: [a] -> Maybe (NonEmpty a)
fromListMaybe (first : rest) = Just $ first :| rest
fromListMaybe _              = Nothing

-- |Converts list to NonEmpty, falling on empty lists
fromList :: [a] -> NonEmpty a
fromList list = fromJust $ fromListMaybe list

-- |Converts given NonEmpty to list
toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| rest) = (f x :| fmap f rest)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  x <*> y = fromList $ (toList x) <*> (toList y)

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  x >>= f = fromList $ (toList x) >>= (toList . f)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc x = foldr f acc (toList x)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f x = pure fromList <*> traverse f (toList x)
