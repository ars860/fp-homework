{-# LANGUAGE InstanceSigs #-}

module Block2.Task1 () where

import Block1.Task3 (Tree (..))
import Block3.Task2 (NonEmpty (..))

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Node left (value :| []) right) =
    foldr f (f value $ foldr f acc left) right
  foldr f acc (Node left (value :| (first : rest)) right) =
    f value $ foldr f acc (Node left (first :| rest) right)
  foldr _ acc Leaf = acc

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f = foldr ((<>) . f) mempty
