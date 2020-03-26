{-# LANGUAGE InstanceSigs #-}

module Block4.Task2 (Tree (..)) where

-- |Another one Tree
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)            = Leaf $ f a
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure x = Leaf x

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f <*> Leaf x = Leaf $ f x
  Leaf f <*> Branch left right = Branch (Leaf f <*> left) (Leaf f <*> right)
  Branch leftf rightf <*> Branch left right =
    Branch (leftf <*> left) (rightf <*> right)
  Branch leftf rightf <*> Leaf x = Branch (leftf <*> Leaf x) (rightf <*> Leaf x)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc (Leaf value)        = f value acc
  foldr f acc (Branch left right) = foldr f (foldr f acc left) right

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf value) = Leaf <$> (f value)
  traverse f (Branch left right) =
    Branch <$> (traverse f left) <*> (traverse f right)
