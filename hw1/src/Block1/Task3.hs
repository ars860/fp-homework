{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 ( Tree (..)
                    , isEmpty
                    , size
                    , find
                    , insert
                    , fromList
                    , remove
                    ) where

import Block3.Task2 (NonEmpty (..))

-- |Search tree
data Tree a = Leaf
            | Node (Tree a) (NonEmpty a) (Tree a)
            deriving (Show)

-- |Is empty
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- |Amount of elements stored in tree
size :: Tree a -> Integer
size Leaf = 0
size (Node left (_ :| rest) right) =
  size left + (size right) + (toInteger $ length rest) + 1

-- |Returns True if tree has given element in it
find :: Ord a => Tree a -> a -> Bool
find Leaf _ = False
find (Node left (value :| _) right) searched
  | searched == value = True
  | searched < value  = find left searched
  | otherwise         = find right searched

-- |Inserts element in tree
insert :: Ord a => Tree a -> a -> Tree a
insert Leaf searched = Node Leaf (searched :| []) Leaf
insert (Node left values@(value :| rest) right) searched
  | searched == value = Node left (value :| (value : rest)) right
  | searched < value  = Node (insert left searched) values right
  | otherwise         = Node left values (insert right searched)

-- |Builds tree from list
fromList :: Ord a => [a] -> Tree a
fromList = foldr (flip insert) Leaf

instance Ord a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Leaf == Leaf = True
  Node l1 v1 r1 == Node l2 v2 r2 = l1 == l2 && v1 == v2 && r1 == r2
  _ == _ = False

-- |Removes element from tree
remove :: Ord a => Tree a -> a -> Tree a
remove Leaf _ = Leaf
remove (Node left values@(value :| rest) right) searched
  | searched < value              = Node (remove left searched) values right
  | searched > value              = Node left values (remove right searched)
  | rest /= []                    = Node left (value :| tail rest) right
  | left == Leaf && right /= Leaf = right
  | left /= Leaf && right == Leaf = left
  | otherwise                     =
    let (newRight, newValue) = findClosest right in
      case newValue of
        Nothing    -> Leaf
        (Just val) -> Node left val newRight
  where
    findClosest :: Ord a => Tree a -> (Tree a, Maybe (NonEmpty a))
    findClosest Leaf = (Leaf, Nothing)
    findClosest (Node left values right)
      | left == Leaf = (right, Just values)
      | otherwise =
        let (leftModified, saved) = findClosest left in
          (Node leftModified values right, saved)
