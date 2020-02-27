{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left x)       = (Left x, Left x)
distributivity (Right (x, y)) = (Right x, Right y)

associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

eitherAssoc
  :: Either a (Either b c)
  <-> Either (Either a b) c
eitherAssoc = (leftPart, rightPart)
  where
    leftPart :: Either a (Either b c) -> Either (Either a b) c
    leftPart (Left a)          = Left (Left a)
    leftPart (Right (Left b))  = Left (Right b)
    leftPart (Right (Right c)) = Right c
    rightPart :: Either (Either a b) c -> Either a (Either b c)
    rightPart (Right c)        = Right (Right c)
    rightPart (Left (Left a))  = Left a
    rightPart (Left (Right b)) = Right (Left b)
