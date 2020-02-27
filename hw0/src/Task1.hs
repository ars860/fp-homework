{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | function that takes (a | (b, c)) and returns
-- (a | b, a | c)
-- where "|" is a short notation for Either
distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left x)       = (Left x, Left x)
distributivity (Right (x, y)) = (Right x, Right y)

-- | function that takes (a, (b, c)) and
-- returns ((a, b), c)
associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- | function that returns
-- ((a | (b | c)) -> ((a | b) | c), ((a | b) | c) -> (a | (b | c)))
-- where "|" is a short notation for Either
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
