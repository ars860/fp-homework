module Block3.Task1 ( maybeConcat
                    , eitherConcat
                    ) where

import Data.Maybe (fromMaybe)

-- |Concats list of maybe lists in one list
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = (fromMaybe []) . mconcat

-- |Concats list of eithers into one pair of elements
-- mapping them to each other using monoid
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat =
  foldr
    (\cur (a, b) -> case cur of
      Right x -> (a, x <> b)
      Left y  -> (y <> a, b)
    )
    (mempty, mempty)
