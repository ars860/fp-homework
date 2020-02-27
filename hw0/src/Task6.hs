module Task6
  ( fun1
  , fun2
  ) where

import qualified Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _  = False

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) =
  let rs = mapMaybe f xs in
    case f x of
      Nothing -> rs
      Just r  -> r : rs

-- | function that returns two left harolds
-- WHNF: ( Left "harold" ++ " hide " ++ "the " ++ "pain"
--       , Left "harold" ++ " hide " ++ "the " ++ "pain")
fun1 :: (Either String a, Either String b)
fun1 = Task1.distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))


-- | function that returns False
-- WHNF: False
-- Because we will not get weak head normal form
-- before null will be executed
fun2 :: Bool
fun2 = Task6.null $ mapMaybe foo "pole chudes ochen' chudesno"
