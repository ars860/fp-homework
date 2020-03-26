module Block2.Task2 ( splitOn
                    , joinWith
                    ) where

import Block4.Task3 ()

-- |Split string by given char
splitOn :: Char -> String -> [String]
splitOn sep =
  foldr
  (\cur (first : rest) ->
    if cur == sep
    then"" : (first : rest)
    else (cur : first) : rest
  )
  ([""])

-- |Joins strings with given char
joinWith :: Char -> [String] -> String
joinWith sep =
  tail . foldMap ((:) sep)
