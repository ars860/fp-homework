module Block4.Task1 (stringSum) where

import GHC.Base (liftA2)
import Text.Read

-- |Reads numbers from String and adds them
stringSum :: String -> Maybe Int
stringSum =
  (foldr
    (\cur res ->
      liftA2 (+) res (readMaybe cur :: Maybe Int)
    )
    (Just 0)
  ) . words

