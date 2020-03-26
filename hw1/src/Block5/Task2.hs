{-# LANGUAGE RecordWildCards #-}

module Block5.Task2 (moving) where

import Control.Monad.State (State, evalState, get, put)

data WindowHolder = WindowHolder { window  :: [Double]
                                 , maxSize :: Int
                                 } deriving (Show)

updateWindow :: Double -> State WindowHolder Double
updateWindow new = do
  (WindowHolder {..}) <- get
  let curSize = length window
  let newWindow =
        if curSize == maxSize
        then (tail window) ++ [new]
        else window ++ [new]
  let newSize = length newWindow
  put (WindowHolder newWindow maxSize)
  return (sum newWindow / fromIntegral newSize)

-- |Moving average
moving :: Int -> [Double] -> [Double]
moving sz list =
  evalState (mapM updateWindow list) (WindowHolder [] sz)
