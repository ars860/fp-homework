{-# LANGUAGE BangPatterns #-}

module Task1 ( Point (..)
             , plus
             , minus
             , scalarProduct
             , crossProduct
             , perimeter
             , square
             ) where

data Point = Point { px :: {-# UNPACK #-} !Int
                   , py :: {-# UNPACK #-} !Int
                   } deriving ( Show )

plus :: Point -> Point -> Point
plus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point !x1 !y1) (Point !x2 !y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point !x1 !y1) (Point !x2 !y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point !x1 !y1) (Point !x2 !y2) =  x1 * y2 - x2 * y1

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

-- |Finds perimeter
perimeter :: [Point] -> Double
perimeter points = distancePath 0 points
  where
    firstPoint = head points

    distancePath :: Double -> [Point] -> Double
    distancePath !acc (f : s : rest) = distancePath (acc + distance f s) (s : rest)
    distancePath !acc (f : []) = acc + distance firstPoint f
    distancePath !acc [] = acc

-- |Finds square
square :: [Point] -> Double
square points = (/2) $ fromIntegral $ abs $ squareAcc 0 points
  where
    firstPoint = head points

    squareAcc :: Int -> [Point] -> Int
    squareAcc !acc ((Point x1 y1) : s@(Point x2 y2) : rest) = squareAcc (acc + x1 * y2 - x2 * y1) (s : rest)
    squareAcc !acc ((Point x y) : []) = acc + (py firstPoint) * x - (px firstPoint) * y
    squareAcc !acc [] = acc
