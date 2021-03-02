module Main where

import Criterion.Main
import Task1

data PointBad = PointBad { pbx :: Int
                         , pby :: Int
                         }

distanceBad :: PointBad -> PointBad -> Double
distanceBad (PointBad x1 y1) (PointBad x2 y2) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

perimeterSlow :: [PointBad] -> Double
perimeterSlow points = distancePathSlow points
  where
    firstPoint = head points

    distancePathSlow :: [PointBad] -> Double
    distancePathSlow (f : s : rest) = distanceBad f s + distancePathSlow (s : rest)
    distancePathSlow (f : []) = distanceBad firstPoint f
    distancePathSlow [] = 0

squareSlow :: [PointBad] -> Double
squareSlow points = (/2) $ fromIntegral $ abs $ squareAccSlow points
  where
    firstPoint = head points

    squareAccSlow :: [PointBad] -> Int
    squareAccSlow (f : s : rest) = (pbx f) * (pby s) - (pbx s) * (pby f) + squareAccSlow (s : rest)
    squareAccSlow (f : []) = (pby firstPoint) * (pbx f) - (pbx firstPoint) * (pby f)
    squareAccSlow [] = 0

main :: IO ()
main = do
  let
    points = map (\x -> Point x x) ([1..10000000] :: [Int])
    badPoints = map (\x -> PointBad x x) ([1..10000000] :: [Int])
  defaultMain [
      bgroup "perimeter" [bench "diagonal" $ nf perimeter points],
      bgroup "square"    [bench "diagonal" $ nf square points],
      bgroup "perimeterSlow" [bench "diagonal" $ nf perimeterSlow badPoints],
      bgroup "squareSlow"    [bench "diagonal" $ nf squareSlow badPoints]
    ]
