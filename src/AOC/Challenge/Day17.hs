-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day17 (
  day17a
  , day17b
  ) where

import           AOC.Prelude

-- It wasn't worth writing a parser for "target area: x=253..280, y=-73..-46"

sumSquaresTo :: Int -> Int
sumSquaresTo n = (n * (n+1)) `div` 2

maxHeight :: Int -> Int
maxHeight = sumSquaresTo

buildTrajectory :: ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))]
buildTrajectory c@((x, y),(vx, vy)) =
  c : buildTrajectory ((x + vx, y + vy), (max (vx - 1) 0, vy - 1))

checkTrajectory :: ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int))] -> Bool
checkTrajectory b@((xmin, xmax), (ymin, ymax)) (((x, y), (_, _)):rest)
  | x > xmax || y < ymin = False
  | x >= xmin && x <= xmax && y >= ymin && y <= ymax = True
  | otherwise              = checkTrajectory b rest
checkTrajectory _ _ = error "Empty trajectory."

countGoodTrajectories :: (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))) -> Int
countGoodTrajectories (((vxmin, vxmax), (vymin, vymax)), bounds) =
  let all_vs = [(vx, vy) | vx <- [vxmin..vxmax], vy <- [vymin..vymax]]
      checker = (\vs -> checkTrajectory bounds (buildTrajectory ((0, 0), vs)))
  in length $ filter (== True) $ map checker all_vs
  
day17a :: _ :~> Int
day17a = MkSol
    { sParse = Just . const (abs (-73) - 1)
    , sShow  = show
    , sSolve = Just . maxHeight
    }

day17b :: _ :~> Int
day17b = MkSol
    { sParse = Just . const (((22, 280), (-73, 73)), ((253, 280), (-73, -46)))
    , sShow  = show
    , sSolve = Just . countGoodTrajectories
    }
