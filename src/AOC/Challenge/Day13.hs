-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
  day13a
  , day13b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Point = (Int, Int)
type Points = S.Set Point

data Instruction = X Int | Y Int

parser :: String -> Maybe (Points, [Instruction])
parser input =
  let elts = splitOn "\n\n" input
      points = S.fromList $ map (\i -> read ("(" ++ i ++ ")") :: Point) (lines (head elts))
      instructions = map readInstructions (lines (elts !! 1))
  in Just (points, instructions)
  where
    readInstructions l =
      let w = splitOn "=" (words l !! 2)
          value = read (w !! 1)
      in if head w == "x" then X value else Y value

foldPoint :: Instruction -> Point -> Point
foldPoint (X i) (x, y) = (min x (2*i-x), y)
foldPoint (Y i) (x, y) = (x, min y (2*i-y))

printPoints :: Points -> String
printPoints p =
  let maxvals = foldl (\(xn, yn) (x, y) -> (max xn x, max yn y)) (0, 0) (S.toList p)
  in unlines [[if S.member (x, y) p then '0' else ' ' | x <- [0..(fst maxvals)]] | y <- [0..(snd maxvals)]]
  
processFold :: Points -> Instruction -> Points
processFold = flip (S.map . foldPoint)

day13a :: _ :~> _
day13a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \(ps, is) -> Just $ length $ processFold ps (head is)
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \(ps, is) -> Just $ printPoints $ foldl processFold ps is
    }
