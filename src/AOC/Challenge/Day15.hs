-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
  day15a
  , day15b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Monad (liftM2)
import Data.Char (isDigit)
import Data.List (findIndex, nub)
import Data.Maybe (fromJust)

type Pos = (Int, Int)
type Iv = (Int, Int)

parser :: String -> Maybe [(Pos, Pos)]
parser = Just . map parseLine . lines
  where parseLine l =
          let ws = words l
          in ((rt 2 ws, rt 3 ws), (rt 8 ws, rt 9 ws))
        rt i = read . takeWhile (liftM2 (||) isDigit (== '-')). drop 2 . (!! i)

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

rowCoverage1 :: Int -> Pos -> Pos -> Maybe Iv
rowCoverage1 row s@(x, y) b =
  let m = manhattan s b
      r = m - abs (row - y)
  in if r >= 0 then Just (x - r, x + r) else Nothing

mergeOverlapping :: Iv -> Iv -> Iv
mergeOverlapping (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)

overlaps :: Iv -> Iv -> Bool
overlaps (s1, e1) (s2, e2) =
  (s1 <= s2 && s2 <= e1) || (s2 <= s1 && s1 <= e2)

merge :: Maybe Iv -> [Iv] -> [Iv]
merge Nothing is = is
merge (Just i) is =
  let (b, e) = break (overlaps i) is
  in if null e
     then i : is
     else merge (Just $ mergeOverlapping i (head e)) (b ++ tail e)

disjointSize :: [Iv] -> Int
disjointSize = sum . map (\(s, e) -> e - s + 1) 

beaconsInRow :: Int -> [(Pos, Pos)] -> Int
beaconsInRow i =
  length . nub . filter ((==) i . snd) . map snd

rowCoverage :: Int -> [(Pos, Pos)] -> Int
rowCoverage row sbs =
  disjointSize (rowIntervals row sbs) - beaconsInRow row sbs

rowIntervals :: Int -> [(Pos, Pos)] -> [Iv]
rowIntervals row = foldr (merge . uncurry (rowCoverage1 row)) []

clipRowIntervals :: Iv -> [Iv] -> [Iv]
clipRowIntervals _ [] = []
clipRowIntervals i@(b,e) (i'@(b',e'):r) = 
  if overlaps i i'
  then (max b b', min e e'):clipRowIntervals i r
  else clipRowIntervals i r

top :: Int
top = 4000000

missingElt :: [Iv] -> Int
missingElt [(_, e1), (_, e2)] = min e1 e2 + 1
missingElt _ = undefined

day15a :: _ :~> _
day15a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . rowCoverage 2000000
    }

solve2 :: Int -> [(Pos, Pos)] -> Int
solve2 t sbs =
  let clipped = map (clipRowIntervals (0, t) . flip rowIntervals sbs) [0..t]
      y = fromJust $ findIndex ((< t + 1) . disjointSize) clipped
      x = missingElt $ clipped !! y
  in x * top + y

day15b :: _ :~> _
day15b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve2 top
    }
