-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
  day03a
  , day03b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List (intersect)
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = fromEnum c - fromEnum 'a' + 1
  | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
  | otherwise            = undefined

halves :: [a] -> ([a], [a])
halves s = splitAt (length s `div` 2) s 

findIntersection :: String -> Char
findIntersection = head . uncurry intersect . halves

findGroupIntersection :: [String] -> Char
findGroupIntersection = head . foldr1 intersect

day03a :: [String] :~> Int
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map (priority . findIntersection)
    }

day03b :: [String] :~> Int
day03b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map (priority . findGroupIntersection) . chunksOf 3
    }
