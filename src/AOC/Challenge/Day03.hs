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
import Data.List.Split (chunksOf)
import qualified Data.Set as S

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = fromEnum c - fromEnum 'a' + 1
  | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
  | otherwise            = undefined

firstElt :: S.Set a -> a
firstElt = head . S.toList

findIntersection :: String -> Char
findIntersection s =
  let (l, r) = splitAt (length s `div` 2) s
  in firstElt $ S.intersection (S.fromList l) (S.fromList r)

findGroupIntersection :: [String] -> Char
findGroupIntersection bs = firstElt $ foldr1 S.intersection (map S.fromList bs)

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
