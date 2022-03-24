-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day10 (
  day10a
  , day10b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List(sort)

openToClose :: [(Char, Char)]
openToClose = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

safeLookup :: Eq a => a -> [(a, b)] -> b
safeLookup = (fromJust .) . lookup

safeCloseChar :: Char -> Char
safeCloseChar c = safeLookup c openToClose

parseLine :: String -> (String, Maybe Int)
parseLine s = parseLine' s [] where
  parseLine' [] st    = (st, Nothing)
  parseLine' (x:xs) st =
    if elem x (map fst openToClose)
    then parseLine' xs (x:st)
    else if st == [] || safeCloseChar (head st) /= x
         then (st, lookup x [(')', 3), (']', 57), ('}', 1197), ('>', 25137)])
         else parseLine' xs (tail st)

scoreRemainder :: Int -> String -> Int
scoreRemainder s []     = s
scoreRemainder s (c:cs) =
  scoreRemainder (5*s + safeLookup (safeCloseChar c) scores) cs
  where scores = [(')', 1), (']', 2), ('}', 3), ('>', 4)]

median :: Ord a => [a] -> a
median as =
  let sorted = sort as
  in sorted !! (length as `div` 2)

day10a :: [String] :~> Int
day10a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map (fromJust . snd) . filter (isJust . snd) . map parseLine
    }

day10b :: [String] :~> Int
day10b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . median . map ((scoreRemainder 0) . fst) . filter (isNothing . snd) . map parseLine
    }
