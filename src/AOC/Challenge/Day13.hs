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
import Data.Char (isDigit)
import Data.List (elemIndices, findIndices, sortBy)
import Data.List.Split (chunksOf)

pullDigit :: String -> (Int, String)
pullDigit s =
  let (di, r) = span isDigit s
  in (read di, r)

listize :: String -> String
listize s = let (d, r) = pullDigit s in ('[':show d) ++ (']':r)

matchedP :: String -> String -> Ordering
matchedP [] [] = undefined
matchedP "]" "]" = undefined
matchedP ('[':r1) ('[':r2) = matchedP r1 r2
matchedP (']':r1) (']':r2) = matchedP r1 r2
matchedP (',':r1) (',':r2) = matchedP r1 r2
matchedP (']':_) _ = LT
matchedP _ (']':_) = GT
matchedP s1 s2@('[':_) = matchedP (listize s1) s2
matchedP s1@('[':_) s2 = matchedP s1 (listize s2)
matchedP s1 s2 =
  let (d1, r1) = pullDigit s1
      (d2, r2) = pullDigit s2
  in case compare d1 d2 of
    LT -> LT
    GT -> GT
    EQ -> matchedP r1 r2

compareChunk :: [String] -> Ordering
compareChunk lls = matchedP (lls !! 0) (lls !! 1)

day13a :: _ :~> Int
day13a = MkSol
    { sParse = Just . chunksOf 3 . lines
    , sShow  = show
    , sSolve = Just . sum . map (1+) . elemIndices LT . map compareChunk
    }

divs :: [String]
divs = ["[[2]]", "[[6]]"]
  
day13b :: _ :~> Int
day13b = MkSol
    { sParse = Just . filter (not . null) . lines
    , sShow  = show
    , sSolve = Just . product . map (1+) . findIndices (`elem` divs) . sortBy matchedP . (divs ++)
    }
