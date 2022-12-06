-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
  day06a
  , day06b
  ) where

import           AOC.Prelude

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List (nub)

findFirstPositionWithNUnique :: Int -> String -> Int
findFirstPositionWithNUnique n s =
  let firstN = take n s
  in if firstN == nub (firstN) then n else 1 + findFirstPositionWithNUnique n (tail s)


day06a :: String :~> Int
day06a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . findFirstPositionWithNUnique 4
    }

day06b :: String :~> Int
day06b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just  . findFirstPositionWithNUnique 14
    }
