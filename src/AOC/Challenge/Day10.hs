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
import Data.List.Split (chunksOf)

parser :: String -> Maybe [[String]]
parser = Just . map words . lines

process :: [Int] -> [[String]] -> [Int]
process ns [] = reverse ns
process ns@(hn:_) (inst:insts) =
  case inst of
    ["noop"] -> process (hn:ns) insts
    ["addx", nw] -> let nv = hn + (read nw) in process (nv:hn:ns) insts
    _ -> undefined
process _ _ = undefined

signalStrengths :: [Int] -> [Int] -> [Int]
signalStrengths inds ss = map (\i -> i * ss !! i) inds

drawCRT :: [Int] -> [String]
drawCRT ss = chunksOf 40 $ map posToChar [0..239]
  where posToChar i = if abs (i `mod` 40 - ss !! (i + 1)) <= 1 then '#' else '.'
    
day10a :: [[String]] :~> Int
day10a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . signalStrengths [20, 60, 100, 140, 180, 220] . process [1, 0]
    }

day10b :: [[String]] :~> [String]
day10b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . drawCRT . process [1, 0]
    }
