-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!


module AOC.Challenge.Day05 (
  day05a
  , day05b
  ) where

import           AOC.Prelude
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Stacks = M.Map Int String
data Move = Move Int Int Int deriving stock (Show)

parser :: String -> Maybe (Stacks, [Move])
parser s =
  let parts = splitOn [""] $ lines s
  in Just (parseStacks $ parts !! 0, map parseMove $ parts !! 1)

parseStacks :: [String] -> Stacks
parseStacks = M.fromList .
               map (makePair . reverse . dropWhile isSpace . reverse)  .
               filter (isDigit . head) . map reverse . transpose
  where makePair l = (read (take 1 l) - 1, reverse (tail l))

parseMove :: String -> Move
parseMove l =
  let ws = splitOn " " l
      ints = map read [ws !! i | i <- [1, 3, 5]]
  in Move (ints !! 0) (ints !! 1 - 1) (ints !! 2 - 1)

executeMove :: Bool -> Stacks -> Move -> Stacks
executeMove reverseP s (Move count from to) =
  let (move, rest) = splitAt count (forceLookup from s)
      move' = if reverseP then reverse move else move
  in M.insert to (concat [move', forceLookup to s]) (M.insert from rest s)
  where forceLookup k m = fromJust $ M.lookup k m
  
computeResult :: Stacks -> String
computeResult = M.foldr (\n r -> (head n) : r) "" 

day05a :: (Stacks, [Move]) :~> String
day05a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . computeResult . \(s, m) -> foldl' (executeMove True) s m 
    }

day05b :: (Stacks, [Move]) :~> String
day05b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . computeResult . \(s, m) -> foldl' (executeMove False) s m 
    }
