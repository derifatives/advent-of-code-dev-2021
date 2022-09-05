-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day14 (
  day14a
  , day14b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List (foldl')
import qualified Data.Map.Strict as M

type Rules = M.Map String Char
type PairCounts = M.Map String Integer
type CharCounts = M.Map Char Integer

addToMap :: (Ord a, Num b) => b -> a -> M.Map a b -> M.Map a b
addToMap b a = M.insertWith (+) a b

parser :: String -> Maybe (String, Rules)
parser input =
  let lls = lines input
  in Just (head lls,
           M.fromList (map ((\l -> (head l, head (l !! 2))) . words) (drop 2 lls)))

buildPairCounts :: String -> PairCounts
buildPairCounts s = foldl' f M.empty (zip s (tail s))
  where f = \cnts (c1, c2) -> addToMap 1 (c1 : [c2]) cnts

toCharCounts :: Char -> PairCounts -> CharCounts
toCharCounts c = addToMap 1 c . M.foldlWithKey update M.empty
  where
    update cs p i = addToMap i (p !! 1) cs

updatePairCounts :: Rules -> PairCounts -> PairCounts
updatePairCounts rules = M.foldlWithKey update M.empty
  where
    update cs s i =
      let adder = addToMap i in
        case M.lookup s rules of
          Nothing -> adder s cs
          Just rhs -> adder (rhs : tail s) (adder (head s:[rhs]) cs)

reduceOnSecond :: (b -> b -> Bool) -> [(a, b)] -> (a, b)
reduceOnSecond f = foldl1 (\t1@(_, b1) t2@(_, b2) -> if f b1 b2 then t1 else t2)

range :: (Num b, Ord b) => [(a, b)] -> b
range l = snd (reduceOnSecond (>) l) - snd (reduceOnSecond (<) l)

process :: Int -> (String, Rules) -> Maybe Integer
process iters (input, rules) =
  let iterates = iterate (updatePairCounts rules) (buildPairCounts input)
  in Just . range . M.toList . toCharCounts (head input) . (!! iters) $ iterates

day14a :: (String, Rules) :~> Integer
day14a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = process 10
    }

day14b :: (String, Rules) :~> Integer
day14b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = process 40
    }
