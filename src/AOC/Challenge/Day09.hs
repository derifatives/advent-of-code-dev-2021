{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day09 (
  day09a
  , day09b
  ) where

import           AOC.Prelude
import Data.Char(digitToInt)
import Data.Array
import Data.List.Split(splitOn)
import Data.Sort(sort)

type Pos = (Int, Int)
type Heightmap = Array Pos Int
type Usedmap = Array Pos Bool

parser :: String -> Maybe Heightmap
parser input = Just $ listArray ((1, 1),(num_rows, num_columns)) elts
  where lls = map (map digitToInt) $ lines input
        num_rows = length lls
        num_columns = length (head lls)
        elts = concat lls

neighbors :: Heightmap -> Pos -> [Pos]
neighbors h (r, c) = filter (inRange (bounds h)) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

lowPointP :: Heightmap -> Pos -> Bool
lowPointP h i = all (\n -> h ! i < h ! n) (neighbors h i)

totalRisk :: Heightmap -> Int
totalRisk h = sum $ map (\i -> 1 + h ! i) $ filter (lowPointP h) $ indices h

buildBasin :: Heightmap -> Usedmap -> [Pos] -> Int -> (Int, Usedmap)
buildBasin _ u [] i = (i, u)
buildBasin h u (p:ps) i =
  if usable p
  then buildBasin h (u // [(p, True)]) ((neighbors h p) ++ ps) (i+1)
  else buildBasin h u ps i 
  where usable p' = h ! p' < 9 && u ! p' == False

buildBasins :: Heightmap -> [Int]
buildBasins h = fst $ foldr buildOne initial (indices h)
  where initial = ([], listArray (bounds h) (repeat False))
        buildOne p (bs, u) =
          let (b, u') = buildBasin h u [p] 0
          in if b > 0 then (b:bs, u') else (bs, u')

day09a :: Heightmap :~> Int
day09a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . totalRisk
    }

day09b :: Heightmap :~> Int
day09b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . product . take 3 . reverse . sort . buildBasins 
    }
