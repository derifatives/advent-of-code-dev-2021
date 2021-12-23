-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day01 (
  day01a
  , day01b
  ) where

-- import           AOC.Prelude
import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List (tails)

makeInteger :: String -> Int
makeInteger = read

decreases :: [Int] -> Int
decreases is = length $ filter (uncurry (<)) (zip is (drop 1 is))

slidingWindows :: [Int] -> Int -> [[Int]]
slidingWindows is n = filter (\l -> length l == n) $ map (take n) $ tails is

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = \i -> Just $ map makeInteger $ lines i
    , sShow  = show
    , sSolve = \i -> Just $ decreases i
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = \i -> Just $ map makeInteger $ lines i
    , sShow  = show
    , sSolve = \i -> Just $ decreases $ map sum $ slidingWindows i 3
    }
