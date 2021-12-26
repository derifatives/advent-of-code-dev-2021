-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
  day03a
  , day03b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Char(digitToInt)
import Data.Map as M(Map, fromListWith, (!))

fromBinary :: [Int] -> Int
fromBinary = foldl (\t d -> 2*t + d) 0

counts :: (Ord a) => [a] -> Map a Int
counts = M.fromListWith (+) . (flip zip) (repeat 1)

mostCommon :: [String] -> Int -> Char
mostCommon ls pos =
  let cs = counts $ map (!! pos) ls in
    if (cs M.! '1') >= (cs M.! '0') then '1' else '0'

leastCommon :: [String] -> Int -> Char
leastCommon ls pos = if mostCommon ls pos == '1' then '0' else '1'

getCommonishFiltering :: [String] -> ([String] -> Int -> Char) -> Int -> String
getCommonishFiltering (l:[]) _ _ = l
getCommonishFiltering ls commonProc pos =
  let keep = commonProc ls pos in
    getCommonishFiltering (filter (\l -> (l !! pos) == keep) ls) commonProc (pos + 1)

readBinaryInt :: String -> Int
readBinaryInt = fromBinary . map digitToInt
  
day03a :: [String] :~> (Int, Int)
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show . uncurry (*)
    , sSolve = \ls -> let ns = [0..(length(head ls) - 1)] in
        Just $ (readBinaryInt $ map (mostCommon ls) ns,
                 readBinaryInt $ map (leastCommon ls) ns)
    }

day03b :: [String] :~> (Int, Int)
day03b = MkSol
    { sParse = Just . lines
    , sShow  = show . uncurry (*)
    , sSolve = \ls -> Just $ (readBinaryInt $ getCommonishFiltering ls mostCommon 0,
                              readBinaryInt $ getCommonishFiltering ls leastCommon 0)
    }
