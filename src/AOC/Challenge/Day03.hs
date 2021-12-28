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
import Control.Monad(join,)
import Data.Char(digitToInt)
import Data.List(transpose)
import Data.Map as M(Map, fromListWith, (!))
import Data.Bifunctor(bimap)

fromBinary :: [Int] -> Int
fromBinary = foldl (\t d -> 2*t + d) 0

counts :: (Ord a) => [a] -> Map a Int
counts = M.fromListWith (+) . (flip zip) (repeat 1)

mostCommon :: String -> Char
mostCommon s = 
  let cs = counts s in
    if (cs M.! '1') >= (cs M.! '0') then '1' else '0'
  
leastCommon :: String -> Char
leastCommon s = if mostCommon s == '1' then '0' else '1'

commonFiltering :: (String -> Char) -> Int -> [String] -> String
commonFiltering _ _ (l:[]) = l
commonFiltering commonProc pos ls =
  let atPos = map (!! pos) ls
      keep = commonProc atPos in
    commonFiltering commonProc (pos + 1) (map fst $ filter ((==) keep . snd) $ zip ls atPos)
  
readBinaryInt :: String -> Int
readBinaryInt = fromBinary . map digitToInt

day03a :: [String] :~> (Int, Int)
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show . uncurry (*)
    , sSolve = Just . join bimap readBinaryInt . ((,) <$> (map mostCommon) <*> (map leastCommon)) . transpose
    }

day03b :: [String] :~> (Int, Int)
day03b = MkSol
    { sParse = Just . lines
    , sShow  = show . uncurry (*)
    , sSolve = Just . join bimap readBinaryInt . ((,) <$> (commonFiltering mostCommon 0) <*> (commonFiltering leastCommon 0))
    }
