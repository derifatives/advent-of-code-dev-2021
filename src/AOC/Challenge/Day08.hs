-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
  day08a
  , day08b
  ) where

import           AOC.Prelude
import Data.List(permutations)
import Data.List.Split(splitOn)
-- import Data.Maybe(fromJust)
import Data.Sort(sort)

toTuple :: [a] -> (a, a)
toTuple [a1, a2] = (a1, a2)
toTuple _ = error "Not a 2-list"

parser :: String -> Maybe [([String], [String])]
parser = Just . map (toTuple . map (map sort . splitOn " ") . (splitOn " | ")) . lines

isUnique :: String -> Bool
isUnique s = n == 2 || n == 3 || n == 4 || n == 7
  where n = length s

segmentToDigit :: [(String, Int)]
segmentToDigit = [("abcefg", 0),
                  ("cf", 1),
                  ("acdeg", 2),
                  ("acdfg", 3),
                  ("bcdf", 4),
                  ("abdfg", 5),
                  ("abdefg", 6),
                  ("acf", 7),
                  ("abcdefg", 8),
                  ("abcdfg", 9)]

patterns :: [String]
patterns = map fst segmentToDigit

mappings :: [[(Char, Char)]]
mappings = map (flip zip $ "abcdefg") (permutations "abcdefg")

applyAssocList :: Eq a => [(a, b)] -> [a] -> [b]
applyAssocList assocs as = map applyAssoc as
  where applyAssoc a = snd $ fromJust $ find ((== a) . fst) assocs

mappingIsValid :: [(Char, Char)] -> [String] -> Bool
mappingIsValid _ [] = True
mappingIsValid assocs (a:as) =
  elem (sort (applyAssocList assocs a)) patterns && mappingIsValid assocs as

applyValidMapping :: [(Char, Char)] -> [String] -> [Int]
applyValidMapping mapping ws =
  let translated = map (sort . applyAssocList mapping) ws
  in applyAssocList segmentToDigit translated

getValidMapping :: [String] -> [(Char, Char)]
getValidMapping ws = fromJust $ find (\m -> mappingIsValid m ws) mappings

assembleBase10 :: [Int] -> Int
assembleBase10 = foldl (\s d -> 10*s + d) 0

subOne :: ([String], [String]) -> Int
subOne (ms, ds) = assembleBase10 $ applyValidMapping (getValidMapping ms) ds

solveB :: [([String], [String])] -> Int
solveB = sum . map subOne
  
day08a :: [([String], [String])] :~> Int
day08a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map (length . (filter isUnique) . snd)
    }

day08b :: [([String], [String])] :~> Int
day08b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solveB 
    }
