-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day12 (
  day12a
  , day12b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Char(isLower, isUpper)
import Data.List(foldl', nub)
import Data.List.Split(splitOn)
import Data.Map (Map, empty, insertWith, lookup)
import Data.Maybe(fromJust)
import Data.Set (Set, singleton, union, toList)

newtype Graph = Graph (Map String (Set String)) deriving stock (Show)
type Path = [String]

parser :: String -> Maybe Graph
parser s = Just $ foldl' addLink (Graph Data.Map.empty) (map (splitOn "-") (lines s))
  where addLink (Graph m) names =
          let n1 = names !! 0
              n2 = names !! 1
          in Graph (insertWith union n2 (singleton n1)
                    (insertWith union n1 (singleton n2) m))

allPaths :: (Path -> Bool) -> (Path -> Bool) -> (Path -> [Path]) -> [Path] -> [Path]
allPaths validP doneP extender queue = allPaths' queue []
  where allPaths' [] found = found
        allPaths' (p:ps) found
          | doneP p = allPaths' ps (p : found)
          | validP p = allPaths' (extender p ++ ps) found
          | otherwise = allPaths' ps found

newestNotSmallDupe :: Path -> Bool
newestNotSmallDupe [] = True  -- Unused.
newestNotSmallDupe (p:ps) =  isUpper (head p) || p `notElem` ps

noSmallDupes :: Path -> Bool
noSmallDupes l =
  let l' = filter (isLower . head) l
  in l' == nub l'

atMostOneSmallDupe :: Path -> Bool
atMostOneSmallDupe l = noSmallDupes (tail l) || newestNotSmallDupe l

pathIsDone :: Path -> Bool
pathIsDone path = head path == "end"

pathExtensions :: Graph -> Path -> [Path]
pathExtensions _ [] = []
pathExtensions (Graph m) l@(p:_) = map (: l) (filter (/= "start") (toList (fromJust (Data.Map.lookup p m))))

day12a :: Graph :~> Int
day12a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \g -> Just $ length $ allPaths newestNotSmallDupe pathIsDone (pathExtensions g) [["start"]]
    }

day12b :: Graph :~> Int
day12b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \g -> Just $ length $ allPaths atMostOneSmallDupe pathIsDone (pathExtensions g) [["start"]]
    }
