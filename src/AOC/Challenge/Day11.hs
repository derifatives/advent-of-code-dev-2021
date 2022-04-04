-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
  day11a
  , day11b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Arrow(first, second)
import Data.Char(digitToInt)
import Data.List(findIndex)
import Math.Geometry.Grid(Index, neighbours)
import qualified Math.Geometry.GridInternal as G
import qualified Math.Geometry.GridMap as GM (adjust, map, toList)
import Math.Geometry.Grid.Octagonal(RectOctGrid, rectOctGrid)
import Math.Geometry.GridMap.Lazy (lazyGridMap, LGridMap)

type Octopii = LGridMap RectOctGrid (Int, Bool)

parser :: String -> Maybe Octopii
parser s =
  let lls = lines s
      grid = rectOctGrid (length lls) (length (head lls))
      elts = zip (concatMap (map digitToInt) lls) (repeat False)
  in Just $ lazyGridMap grid elts

resetFlashed :: Octopii -> Octopii
resetFlashed = GM.map ((, False) . fst)

oneStep :: Octopii -> Octopii
oneStep =
  GM.map (first (\i -> if i > 9 then 0 else i)) . oneStep' . GM.map (first (1+))

oneStep' :: Octopii -> Octopii
oneStep' o =
  case filter ((\(i, b) -> i > 9 && not b) . snd) (GM.toList o) of
    [] -> o
    us -> oneStep $ foldr oneUpdate o us

modifyNeighbors :: (Ord (Index a), G.Grid a) => LGridMap a b -> Index a -> (b -> b) -> LGridMap a b
modifyNeighbors lg a f = foldr (GM.adjust f) lg (neighbours lg a)

oneUpdate :: (Index RectOctGrid, (Int, Bool)) -> Octopii -> Octopii
oneUpdate (k, _) o =
  let oo = modifyNeighbors o k (first (1+)) 
  in GM.adjust (second (const True)) k oo

oneStepAccum :: (Octopii, Int) -> (Octopii, Int)
oneStepAccum (oo, i) =
  let o = oneStep oo
      new_flashes = length $ filter (== True) $ map (snd . snd) $ GM.toList o
  in (resetFlashed o, i + new_flashes)

day11a :: Octopii :~> Int
day11a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . snd . flip (!!) 100 . iterate oneStepAccum . (, 0)
    }

day11b :: Octopii :~> Int
day11b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = findIndex (all ((==) 0 . fst . snd) . GM.toList) . iterate (resetFlashed . oneStep)
    }
