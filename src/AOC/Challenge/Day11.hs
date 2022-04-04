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


newtype Energy = Energy Int deriving stock (Eq, Ord, Show)
type Octopii = LGridMap RectOctGrid (Energy, Bool)

parser :: String -> Maybe Octopii
parser s =
  let lls = lines s
      grid = rectOctGrid (length lls) (length (head lls))
      elts = zip (concatMap (map (Energy . digitToInt)) lls) (repeat False)
  in Just $ lazyGridMap grid elts

incrementEnergy :: (Energy, Bool) -> (Energy, Bool)
incrementEnergy (Energy e, b) = (Energy (e+1), b)

resetFlashed :: Octopii -> Octopii
resetFlashed = GM.map ((, False) . fst)

oneStep :: Octopii -> Octopii
oneStep =
  GM.map (first (\(Energy e) -> Energy (if e > 9 then 0 else e))) . oneStep' . GM.map incrementEnergy

oneStep' :: Octopii -> Octopii
oneStep' o =
  case filter ((\(Energy e, b) -> e > 9 && not b) . snd) (GM.toList o) of
    [] -> o
    us -> oneStep' $ foldr oneUpdate o us

modifyNeighbors :: (Ord (Index a), G.Grid a) => LGridMap a b -> Index a -> (b -> b) -> LGridMap a b
modifyNeighbors lg a f = foldr (GM.adjust f) lg (neighbours lg a)

oneUpdate :: (Index RectOctGrid, (Energy, Bool)) -> Octopii -> Octopii
oneUpdate (k, _) o =
  let oo = modifyNeighbors o k incrementEnergy
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
    , sSolve = findIndex (all ((==) (Energy 0) . fst . snd) . GM.toList) . iterate (resetFlashed . oneStep)
    }
