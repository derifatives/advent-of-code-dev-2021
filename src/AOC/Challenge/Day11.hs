{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
  day11a
  , parser
  , octos
  , resetFlashed
  , oneStep
  , oneStep'
  , oneUpdate
  , oneStepAccum
  , day11b
  ) where

import AOC.Prelude
import Data.Char(digitToInt)
import Data.List(findIndex)
import Math.Geometry.Grid(Index, neighbours)
import Math.Geometry.GridMap(adjust, map, toList)
import Math.Geometry.Grid.Octagonal(RectOctGrid, rectOctGrid)
import Math.Geometry.GridMap.Lazy (lazyGridMap, LGridMap)

type Octopii = LGridMap RectOctGrid (Int, Bool)

parser :: String -> Maybe Octopii
parser s =
  let lls = lines s
      grid = rectOctGrid (length lls) (length (head lls))
      elts = zip (concat (AOC.Prelude.map (AOC.Prelude.map digitToInt) (lls))) (repeat False)
  in Just $ lazyGridMap grid elts

resetFlashed :: Octopii -> Octopii
resetFlashed = Math.Geometry.GridMap.map (flip (,) False . fst)

oneStep :: Octopii -> Octopii
oneStep =
  Math.Geometry.GridMap.map (\(i, b) -> if i > 9 then (0, b) else (i, b)) . oneStep' . Math.Geometry.GridMap.map (\(i, b) -> (i+1, b))

oneStep' :: Octopii -> Octopii
oneStep' o =
  let updates = AOC.Prelude.filter (\(_, (i, b)) -> i > 9 && b == False) (Math.Geometry.GridMap.toList o)
  in if updates == []
     then o
     else oneStep' $ AOC.Prelude.foldr oneUpdate o updates
                  
oneUpdate :: (Index RectOctGrid, (Int, Bool)) -> Octopii -> Octopii
oneUpdate (k, _) o =
  let oo = AOC.Prelude.foldr (\n o' -> adjust (first (1+)) n o') o (neighbours o k)
  in adjust (second (const True)) k oo

oneStepAccum :: (Octopii, Int) -> (Octopii, Int)
oneStepAccum (oo, i) =
  let o = oneStep oo
      new_flashes = length $ filter (== True) $ AOC.Prelude.map (snd . snd) $ Math.Geometry.GridMap.toList o
  in (resetFlashed o, i + new_flashes)

day11a :: _ :~> _
day11a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . snd . flip (!!) 100 . iterate oneStepAccum . (flip (,) 0)
    }

day11b :: _ :~> _
day11b = MkSol
    { sParse = parser
    , sShow  = show
    -- How would I have caught the fst vs. second bug with typing?
    -- , sSolve = findIndex (all ((==) 0 . fst . fst) . Math.Geometry.GridMap.toList) . iterate (resetFlashed . oneStep)
    , sSolve = findIndex (all ((==) 0 . fst . snd) . Math.Geometry.GridMap.toList) . iterate (resetFlashed . oneStep)
    }


octos :: Octopii
octos = fromJust $ parser "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
