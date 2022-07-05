-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
  day15a
  , day15b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Char(digitToInt)
import qualified Data.Array.Unboxed as A
import qualified Data.Set as S

import qualified Math.Geometry.Grid as G
import Math.Geometry.Grid.Square(RectSquareGrid, rectSquareGrid)
-- The Grid package seems to use an unusual / surprising layout convention.
-- In particular, rectSquareGrid is documented to take "rows" and "columns"
-- as arguments, but then when indexing it seems to have the column as the first
-- entry of the index?

type Ix = G.Index RectSquareGrid

data SearchState = SearchState {
   -- (Min Cost, Actual Cost, Index)
   active :: S.Set (Int, Int, Ix),
   used :: S.Set Ix } deriving stock (Show)

class Cave c where
  cost :: c -> Ix -> Int
  neighbours :: c -> Ix -> [Ix]
  size :: c -> (Int, Int)

data SmallCavern = SmallCavern {
  grid :: RectSquareGrid,
  array :: A.Array Ix Int } deriving stock (Show)

instance Cave SmallCavern where
  cost SmallCavern {array=a} ix = a A.! ix
  size SmallCavern {grid=g} = G.size g
  neighbours SmallCavern {grid=g} i = G.neighbours g i

data LargeCavern = LargeCavern {
  baseSize :: (Int, Int),
  fullGrid :: RectSquareGrid,
  baseArray :: A.Array Ix Int } deriving stock (Show)

instance Cave LargeCavern where
  size LargeCavern {fullGrid=g} = G.size g
  neighbours LargeCavern {fullGrid=g} i = G.neighbours g i
  cost LargeCavern {baseSize=(rb, cb), baseArray=a} (r, c) =
    let (rq, rr) = r `quotRem` rb
        (cq, cr) = c `quotRem` cb
    in ((((a A.! (rr, cr)) - 1) + rq + cq) `mod` 9) + 1
      
buildInitialState :: SearchState
buildInitialState = SearchState { active = S.singleton (0, 0, (0, 0)),
                                  used = S.empty }

parser :: String -> Maybe (SmallCavern, SearchState)
parser s =
  let lls = lines s
      rows = length lls
      columns = length (head lls)
      grid = rectSquareGrid columns rows
      elts = [ digitToInt c | l <- lls, c <- l ]
  in Just $ (SmallCavern { grid=grid,
                           array=A.listArray ((0, 0), (rows-1, columns-1)) elts },
             buildInitialState)

findAndRemoveMinCostNode :: SearchState -> ((Int, Int, Ix), SearchState)
findAndRemoveMinCostNode SearchState {active=a, used=u} =
  let v@(minCost, currentCost, ix) = S.findMin a
  in ((minCost, currentCost, ix), SearchState { active=S.delete v a, used=S.insert ix u })
            
exploreFromNode :: (Cave cv) => cv -> Int-> SearchState -> Ix -> SearchState
exploreFromNode cave currentCost ss ix = foldr f ss (neighbours cave ix)
  where f ix'@(r, c) ss'@SearchState {active=a, used=u} =
          if S.member ix' u
          then ss'
          else let new_cost = currentCost + AOC.Challenge.Day15.cost cave ix'
               in ss' { active=S.insert (new_cost-r-c, new_cost, ix') a }

minDistance :: (Cave c) => (c, SearchState) -> Int
minDistance (cave, ss) =
  let ((_, currentCost, ix), new_ss) = findAndRemoveMinCostNode ss
      (rows, columns) = size cave
  in if ix == (columns-1, rows-1)
     then currentCost
     else minDistance (cave, exploreFromNode cave currentCost new_ss ix)

expandCavern :: (SmallCavern, SearchState) -> (LargeCavern, SearchState)
expandCavern (SmallCavern {grid=g, array=a}, ss) =
  let (r, c) = G.size g
      e = 5
  in (LargeCavern { baseSize = (r, c),
                    fullGrid = rectSquareGrid (r*e) (c*e),
                    baseArray = a },
      ss)

day15a :: (SmallCavern, SearchState) :~> Int
day15a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minDistance
    }

day15b :: (SmallCavern, SearchState) :~> Int
day15b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minDistance . expandCavern
    }
