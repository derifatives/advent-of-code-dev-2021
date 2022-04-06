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
import Control.Monad.State(State, evalState, execState, get, gets, modify, put, replicateM)
import Control.Monad.Loops(untilM)
import Data.Char(digitToInt)
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

resetEnergy :: Octopii -> Octopii
resetEnergy = GM.map (first (\(Energy e) -> Energy (if e > 9 then 0 else e)))

countFlashes :: Octopii -> Int
countFlashes = length . filter (== True) . map (snd . snd) . GM.toList

modifyNeighbors :: (Ord (Index a), G.Grid a) => (b->b) -> Index a -> State (LGridMap a b) ()
modifyNeighbors f a = modify $ \g ->  foldr (GM.adjust f) g (neighbours g a)

oneStep :: State Octopii Int
oneStep = do
  modify (GM.map incrementEnergy)
  flashNeighborsRepeatedly
  num_flashes <- gets countFlashes
  modify resetFlashed
  modify resetEnergy
  pure num_flashes

flashNeighborsRepeatedly :: State Octopii ()
flashNeighborsRepeatedly = do
    o <- get
    case filter ((\(Energy e, b) -> e > 9 && not b) . snd) (GM.toList o) of
      [] -> pure ()
      us -> do
        put $ execState (mapM oneUpdate us) o
        flashNeighborsRepeatedly

oneUpdate :: (Index RectOctGrid, (Energy, Bool)) -> State Octopii ()
oneUpdate (k, _) = do
  modifyNeighbors incrementEnergy k
  modify (GM.adjust (second (const True)) k)

allFlashedP :: State Octopii Bool
allFlashedP = gets (all ((==) (Energy 0) . fst . snd) . GM.toList)

day11a :: Octopii :~> Int
day11a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . evalState (replicateM 100 oneStep)
    }

day11b :: Octopii :~> Int
day11b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . length . evalState (untilM oneStep allFlashedP)
    }
