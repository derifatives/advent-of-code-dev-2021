-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day12 (
  day12a
  , day12b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Char (ord)
import Data.Maybe (fromJust, isNothing)
import Linear.V2
import qualified Data.Map as M
import Data.Map.Strict (insertWith)

type Pos = V2 Int
type Grid = M.Map Pos Int
type Found = M.Map Pos Int
type Queue = M.Map Int [Pos]

safeLookup :: (Ord k) => k -> M.Map k a -> a
safeLookup k m = fromJust $ M.lookup k m

dummy :: Pos
dummy = V2 (-1) (-1)

parser :: String -> Maybe (Grid, (Pos, Pos))
parser s = Just $ foldr parseLine (M.empty, (dummy, dummy)) $ zip [0..] $ lines s 

parseLine :: (Int, String) -> (Grid, (Pos, Pos)) -> (Grid, (Pos, Pos))
parseLine (i, l) d =
  foldr (parseChar i) d (zip [0..] l)

parseChar :: Int -> (Int, Char) -> (Grid, (Pos, Pos)) -> (Grid, (Pos, Pos))
parseChar row (col, ch) (g, (s, e)) =
  let pos = V2 row col
      s' = if ch == 'S' then pos else s
      e' = if ch == 'E' then pos else e
      ch' = case ch of
        'S' -> 'a'
        'E' -> 'z'
        _ -> ch
  in (M.insert pos (ord ch') g, (s', e'))

offsets :: [Pos]
offsets = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

bfs :: Grid -> Queue -> Found -> Found
bfs g q f
  | M.null q = f
  | otherwise = 
    let ((p, s), q') = findAndRemoveMin q
        np = filter (validNextPos g f p) $ map (p +) offsets
        f' = foldr (\p' -> M.insert p' (s+1)) f np
        q'' = if null np then q' else insertWith (++) (s+1) np q'
    in bfs g q'' f'

runBfs :: (Grid, (Pos, Pos)) -> Found
runBfs (g, (_, e)) = bfs g (M.fromList [(0, [e])]) (M.fromList [(e, 0)])

findAndRemoveMin :: Queue -> ((Pos, Int), Queue)
findAndRemoveMin q =
  let (s, ps) = M.findMin q
  in case ps of
    (p:[]) -> ((p, s), M.deleteMin q)
    (p:rest) -> ((p, s), M.insert s rest q) 
    [] -> undefined

validNextPos :: Grid -> Found -> Pos -> Pos -> Bool
validNextPos g f p np =
  case M.lookup np g of
    Nothing -> False
    Just nc -> (isNothing $ M.lookup np f) && nc >= (safeLookup p g) - 1

minSteps :: (Grid, (Pos, Pos)) -> Int
minSteps i@(_, (s, _)) = safeLookup s $ runBfs i

minStepsToA :: (Grid, (Pos, Pos)) -> Int
minStepsToA i@(g, _) =
  let f = M.toList $ runBfs i
      fa = filter ((\p -> (safeLookup p g) == ord 'a') . fst) f
  in minimum $ map snd $ fa
  
day12a :: (Grid, (Pos, Pos)) :~> Int
day12a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minSteps
    }

day12b :: (Grid, (Pos, Pos)) :~> Int
day12b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minStepsToA
    }
