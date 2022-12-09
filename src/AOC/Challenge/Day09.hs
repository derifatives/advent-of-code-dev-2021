-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
  day09a
  , day09b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Foldable (foldl')
import qualified Data.Set as S
import Linear.V2

type Pos = V2 Int
type Offset = V2 Int
type Visited = S.Set Pos

-- data Rope = Rope Pos Pos deriving stock (Show)
type Rope = [Pos]

wordToOffset :: String -> Offset
wordToOffset "R" = V2 1 0
wordToOffset "U" = V2 0 1
wordToOffset "L" = V2 (-1) 0
wordToOffset "D" = V2 0 (-1)
wordToOffset _ = undefined

parser :: String -> Maybe [(Offset, Int)]
parser = Just . map parseLine . lines
  where
    parseLine l = let ws = words l
      in (wordToOffset $ ws !! 0, read $ ws !! 1)

moveSegment :: Pos -> Pos -> Pos
moveSegment (V2 hx hy) tl@(V2 tx ty) =
  case max (abs (hx - tx)) (abs (hy - ty)) of
    0 -> tl
    1 -> tl
    2 -> V2 (tx + signum (hx - tx)) (ty + signum (hy - ty))
    _ -> undefined
    
moveTail :: Rope -> Rope -> Rope
moveTail [] newReversed = reverse newReversed
moveTail (t:ts) newReversed =
  moveTail ts (moveSegment (head newReversed) t:newReversed)

moveRope :: Rope -> Offset -> Rope
moveRope rope offset = moveTail (tail rope) [head rope + offset]

recordVisit :: (Rope, Visited) -> (Rope, Visited)
recordVisit (r, v) = (r, S.insert (last r) v)
 
runSequence :: (Rope, Visited) -> (Offset, Int) ->  (Rope, Visited)
runSequence s (_, 0) = s
runSequence (r, v) (o, i) =
  runSequence (recordVisit $ (moveRope r o, v)) (o, i-1)
  
initState :: Int -> (Rope, Visited)
initState n = (take n (repeat $ V2 0 0), S.insert (V2 0 0) S.empty)
  
day09a :: [(Offset, Int)] :~> Int
day09a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . S.size . snd . foldl' runSequence (initState 2)
    }

day09b ::  [(Offset, Int)] :~> Int
day09b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . S.size . snd . foldl' runSequence (initState 10)
    }
