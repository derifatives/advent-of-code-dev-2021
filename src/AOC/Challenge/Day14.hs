-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day14 (
  day14a
  , day14b
  , testInput
  , parser
  , sandPosition
  , fill
  , maybeLower
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)
import Linear.V2
import qualified Data.Set as S

type Pos = V2 Int
type Cave = S.Set Pos

parser :: String -> Maybe Cave
parser = Just . foldr parseLine S.empty . lines

toPos :: String -> Pos
toPos s = let (x, y) = span (/= ',') s in V2 (read x) (read (tail y))

insertSegment :: (Pos, Pos) -> Cave -> Cave
insertSegment (V2 x1 y1, V2 x2 y2) c
  | x1 == x2 = foldr (S.insert . V2 x1) c [(min y1 y2)..(max y1 y2)] 
  | y1 == y2 = foldr (S.insert . flip V2 y1) c [(min x1 x2)..(max x1 x2)] 
  | otherwise = undefined

parseLine :: String -> Cave -> Cave
parseLine l c =
  let endpoints = map toPos $ splitOn "->" l
  in foldr insertSegment c $ zip endpoints (tail endpoints)

-- TODO: Use lens?
getY :: Pos -> Int
getY (V2 _ y) = y

maxDepth :: Cave -> Int
maxDepth = foldr (max . getY) 0 . S.toList

start :: Pos
start = V2 500 0

offsets :: [Pos]
offsets = [V2 0 1, V2 (-1) 1, V2 1 1]

maybeLower :: Cave -> Pos -> Pos
maybeLower c p =
  let ps = filter (not . (`S.member` c)) (map (p +) offsets)
  in if null ps then p else head ps

sandPosition :: Cave -> Int -> Pos -> Pos
sandPosition c d p@(V2 _ y) =
  if y > d
  then p
  else let lower = maybeLower c p
       in if lower == p then p else sandPosition c d lower

fill :: Cave -> (Cave, Int)
fill cave = fill' cave (maxDepth cave) 0
  where fill' c d f = 
          let p@(V2 _ y) = sandPosition c d start
          in if y < d
             then fill' (S.insert p c) d (f+1)
             else (c, f)

fill2 :: Cave -> (Cave, Int)
fill2 cave = fill' cave (maxDepth cave) 0
  where fill' c d f = 
          let p = sandPosition c d start
          in if p /= start
             then fill' (S.insert p c) d (f+1)
             else (S.insert start c, f+1)
        
day14a :: Cave :~> Int
day14a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . snd . fill
    }

day14b :: Cave :~> Int
day14b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . snd . fill2
    }

testInput :: String
testInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
