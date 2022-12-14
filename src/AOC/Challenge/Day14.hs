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
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Linear.V2
import qualified Data.Map as M

data D = R | S deriving stock (Show)
type Pos = V2 Int
type Cave = M.Map Pos D

parser :: String -> Maybe Cave
parser = Just . foldr parseLine M.empty . lines

toPos :: String -> Pos
toPos s = let (x, y) = span (/= ',') s in V2 (read x) (read (tail y))

insertSegment :: (Pos, Pos) -> Cave -> Cave
insertSegment (V2 x1 y1, V2 x2 y2) c
  | x1 == x2 = foldr (flip M.insert R . V2 x1) c [(min y1 y2)..(max y1 y2)] 
  | y1 == y2 = foldr (flip M.insert R . flip V2 y1) c [(min x1 x2)..(max x1 x2)] 
  | otherwise = undefined

parseLine :: String -> Cave -> Cave
parseLine l c =
  let endpoints = map toPos $ splitOn "->" l
  in foldr insertSegment c $ zip endpoints (tail endpoints)

-- TODO: Use lens?
getY :: Pos -> Int
getY (V2 _ y) = y

maxDepth :: Cave -> Int
maxDepth = foldr (max . getY) 0 . M.keys

start :: Pos
start = V2 500 0

offsets :: [Pos]
offsets = [V2 0 1, V2 (-1) 1, V2 1 1]

toLower :: Cave -> Pos -> Maybe Pos
toLower c p =
  let ps = filter (isNothing . flip M.lookup c) (map (p +) offsets)
  in if null ps then Nothing else Just $ head ps

sandPosition :: Cave -> Int -> Pos -> Maybe Pos
sandPosition c d p@(V2 _ y) =
  if y >= d
  then Nothing
  else let lower = toLower c p
       in maybe (Just p) (sandPosition c d) lower

fill :: Cave -> (Cave, Int)
fill cave = fill' cave (maxDepth cave) 0
  where fill' c d f = 
          let p = sandPosition c d start
          in if isJust p
             then fill' (M.insert (fromJust p) S c) d (f+1)
             else (c, f)

sandPosition2 :: Cave -> Int -> Pos -> Pos
sandPosition2 c d p@(V2 _ y) =
  if y >= d + 1
  then p
  else let lower = toLower c p
       in maybe p (sandPosition2 c d) lower

fill2 :: Cave -> (Cave, Int)
fill2 cave = fill' cave (maxDepth cave) 0
  where fill' c d f = 
          let p = sandPosition2 c d start
          in if p /= start
             then fill' (M.insert p S c) d (f+1)
             else (M.insert start S c, f+1)
        
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
