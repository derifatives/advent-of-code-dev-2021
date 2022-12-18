{-# HLINT ignore "Use head" #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day18 (
  day18a
  , day18b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Linear.V3
import qualified Data.Map as M
import Data.Map.Strict (insertWith)

type Cube = V3 Int
type Lava = S.Set Cube
type Box = (V3 Int, V3 Int)
type Found = S.Set Cube

parser :: String -> Maybe Lava
parser = Just . foldr (S.insert . parseLine) S.empty . lines
  where parseLine = (\is -> V3 (is !! 0) (is !! 1) (is !! 2)) . map read . splitOn ","

offsets :: [V3 Int]
offsets = [V3 0 0 1, V3 0 0 (-1), V3 0 1 0, V3 0 (-1) 0, V3 1 0 0, V3 (-1) 0 0]

openNextTo :: Lava -> Cube -> [Cube]
openNextTo l c = filter (not . (`S.member` l)) $ map (c +) offsets

exposedFaces :: Lava -> Int
exposedFaces l = sum $ map (length . openNextTo l) $ S.elems l

eltWise :: (Int -> Int -> Int) -> V3 Int -> V3 Int -> V3 Int
eltWise f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

boundingBox :: Lava -> Box
boundingBox lava =
  let cs = S.elems lava
  in (foldr1 (eltWise min) cs, foldr1 (eltWise max) cs)

inBox :: Box -> Cube -> Bool
inBox (V3 xl yl zl, V3 xh yh zh) (V3 x y z) =
  xl <= x && x <= xh && yl <= y && y <= yh && zl <= z && z <= zh

type Queue a = M.Map Int [a]
type Pq = Queue Cube

exteriorP :: Lava -> Box -> Found -> [Cube] -> Bool
exteriorP l b f q =
  not (null q) &&
  (let c = head q
    in
      (not (inBox b c) ||
       (let new = filter (not . (`S.member` f)) $ openNextTo l c
         in exteriorP l b (foldr S.insert f new) (new ++ tail q))))

exteriorFaces :: Lava -> Int
exteriorFaces lava =
  let box = boundingBox lava
  in sum $ map (ef1 lava box) $ S.elems lava
  where
    ef1 l b c =
      length $ filter (True ==) $ map (exteriorP l b S.empty. (: [])) (openNextTo l c)

day18a :: Lava :~> Int
day18a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . exposedFaces
    }

day18b :: Lava :~>  Int
day18b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . exteriorFaces
    }
