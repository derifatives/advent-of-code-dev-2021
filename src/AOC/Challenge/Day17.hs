-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day17 (
  day17a
  , day17b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Foldable (foldl')
import Data.List (nub)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Set as S
import Linear.V2

rocksString :: String
rocksString = "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##\n"

type Pos = V2 Int
type Rock = S.Set Pos
type Wind = String

parseRocks :: [Rock]
parseRocks = map parseRock $ splitOn [""] $ lines rocksString

parseRock :: [String] -> Rock
parseRock ls = foldl' addLine S.empty $ zip [0..] (reverse ls)
  where
    addLine r (row, rowLine) =
      -- Start with rock at position 2.
      foldl' (\r' (col, ch) -> if ch == '#' then S.insert (V2 row $ col + 2) r' else r') r $ zip [0..] rowLine

data Cave = Cave {
  rocks :: S.Set Pos,
  height :: Int
  } deriving stock (Show)

initCave :: Cave
initCave = Cave {
  rocks = foldl (\c i -> S.insert (V2 0 i) c) S.empty [0..6],
  height = 0
  }

initRock :: Rock -> Cave -> Rock
initRock r c = S.map (V2 (height c + 4) 0 +) r

valid :: Rock -> Cave -> Bool
valid r c = S.null $ S.intersection r (rocks c)

applyWind :: Rock -> Char -> Rock
applyWind r ch =
  let offset = if ch == '<' then V2 0 (-1) else V2 0 1
      r' = S.map (offset +) r
  in if any (\(V2 _ c) -> c < 0 || c > 6) r' then r else r'

applyDrop :: Rock -> Rock
applyDrop = S.map (V2 (-1) 0 +)

getRow :: V2 Int -> Int
getRow (V2 r _) = r

placeRock :: Rock -> Wind -> Cave -> (Cave, Wind)
placeRock r w c =
  let wr = applyWind r (head w)
      r' = if valid wr c then wr else r
      dr = applyDrop r'
  in if valid dr c
     then placeRock dr (tail w) c
     else (Cave {rocks=S.union r' (rocks c), height=max (height c) $ maximum $ S.map getRow r'}, tail w)

fallingRocks :: Wind -> [Cave]
fallingRocks wind =
  fallingRocks' (cycle parseRocks) (cycle wind) initCave
  where
    fallingRocks' rs w c =
      let (c', w') = placeRock (initRock (head rs) c) w c
      in c' : fallingRocks' (tail rs) w' c'

findDiffCycles :: [Int] -> Int -> Int -> [Int]
findDiffCycles is minC maxC
  | minC >= maxC = []
  | otherwise =
      let chunks = take 2  $ chunksOf minC is
          cycleP = length (nub (zipWith (-) (chunks !! 0) (chunks !! 1))) == 1
          rest = findDiffCycles is (minC + 1) maxC
      in if cycleP then minC : rest else rest

heightAfterManyRocks :: Int -> Int -> Int -> Int -> Int -> Wind -> Int
heightAfterManyRocks numRocksDirect burnin minCycle maxCycle numRocks wind =
  let heights = map height $ take numRocksDirect $ fallingRocks wind
      cycleLength = head $ findDiffCycles (drop burnin heights) minCycle maxCycle
      cycleHeight = heights !! (burnin + cycleLength - 1) - heights !! (burnin - 1)
      numCycles = (numRocks - burnin) `div` cycleLength
      extra = numRocks - burnin - numCycles * cycleLength
  in  numCycles * cycleHeight + heights !! (burnin + extra - 1)

day17a :: String :~> Int
day17a = MkSol
    { sParse = Just . cycle
    , sShow  = show
    , sSolve = Just . height . last . take 2022 . fallingRocks
    }

day17b :: String :~> Int
day17b = MkSol
    { sParse = Just . cycle
    , sShow  = show
    , sSolve = Just . heightAfterManyRocks 10000 2500 5 2000 1000000000000
    }
