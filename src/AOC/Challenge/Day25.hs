{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day25 (
  day25a
  -- , day25b
  , testInput2
  , parser
  , step
  , numSteps
  , updateBoard
  , Cucumber(..)
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import qualified Data.Map as M

type Map = M.Map (Int, Int) Cucumber
data Cucumber = South | East deriving stock (Eq, Show)
data Board = Board {
  m :: Map,
  nr :: Int,
  nc :: Int } deriving stock (Eq, Show)

parser :: String -> Maybe Board
parser s =
  let lls = lines s
      nr = length lls
      nc = length (head lls)
      m = foldr parseLine  M.empty (zip lls [0..])
  in Just Board { m=m, nr=nr, nc=nc }

parseLine :: (String, Int) -> Map -> Map
parseLine (line, r) m =
  foldr maybeInsertCucumber m (zip line [0..])
  where
    maybeInsertCucumber ('.', _) m' = m'
    maybeInsertCucumber ('>', c) m' = M.insert (r, c) East m'
    maybeInsertCucumber ('v', c) m' = M.insert (r, c) South m'
    maybeInsertCucumber _ _ = error "Unexpected insert."

testInput :: String
testInput = "...>...\n.......\n......>\nv.....>\n......>\n.......\n..vvv.."

testInput2 :: String
testInput2 = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>\n"

updateBoard :: Board -> Cucumber -> Board
updateBoard b@Board{m=m, nr=nr, nc=nc} cucumber = b{ m=M.foldrWithKey inserter M.empty m }
  where
    inserter (row, col) c m' =
      if c == cucumber then
        let newpos = getNewPos (row, col) c
        in if M.member newpos m
           then M.insert (row, col) c m'
           else M.insert newpos c m'
      else M.insert (row, col) c m'
    getNewPos (rr, cc) East = (rr, (cc+1) `mod` nc)
    getNewPos (rr, cc) South = ((rr+1) `mod` nr, cc)

step :: Board -> Board
step b = updateBoard (updateBoard b East) South

numSteps :: Board -> Int -> Int
numSteps b i =
  let b' = step b
      in if b' == b then i+1 else numSteps b' (i+1)

day25a :: _ :~> _
day25a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . (flip numSteps) 0
    }

day25b :: _ :~> _
day25b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
