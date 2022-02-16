{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
  day04a
  , day04b
  ) where

import AOC.Prelude
import Data.Functor
import Data.List(find, partition)
import Data.List.Split(chunksOf, splitOn)
import Data.Array(Array, listArray, assocs, elems, (//), (!))
import Data.Maybe(isJust)

type Card = Array (Int, Int) (Maybe Int)

parser :: String -> Maybe ([Int], [Card])
parser input =
  let lls = lines input
      numbers = map read (splitOn "," (head lls))
      cards = map (parseCard . tail) (chunksOf 6 (tail lls))
  in Just (numbers, cards)

parseCard :: [String] -> Card
parseCard lls = listArray ((0, 0), (4, 4)) (map (Just . read) (concat (map words lls)))

markCard :: Int -> Card -> Card
markCard n card =
  fmap (\m -> if (m == (Just n)) then Nothing else m) card

adder :: Maybe Int -> Int -> Int
adder (Just i) n = i + n
adder Nothing n = n

cardSum :: Card -> Int
cardSum c = foldr adder 0 (elems c)

cardFinished :: Card -> Bool
cardFinished card =
  any (\(s, o) -> sequenceMarked s o 5 card)
  [((0, 0), (0, 1)),
   ((1, 0), (0, 1)),
   ((2, 0), (0, 1)),
   ((3, 0), (0, 1)),
   ((4, 0), (0, 1)),
   ((0, 0), (1, 0)),
   ((0, 1), (1, 0)),
   ((0, 2), (1, 0)),
   ((0, 3), (1, 0)),
   ((0, 4), (1, 0))]
  
sequenceMarked :: (Int, Int) -> (Int, Int) -> Int -> Card -> Bool
sequenceMarked _ _ 0 _ = True
sequenceMarked (cr, cc) (ofr, ofc) n c =
  if isJust (c ! (cr, cc)) then False else
    sequenceMarked (cr + ofr, cc + ofc) (ofr, ofc) (n - 1) c

playGame :: [Int] -> [Card] -> (Int, Card)
playGame (n:ns) cards =
  let marked = map (markCard n) cards
      winner = find cardFinished marked
  in case winner of
    Just c  -> (n, c)
    Nothing -> playGame ns marked
playGame _ _ = error "Unexpected empty list."

playToLose :: [Int] -> [Card] -> (Int, Card)
playToLose (n:ns) cards =
  let marked = map (markCard n) cards
      (done, undone) = partition cardFinished marked
  in case undone of
    [] -> (n, head done)
    ss -> playToLose ns ss
playToLose _ _ = error "Unexpected empty list."

score :: (Int, Card) -> Maybe Int
score (n, c) = Just (n * cardSum c)

day04a :: ([Int], [Card]) :~> Int
day04a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = score . (uncurry playGame)
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = score . (uncurry playToLose)
    }
