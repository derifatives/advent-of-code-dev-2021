-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
  day02a
  , day02b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)

data Shape = Rock | Paper | Scissors deriving stock (Eq)
data Strategy = X | Y | Z
data Outcome = Win | Tie | Lose

parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'B' = Paper
parseShape 'C' = Scissors
parseShape _ = undefined

parseStrategy :: Char -> Strategy
parseStrategy 'X' = X
parseStrategy 'Y' = Y
parseStrategy 'Z' = Z
parseStrategy _ = undefined

symbolToShape :: Strategy -> Shape
symbolToShape X = Rock
symbolToShape Y = Paper
symbolToShape Z = Scissors

symbolToOutcome :: Strategy -> Outcome
symbolToOutcome X = Lose
symbolToOutcome Y = Tie
symbolToOutcome Z = Win

winsAgainst :: Shape -> Shape
winsAgainst Paper = Scissors
winsAgainst Scissors = Rock
winsAgainst Rock = Paper

parser :: String -> Maybe [(Shape, Strategy)]
parser = Just . map parseLine . lines
  where parseLine l = (parseShape (l !! 0), parseStrategy (l !! 2))

outcome :: Shape -> Shape -> Outcome
outcome opponentShape playerShape =
  if opponentShape == playerShape then Tie
  else if winsAgainst opponentShape == playerShape then Win
  else Lose

shapeForOutcome :: Shape -> Outcome -> Shape
shapeForOutcome opponentShape Win = winsAgainst opponentShape
shapeForOutcome opponentShape Tie = opponentShape
shapeForOutcome opponentShape Lose = winsAgainst (winsAgainst opponentShape)

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Tie = 3
outcomeScore Lose = 0

gameScore :: Shape -> Shape -> Int
gameScore opponentShape playerShape =
  outcomeScore (outcome opponentShape playerShape) + shapeScore playerShape
  
day02a :: [(Shape, Strategy)] :~> Int
day02a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map (\(sh, sy) -> gameScore sh (symbolToShape sy))
    }

day02b :: [(Shape, Strategy)] :~> Int
day02b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map \(sh, sy) -> gameScore sh (shapeForOutcome sh (symbolToOutcome sy))
    }
