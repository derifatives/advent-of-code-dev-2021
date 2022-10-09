module AOC.Challenge.Day21 (
  day21a
  , day21b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import qualified Data.Map as M

data GameState = GameState {
  player1Pos :: Int, -- 0-indexed
  player2Pos :: Int, -- 0-indexed
  player1Turn :: Bool,
  player1Score :: Int,
  player2Score :: Int
} deriving stock (Eq, Ord, Show) 

data GameStateA = GameStateA {
  gameState :: GameState,
  dieState :: Int, -- 0-indexed
  numDieRolls :: Int } deriving stock (Show)

boardSize, dieFaces, rollsPerTurn, endScoreA, endScoreB :: Int
boardSize = 10
dieFaces = 100
rollsPerTurn = 3
endScoreA = 1000
endScoreB = 21

initialGameState :: GameState
initialGameState = GameState {
  player1Pos = 10 - 1, -- Copied from input, 0-indexed.
  player2Pos = 7 - 1, -- Copied from input, 0-indexed.
  player1Turn = True,
  player1Score = 0,
  player2Score = 0 }
                  
parserA :: String -> Maybe GameStateA
parserA _ = Just GameStateA {
  gameState = initialGameState, 
  dieState = 0, -- 0-indexed.
  numDieRolls = 0 }

nextNRolls :: Int -> Int -> (Int, Int)
nextNRolls dieState n =
  let total = sum $ map (\ds -> (ds `mod` dieFaces) + 1) [dieState..(dieState+n-1)]
      newState = (dieState + n) `mod` dieFaces
  in (total, newState)

updateGameA :: GameStateA -> GameStateA
updateGameA gsa =
  let gs = gameState gsa
      p1t = player1Turn gs
      p1s = player1Score gs
      p2s = player2Score gs
      pos = if p1t then player1Pos gs else player2Pos gs
      (dieTotal, newDieState) = nextNRolls (dieState gsa) rollsPerTurn
      newPos = (pos + dieTotal) `mod` boardSize
  in GameStateA {
    gameState = GameState {
        player1Pos = if p1t then newPos else player1Pos gs,
        player2Pos = if p1t then player2Pos gs else newPos,
        player1Turn = not p1t,
        player1Score = if p1t then p1s + newPos + 1 else p1s,
        player2Score = if p1t then p2s else p2s + newPos + 1 },
    dieState = newDieState,
    numDieRolls = numDieRolls gsa + rollsPerTurn }

playGameA :: GameStateA -> GameStateA
playGameA gsa =
  let nsa = updateGameA gsa
      ns = gameState nsa
  in if max (player1Score ns) (player2Score ns) >= endScoreA
  then nsa
  else playGameA nsa

gameAResult :: GameStateA -> Int
gameAResult gsa =
  let gs = gameState gsa
      lowScore = min (player1Score gs) (player2Score gs)
  in lowScore * numDieRolls gsa

data NumWins = NumWins {
  player1 :: Int,
  player2 :: Int } deriving stock (Show)

type Universes = M.Map GameState Int

data UniverseState = UniverseState {
  universes :: Universes,
  wins :: NumWins } deriving stock (Show)
  
initialUniverses :: String -> Maybe UniverseState
initialUniverses _ = Just UniverseState {
  universes = M.fromList [(initialGameState, 1)],
  wins = NumWins { player1 = 0, player2 = 0} }
  
rollCounts :: [(Int, Int)]
rollCounts = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

updateUniverses :: UniverseState -> UniverseState
updateUniverses UniverseState{universes=us, wins=ws} =
  foldr expandUniverse (UniverseState {universes=M.empty, wins=ws}) (M.assocs us)

expandUniverse :: (GameState, Int) -> UniverseState -> UniverseState
expandUniverse (gs, count) universeState =
  foldr expandOneRoll universeState rollCounts
  where
    expandOneRoll (total, multiplier) us@UniverseState{universes=unis, wins=ws} =
      let p1t = player1Turn gs
          pos = if p1t then player1Pos gs else player2Pos gs
          score = if p1t then player1Score gs else player2Score gs
          newPos = (pos + total) `mod` boardSize
          newScore = score + newPos + 1
          factor = count * multiplier
      in if newScore >= endScoreB
         then us { wins = if p1t
                          then ws { player1 = player1 ws + factor }
                          else ws { player2 = player2 ws + factor }
                 }
         else
           let ngs = if player1Turn gs
                 then gs { player1Pos = newPos,
                           player1Turn = False,
                           player1Score = newScore }
                 else gs { player2Pos = newPos,
                           player1Turn = True,
                           player2Score = newScore }
           in us { universes = M.insertWith (+) ngs factor unis }

simulate :: UniverseState -> UniverseState
simulate us =
  if M.null (universes us) then us else simulate (updateUniverses us)

maxWins :: UniverseState -> Int
maxWins UniverseState {wins = ws} = 
  max (player1 ws) (player2 ws)
  
    
day21a :: GameStateA :~> Int
day21a = MkSol
    { sParse = parserA
    , sShow  = show
    , sSolve = Just . gameAResult . playGameA
    }

day21b :: UniverseState :~> Int
day21b = MkSol
    { sParse = initialUniverses
    , sShow  = show
    , sSolve = Just . maxWins . simulate
    }
