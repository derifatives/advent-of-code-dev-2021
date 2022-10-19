module AOC.Challenge.Day22 (
  day22a
  , day22b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Monad
import Data.Either
import Data.List(foldl')
import Data.Maybe(isJust)
import qualified Data.MultiSet as MS
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Dir = On | Off deriving stock (Eq, Show)

data Range = Range {
  bottom :: Int,
  top :: Int
  } deriving stock (Eq, Ord, Show)

data Cuboid = Cuboid {
  x :: Range,
  y :: Range,
  z :: Range } deriving stock (Eq, Ord, Show)

data Instruction = Instruction Dir Cuboid deriving stock (Show)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

signedInt :: Parser Int
signedInt = L.signed sc L.decimal

parseDir :: Parser Dir
parseDir = choice
  [ On <$ string "on"
  , Off <$ string "off" ]

parseRange :: Parser Range
parseRange = do
  bottom <- signedInt <?> "valid int"
  void (string "..")
  top <- signedInt <?> "valid int"
  return Range { bottom=bottom, top=top }

parseInstruction :: Parser Instruction
parseInstruction = do
  dir <- parseDir
  void (string " x=")
  x <- parseRange
  void (string ",y=")
  y <- parseRange
  void (string ",z=")
  z <- parseRange
  return (Instruction dir (Cuboid x y z))

instructionParser :: String -> Instruction
instructionParser s = fromRight undefined (runParser parseInstruction "" s)

parser :: String -> Maybe [Instruction]
parser = Just . map instructionParser . lines

rangeSize :: Range -> Int
rangeSize (Range b t) = t - b + 1

size :: Cuboid -> Int
size (Cuboid x y z) = product $ map rangeSize [x, y, z]

rangeOverlap :: Range -> Range -> Maybe Range
rangeOverlap Range{bottom=b1, top=t1} Range{bottom=b2, top=t2} =
  let mb = max b1 b2
      lt = min t1 t2
  in if mb <= lt then Just (Range mb lt) else Nothing

rangeOverlapP :: Range -> Range -> Bool
rangeOverlapP r1 r2 = isJust $ rangeOverlap r1 r2

overlapP :: Cuboid -> Cuboid -> Bool
overlapP (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) =
  rangeOverlapP x1 x2 &&
  rangeOverlapP y1 y2 &&
  rangeOverlapP z1 z2

overlap :: Cuboid -> Cuboid -> Maybe Cuboid
overlap (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  ox <- rangeOverlap x1 x2
  oy <- rangeOverlap y1 y2
  oz <- rangeOverlap z1 z2
  return (Cuboid ox oy oz)

initialRange :: Range
initialRange = Range{bottom=(-50), top=50}

initialCuboid :: Cuboid
initialCuboid = Cuboid{x=initialRange, y=initialRange, z=initialRange}

instructionIntersectsInitial :: Instruction -> Bool
instructionIntersectsInitial (Instruction _ c) = overlapP initialCuboid c

data ReactorState = ReactorState (MS.MultiSet Cuboid) (MS.MultiSet Cuboid)

initialReactorState :: ReactorState
initialReactorState = ReactorState MS.empty MS.empty

updateReactorState :: ReactorState -> Instruction -> ReactorState
updateReactorState (ReactorState adds subtracts) (Instruction dir cuboid) =
  let intersector =  (\c -> overlap c cuboid)
      addsIntersects = MS.mapMaybe intersector adds
      subtractsIntersects = MS.mapMaybe intersector subtracts
      newAdds = MS.union adds subtractsIntersects
      newAdds' = if dir == On then (MS.insert cuboid newAdds) else newAdds
      newSubtracts = MS.union subtracts addsIntersects
  in (ReactorState newAdds' newSubtracts)

reactorSize :: ReactorState -> Int
reactorSize (ReactorState adds subtracts) =
  sum (MS.map size adds) - sum (MS.map size subtracts)
 
day22a :: _ :~> _
day22a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . reactorSize . foldl' updateReactorState initialReactorState . filter instructionIntersectsInitial
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . reactorSize . foldl' updateReactorState initialReactorState
    }
