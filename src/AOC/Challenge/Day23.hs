{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fobject-code -O #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
  day23a
  , input
  , findCheapestPath
  , allMoves
  , parser
  , parserB
  , Q
  , columnDepth
  , constructFinishedBurrow
  , columnVal
  , columnUp
  , columnDown
  , Piece (..)
  , columnHasWrongPiece
  , columnDeparturePoint
  , columnArrivalPoint
  , toHallMoves
  , toColumnMoves
  , posToColumnMove
  , Burrow (..)
  , day23b
  ) where

import           AOC.Prelude
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import Linear.V2

data Piece = A | B | C | D deriving stock (Eq, Show)

type V2i = V2 Int

data Burrow = Burrow {
  pos :: Set V2i,
  arr :: A.Array V2i (Maybe Piece)
  }

upperBound :: Burrow -> V2 Int
upperBound (Burrow _ a) = snd (A.bounds a)

numRows :: Burrow -> Int
numRows b = let V2 mr _ = upperBound b in mr + 1

numCols :: Burrow -> Int
numCols b = let V2 _ mc = upperBound b in mc + 1

instance Show Burrow where
  show b@(Burrow ps a) =
    let (V2 mr mc) = upperBound b
        showBurrowLine r = map (\c -> getPos (V2 r c)) [0..mc]
        getPos p =
            if S.member p ps
            then case a A.! p of
              Just A -> 'A'
              Just B -> 'B'
              Just C -> 'C'
              Just D -> 'D'
              Nothing -> '.'
            else '#'
    in "\n" ++ unlines (map showBurrowLine [0..mr]) ++ "\n"

instance Eq Burrow where
  (Burrow p1 a1) == (Burrow p2 a2) =
    p1 == p2 && all (\p -> a1 A.! p == a2 A.! p) p1


parser :: String -> Maybe Burrow
parser s =
  let lls = lines s
      nr = length lls
      nc = length (head lls)
      linesWithRows = zip (lines s) [0..]
      lineToAssoc (l, r) =
        map (\(ch, c) -> ((V2 r c), (charToPiece ch))) . filter (\(ch, _) -> elem ch "ABCD.") $ zip l [0..]
      assocs = concat $ map lineToAssoc linesWithRows
  in Just $ Burrow {
    pos = S.fromList (map fst assocs),
    arr = A.array (V2 0 0, V2 (nr-1) (nc-1)) assocs
    }

parserB :: String -> Maybe Burrow
parserB s =
  let (start, rest) = splitAt 3 (lines s)
  in parser $ unlines $ start ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ rest
     
charToPiece :: Char -> Maybe Piece
charToPiece 'A' = Just A
charToPiece 'B' = Just B
charToPiece 'C' = Just C
charToPiece 'D' = Just D
charToPiece _ = Nothing

goalColumn :: Piece -> Int
goalColumn A = 3
goalColumn B = 5
goalColumn C = 7
goalColumn D = 9

columnToPiece :: Int -> Piece
columnToPiece 3 = A
columnToPiece 5 = B
columnToPiece 7 = C
columnToPiece 9 = D
columnToPiece _ = undefined

goalColumns :: [Int]
goalColumns = map goalColumn [A, B, C, D]

hallwayRow, hallwayStart, hallwayEnd :: Int
hallwayRow = 1
hallwayStart = 1
hallwayEnd = 11

costPerStep :: Piece -> Int
costPerStep A = 1
costPerStep B = 10
costPerStep C = 100
costPerStep D = 1000

columnDepth :: Burrow -> Int
columnDepth b = numRows b - 3

burrowVal :: Burrow -> V2 Int -> Maybe Piece
burrowVal (Burrow _ a) p = a A.! p

columnVal :: Burrow -> Piece -> Int -> Maybe Piece
columnVal b piece depth =
  burrowVal b (V2 (depth + 2) (goalColumn piece))

columnDown :: Burrow -> Piece -> [(Int, Maybe Piece)]
columnDown b p = map (\i -> (i, columnVal b p i)) [0..(columnDepth b - 1)]

columnUp :: Burrow -> Piece -> [(Int, Maybe Piece)]
columnUp b p =
  let d = columnDepth b 
  in map (\i -> (i, columnVal b p i)) [d-1, (d-2)..0]

constructFinishedBurrow :: Burrow -> Burrow
constructFinishedBurrow (Burrow pos arr) =
  Burrow pos (A.array (A.bounds arr) (map f (S.toList pos)))
  where f p@(V2 r c) =
          (p, if r == hallwayRow then Nothing else Just $ columnToPiece c)

columnHasWrongPiece :: Burrow -> Piece -> Bool
columnHasWrongPiece b p =
  any (\(_, v) -> not (elem v [Just p, Nothing])) (columnDown b p)

columnDeparturePoint :: Burrow -> Piece -> Maybe (V2 Int)
columnDeparturePoint b p =
  if columnHasWrongPiece b p
  then let d = (fst $ fromJust $ find (isJust . snd) (columnDown b p))
       in Just $ V2 (d + 2) (goalColumn p)
  else Nothing

columnArrivalPoint :: Burrow -> Piece -> Maybe (V2 Int)
columnArrivalPoint b p =
  if columnHasWrongPiece b p
  then Nothing
  else
    let maybeSpot = find (isNothing . snd) (columnUp b p)
    in case maybeSpot of
      Nothing -> Nothing
      Just (i, Nothing) -> Just (V2 (i+2) (goalColumn p))
      _ -> undefined

toHallMoves :: Burrow -> [(Burrow, Int)]
toHallMoves b = concat $ map (fromColumnToHallMoves b) [A, B, C, D]

fromColumnToHallMoves :: Burrow -> Piece -> [(Burrow, Int)]
fromColumnToHallMoves b p =
  if not $ columnHasWrongPiece b p
  then []
  else
    let startPos = columnDeparturePoint b p
    in case startPos of
         Nothing -> []
         Just s@(V2 _ c) ->
           let endPoses = openHallwayLandingPositions b c
           in map (makeMove b s) endPoses

openHallwayLandingPositions :: Burrow -> Int -> [V2 Int]
openHallwayLandingPositions (Burrow ps arr) ix =
  concat [f (ix+1) 1, f (ix-1) (-1)]
  where
    f i o =
      let newPos = (V2 hallwayRow i)
      in if S.member newPos ps && isNothing (arr A.! newPos)
         then if elem i goalColumns
              then f (i+o) o
              else newPos : f (i+o) o
         else []

toColumnMoves :: Burrow -> [(Burrow, Int)]
toColumnMoves b@(Burrow pos _) =
  map fromJust $ filter isJust $ map (posToColumnMove b) (S.toList pos)

allMoves :: Burrow -> [(Burrow, Int)]
allMoves b = concat [toHallMoves b, toColumnMoves b]

posToColumnMove :: Burrow -> V2 Int -> Maybe (Burrow, Int)
posToColumnMove b@(Burrow _ arr) pos@(V2 r c) =
  case arr A.! pos of
    Nothing -> Nothing
    (Just p) ->
      if (c == goalColumn p ||
          (r /= hallwayRow && Just pos /= columnDeparturePoint b (columnToPiece c)))
      then Nothing
      else
        case columnArrivalPoint b p of
          Nothing -> Nothing
          Just dest@(V2 _ dc) ->
            if any (\i -> isJust (arr A.! (V2 hallwayRow i))) $ filter ((/=) c) [(min c dc)..(max c dc)]
            then Nothing
            else Just $ makeMove b pos dest

moveCost :: V2 Int -> V2 Int -> Piece -> Int
moveCost (V2 r1 c1) (V2 r2 c2) p =
  (sum $ map abs [r1 - hallwayRow, r2 - hallwayRow, c1 - c2]) * (costPerStep p)

makeMove :: Burrow -> V2 Int -> V2 Int -> (Burrow, Int)
makeMove (Burrow pos arr) start end =
  let p = fromJust $ arr A.! start
  in (Burrow pos (arr A.// [(start, Nothing), (end, Just p)]),
      moveCost start end p)

findCheapestPath :: Burrow -> Int
findCheapestPath b =
  findInternal (MM.insert 0 b MM.empty) (M.singleton (show b) 0) (constructFinishedBurrow b)

type Q = MM.MultiMap Int Burrow
type CostMap = M.Map String Int

findInternal :: Q -> CostMap -> Burrow -> Int
findInternal q cm goal =
  case MM.findMinWithValues q of
    Nothing -> maxBound :: Int
    Just (cost, (b:bs)) ->
      if b == goal
      then cost
      else 
        let qMinusB = MM.delete cost q
            qWithBs = foldr (\b' q' -> MM.insert cost b' q') qMinusB bs
            (newQ, newCM) = foldr (maybeUpdateQueueAndCost cost) (qWithBs, cm) (allMoves b)
        in findInternal newQ newCM goal
    _ -> undefined

maybeUpdateQueueAndCost :: Int ->  (Burrow, Int) -> (Q, CostMap) -> (Q, CostMap)
maybeUpdateQueueAndCost cost (b, costOfMove) (q, cm) =
  let newCost = cost + costOfMove in
    case M.lookup (show b) cm of
      Nothing -> (MM.insert newCost b q, M.insert (show b) newCost cm)
      Just foundCost -> if newCost < foundCost
                        then (MM.insert newCost b q, M.insert (show b) newCost cm)
                        else (q, cm)
     
day23a :: _ :~> _
day23a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . findCheapestPath
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = parserB
    , sShow  = show
    , sSolve = Just . findCheapestPath
    }

input :: String
input = "#############\n#...........#\n###D#C#A#B###\n  #D#C#B#A#\n  #########"
