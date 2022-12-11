-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
  day11a
  , day11b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Sort (sortBy)
import qualified Data.Map as M

data OldOrInt = O | I Int deriving stock (Show)
data Op = Plus | Times deriving stock (Show)
data Update = Update OldOrInt Op OldOrInt deriving stock (Show)

type WorryReducer = Int -> Int

data Monkey = Monkey {
  items   :: [Int]
  , update  :: Update
  , divisor :: Int
  , next :: (Int, Int)
  , count :: Int
} deriving stock (Show)

type Monkeys = M.Map Int Monkey

parser :: String -> Maybe Monkeys
parser = Just . M.fromList . zip [0..] . map parseMonkey . chunksOf 7 . lines

parseUpdate :: String -> Update
parseUpdate s =
  let ws = words s
      o1 = parseOoI $ ws !! 3
      op = if (ws !! 4) == "+" then Plus else Times
      o2 = parseOoI $ ws !! 5
  in Update o1 op o2
  where parseOoI w = if w == "old" then O else I (read w)

parseMonkey :: [String] -> Monkey
parseMonkey lls =
  Monkey {
  items = map readInt $ drop 2 $ words (lls !! 1),
  update = parseUpdate $ lls !! 2,
  divisor = wl (lls !! 3) 3,
  next = (wl (lls !! 4) 5, wl (lls !! 5) 5),
  count = 0}
  where
    readInt = read . takeWhile isDigit
    wl l i = read $ (words l) !! i

runMonkey :: WorryReducer -> Monkeys -> Int -> Monkeys
runMonkey wr monkeys i =
  let m = fromJust $ M.lookup i monkeys
      is = items m
  in case is of
    [] -> monkeys
    item:_ -> runMonkey wr (processItem wr i m item monkeys) i

runAllMonkeys :: WorryReducer -> Monkeys -> Monkeys
runAllMonkeys wr monkeys = foldl' (runMonkey wr) monkeys [0..(M.size monkeys - 1)]

processItem :: WorryReducer -> Int -> Monkey -> Int -> Monkeys -> Monkeys
processItem wr monkeyId monkey item monkeys =
  let (Update loi op roi) = update monkey
      lhs = getVal loi item
      rhs = getVal roi item
      fn = getFn op
      newVal = wr (fn lhs rhs)
      nexts = next monkey
      newId = if rem newVal (divisor monkey) == 0 then fst nexts else snd nexts
      newMonkey = fromJust $ M.lookup newId monkeys
      newMonkey' = newMonkey { items = concat [items newMonkey, [newVal]] }
      oldMonkey = monkey { items = tail $ items monkey, count = count monkey + 1 }
  in M.insert monkeyId oldMonkey (M.insert newId newMonkey' monkeys)
  where
    getVal oi item' =
      case oi of
        O -> item'
        I i -> i
    getFn op =
      case op of
        Plus -> (+)
        Times -> (*)

extractValues :: Monkeys -> [Int]
extractValues = M.foldr (\m vs -> count m : vs) []

extractDivisors :: Monkeys -> [Int]
extractDivisors = M.foldr (\m vs -> divisor m : vs) []

divisorFix :: Monkeys -> WorryReducer
divisorFix = flip mod . product . extractDivisors

monkeyBusiness :: WorryReducer -> Int -> Monkeys -> Int
monkeyBusiness wr i m =
  product $ take 2 $ sortBy (flip compare) $ extractValues $ (!! i) $ iterate (runAllMonkeys wr) m

day11a :: Monkeys :~> Int
day11a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . monkeyBusiness ((flip div) 3) 20
    }

day11b :: Monkeys :~> Int
day11b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \m -> Just $ monkeyBusiness (divisorFix m) 10000 m
    }
