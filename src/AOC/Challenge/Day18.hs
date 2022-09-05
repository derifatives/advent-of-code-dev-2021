module AOC.Challenge.Day18 (
  day18a
  , day18b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Applicative ((<|>))
import Data.List (tails)
import Numeric (readDec)

data Token = L | R | I Int deriving stock (Eq, Show)
type Snailfish = [Token]

parser :: String -> Maybe [Snailfish]
parser = Just . map (parseSnailfish []) . lines
  where
    parseSnailfish soFar [] = reverse soFar
    parseSnailfish soFar ('[':rest) = parseSnailfish (L : soFar) rest
    parseSnailfish soFar (']':rest) = parseSnailfish (R : soFar) rest
    parseSnailfish soFar (',':rest) = parseSnailfish soFar rest
    parseSnailfish soFar rest = 
       let (i, rest') = head $ readDec rest
       in parseSnailfish (I i : soFar) rest'

toString :: Snailfish -> String
toString = fst . toString'
  where
    toString' (L:rest) = 
      let (n1, rest') = toString' rest
          (n2, rest'') = toString' rest'
      in ("[" ++ n1 ++ "," ++ n2 ++ "]", tail rest'')
    toString' (I i:rest) = (show i, rest)
    toString' _ = error "Unexpected parse in toString'."

tryExplode :: Snailfish -> Maybe Snailfish
tryExplode = search [] [] (0 :: Int)
  where
    search _ _ _ [] = Nothing
    search toNumR pastNumR d (L:rest) = search toNumR (L : pastNumR) (d+1) rest
    search toNumR pastNumR d (R:rest) = search toNumR (R : pastNumR) (d-1) rest
    search toNumR pastNumR d (I i:rest) =
      if d <= 4
      then search (I i : (pastNumR ++ toNumR)) [] d rest
      else
        let toNumR' = if null toNumR then [] else addToHead i toNumR
            pastNumR' = tail pastNumR  -- Remove extra L
            prefix = reverse (I 0 : (pastNumR' ++ toNumR'))
            rhs = head rest
        in Just $ prefix ++ addExplodedRhs rhs [] (tail (tail rest))
    addExplodedRhs (I i') soFar (I i:rest) = reverse soFar ++ I (i' + i):rest
    addExplodedRhs rhs soFar (c:rest) = addExplodedRhs rhs (c:soFar) rest
    addExplodedRhs _ soFar [] = reverse soFar

addToHead :: Int -> Snailfish -> Snailfish
addToHead i' (I i:rest) = I (i+i') : rest
addToHead _ _ = error "Attempt to add to Snailfish non-numeric head."

trySplit :: Snailfish -> Maybe Snailfish
trySplit = search []
  where
    search _ [] = Nothing
    search soFarR (I i:rest) =
      if i >= 10
      then let (half, x) = quotRem i 2
           in Just (reverse soFarR ++ [L, I half, I (half + x), R] ++ rest)
      else search (I i:soFarR) rest
    search soFarR (t:rest) = search (t:soFarR) rest
  
reduce :: Snailfish -> Snailfish
reduce s = maybe s reduce (tryExplode s <|> trySplit s)

add :: Snailfish -> Snailfish -> Snailfish
add s1 s2 = [L] ++ s1 ++ s2 ++ [R]

magnitude :: Snailfish -> Int
magnitude = fst . mag'
  where
    mag' (L:rest) =
      let (m1, rest') = mag' rest
          (m2, rest'') = mag' rest'
      in (3 * m1 + 2 * m2, tail rest'')
    mag' (I i:rest) = (i, rest)
    mag' _ = error "Unexpected parse in mag'."

addList :: [Snailfish] -> Snailfish
addList = foldl1 (\s1 s2 -> reduce $ add s1 s2)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

maximumPairSum :: [Snailfish] -> Int
maximumPairSum ss = maximum $ map maxOfPair (pairs ss)
  where maxOfPair (s1, s2) =
          max (mra s1 s2) (mra s2 s1)
        mra s1 = magnitude . reduce . add s1

day18a :: [Snailfish] :~> Int
day18a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . magnitude . addList
    }

day18b :: [Snailfish] :~> Int
day18b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . maximumPairSum
    }
