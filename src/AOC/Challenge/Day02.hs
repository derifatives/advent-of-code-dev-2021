-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
  day02a
  , day02b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)

data Inst = Up Int | Down Int | Forward Int deriving stock (Show)
data Pos = Pos Int Int Int deriving stock (Show)

parseInst :: [String] -> Inst
parseInst (i:n:[]) =
  case i of
    "up"      -> Up ni
    "down"    -> Down ni
    "forward" -> Forward ni
    _         -> error "Unknown instruction."
  where ni = read n
parseInst _ = error "Instruction with more than two words."

updatePosA :: Pos -> Inst -> Pos
updatePosA (Pos h d _) (Up n)      = Pos h (d-n) 0
updatePosA (Pos h d _) (Down n)    = Pos h (d+n) 0
updatePosA (Pos h d _) (Forward n) = Pos (h+n) d 0

updatePosB :: Pos -> Inst -> Pos
updatePosB (Pos h d a) (Up n)      = Pos h d (a-n)
updatePosB (Pos h d a) (Down n)    = Pos h d (a+n)
updatePosB (Pos h d a) (Forward n) = Pos (h+n) (d+a*n) a

posProduct :: Pos -> Int
posProduct (Pos h d _) = h * d

day02a :: [Inst] :~> Pos
day02a = MkSol
    { sParse = Just . map (parseInst . words) . lines
    , sShow  = show . posProduct
    , sSolve = Just . foldl updatePosA (Pos 0 0 0)
    }

day02b :: [Inst] :~> Pos
day02b = MkSol
    { sParse = Just . map (parseInst . words) . lines
    , sShow  = show . posProduct
    , sSolve = Just . foldl updatePosB (Pos 0 0 0)
    }
