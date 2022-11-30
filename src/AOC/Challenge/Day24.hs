{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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

-- The slick approach:
-- https://github.com/giacomocavalieri/aoc-2021/blob/main/src/Days/Day24.hs

module AOC.Challenge.Day24 (
  day24a
  -- , day24b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List(foldl')
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

data Var = W | X | Y | Z deriving stock (Eq, Show)
data VarOrInt = Var Var | C Int deriving stock (Eq)
instance Show VarOrInt where
  show (Var W) = "W"
  show (Var X) = "X"
  show (Var Y) = "Y"
  show (Var Z) = "Z"
  show (C i) = show i

stringToVar :: String -> VarOrInt
stringToVar "w" = Var W
stringToVar "x" = Var X
stringToVar "y" = Var Y
stringToVar "z" = Var Z
stringToVar s = error $ "Unexpected variable name: " ++ s

data Inst = Inp VarOrInt |
            Add VarOrInt VarOrInt |
            Mul VarOrInt VarOrInt |
            Div VarOrInt VarOrInt |
            Mod VarOrInt VarOrInt |
            Eql VarOrInt VarOrInt deriving stock (Eq, Show)

data Digit = Digit Int deriving stock (Eq)
instance Show Digit where
  show (Digit i) = "D" ++ (show i)
  
data Value = DV Digit |
             CV Int |
             Sum Value Value |
             Prod Value Value deriving stock (Eq)
instance Show Value where
  show (DV d) = show d
  show (CV i) = show i
  show (Sum v1 v2) = "(" ++ (show v1) ++ "+" ++ (show v2) ++ ")"
  show (Prod v1 v2) = "(" ++ (show v1) ++ "*" ++ (show v2) ++ ")"

bounds :: Value -> (Int, Int)
bounds (DV _) = (1, 9)
bounds (CV i) = (i, i)
bounds (Sum v1 v2) =
  let (minv1, maxv1) = bounds v1
      (minv2, maxv2) = bounds v2
  in (minv1+minv2, maxv1+maxv2)
bounds (Prod v1 v2) =
  let (minv1, maxv1) = bounds v1
      (minv2, maxv2) = bounds v2
      prods = [minv1*minv2, minv1*maxv2, maxv1*minv2, maxv1*maxv2]
  in (minimum prods, maximum prods)

modValue :: Value -> Int -> Value
modValue (DV d) i = if i > 9 then (DV d) else error $ "Cannot mod digit with " ++ (show i)
modValue (CV c) i = CV (mod c i)
modValue (Sum v c) i = Sum (modValue v i) (modValue c i)
modValue p@(Prod _ (CV c)) i =
  if c == i then (CV 0) else error $ "modValue(Prod) bad constant mod: " ++ (show p)
modValue p@(Prod _ _) _ = error $ "modValue(Prod) not implemented for " ++ (show p)

divConst :: Value -> Int -> Value
divConst (Sum (Prod v (CV c)) rhs@(Sum _ _)) i =
  if c == i && snd (bounds rhs) < i
  then v
  else error "Bad divValue pattern for Sum."
divConst v i = if (snd (bounds v)) < i then (CV 0) else error "Bad divValue pattern."

data St = St {
  w :: Value,
  x :: Value,
  y :: Value,
  z :: Value,
  dc :: Int,
  p :: Int,
  cs :: [(Digit, Digit, Int)]
  } deriving stock (Eq, Show)

initState :: St
initState = St {
  w=CV 0,
  x=CV 0,
  y=CV 0,
  z=CV 0,
  dc=0,
  p=0,
  cs=[]
  }

parser :: String -> Maybe [Inst]
parser = Just . map parseInst . lines

parseVarOrInt :: String -> VarOrInt
parseVarOrInt s =
  case (readMaybe s :: Maybe Int) of
    Just i -> C i
    Nothing -> stringToVar s

parseInst :: String -> Inst
parseInst s =
  let ws = splitOn " " s
      inst = head ws
      a0 = stringToVar (ws !! 1)
  in
    if inst == "inp"
    then Inp a0
    else
      let a1 = parseVarOrInt (ws !! 2)
      in case inst of
        "add" -> Add a0 a1
        "mul" -> Mul a0 a1
        "div" -> Div a0 a1
        "mod" -> Mod a0 a1
        "eql" -> Eql a0 a1
        _ -> error "unknown"

errorMsg :: St -> Inst -> String
errorMsg st inst = " state=" ++ (show st) ++ ", inst=" ++ (show inst)



updateState :: St -> Inst -> St
-- SPECIAL CASE HACKS
updateState st@St{w=DV (Digit 3), cs=[], p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=[((Digit 3), (Digit 2), (-6))], p=p+1}
updateState st@St{w=DV (Digit 5), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 5), (Digit 4), 5):cs, p=p+1}
updateState st@St{w=DV (Digit 9), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 9), (Digit 8), (-5)):cs, p=p+1}
updateState st@St{w=DV (Digit 10), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 10), (Digit 7), 1):cs, p=p+1}
updateState st@St{w=DV (Digit 11), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 11), (Digit 6), (-8)):cs, p=p+1}
updateState st@St{w=DV (Digit 12), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 12), (Digit 1), 7):cs, p=p+1}
updateState st@St{w=DV (Digit 13), cs=cs, p=p} (Eql (Var X) (Var W)) = st{x=CV 1, cs=((Digit 13), (Digit 0), 8):cs, p=p+1}
-- Inp
updateState st@St{dc=dc, p=p} (Inp (Var W)) = st{w=DV (Digit dc), dc=dc+1, p=p+1}
-- Add
updateState st@St{z=CV 0, p=p} (Add (Var X) (Var Z)) = st{p=p+1}
updateState st@St{w=CV 0, p=p} (Add (Var Y) (Var W)) = st{p=p+1}
updateState st@St{x=CV i, p=p} (Add (Var X) (C a)) = st{x=CV $ i+a, p=p+1}
updateState st@St{y=CV y, p=p} (Add (Var Y) (C a)) = st{y=CV $ y+a, p=p+1}
updateState st@St{w=w, y=CV 0, p=p} (Add (Var Y) (Var W)) = st{y=w, p=p+1}
updateState st@St{w=DV d, y=CV y, p=p} (Add (Var Y) (Var W)) = st{y=Sum (DV d) (CV y), p=p+1}
updateState st@St{y=Sum v (CV yc), p=p} (Add (Var Y) (C a)) =st{y=Sum v (CV $ yc+a), p=p+1}
updateState st@St{x=Sum v (CV xc), p=p} (Add (Var X) (C a)) =st{x=Sum v (CV $ xc+a), p=p+1}
updateState st@St{x=x@(Sum _ _), p=p} (Add (Var X) (C a)) =st{x=Sum x (CV a), p=p+1}
updateState st@St{z=CV 0, y=y, p=p} (Add (Var Z) (Var Y)) = st{z=y, p=p+1}
updateState st@St{y=CV 0, p=p} (Add (Var Z) (Var Y)) = st{p=p+1}
updateState st@St{y=DV d, p=p} (Add (Var Y) (C a)) = st{y=Sum (DV d) (CV a), p=p+1}
updateState st@St{x=CV 0, z=z, p=p} (Add (Var X) (Var Z)) = st{x=z, p=p+1}
updateState st@St{y=y@(Sum _ _), z=z@(Prod _ _), p=p} (Add (Var Z) (Var Y)) = st{z=Sum z y, p=p+1}
-- Mul
updateState st@St{p=p} (Mul (Var X) (C 0)) = st{x=CV 0, p=p+1}
updateState st@St{y=CV y, p=p} (Mul (Var Y) (C c)) = st{y=CV $ y*c, p=p+1}
updateState st@St{x=CV 0, p=p} (Mul (Var Y) (Var X)) = st{y=CV 0, p=p+1}
updateState st@St{x=CV 1, p=p} (Mul (Var Y) (Var X)) = st{p=p+1}
updateState st@St{p=p} (Mul (Var Y) (C 0)) = st{y=CV 0, p=p+1}
updateState st@St{y=CV y, z=CV z, p=p} (Mul (Var Z) (Var Y)) = st{z=CV $ z*y, p=p+1}
updateState st@St{y=CV 1, p=p} (Mul (Var Z) (Var Y)) = st{p=p+1}
updateState st@St{y=CV c, z=z, p=p} (Mul (Var Z) (Var Y)) = st{z=Prod z (CV c), p=p+1}
-- Div
-- updateState st@St{p=p} (Div (Var Z) (C 1)) = st{p=p+1}
updateState st@St{p=p} (Div (Var Z) (C 1)) = st{p=p+1}
updateState st@St{z=CV z, p=p} (Div (Var Z) (C c)) = st{z=CV $ div z c, p=p+1}
updateState st@St{z=z, p=p} (Div (Var Z) (C c)) = st{z=divConst z c, p=p+1} 
-- Mod
updateState st@St{x=x, p=p} (Mod (Var X) (C m)) = st{x=modValue x m, p=p+1}
-- Eql
updateState st@St{x=x, w=w, p=p} inst@(Eql (Var X) (Var W)) =
  let (minx, maxx) = bounds x
      (minw, maxw) = bounds w
  in if minx==maxx && minw==maxw && minx==minw
     then st{x=CV 1, p=p+1}
     else if maxx < minw || maxw < minx
          then st{x=CV 0, p=p+1}
          else error $ "Eql error," ++ errorMsg st inst
updateState st@St{x=CV 0, p=p} (Eql (Var X) (C 0)) = st{x=CV 1, p=p+1}
updateState st@St{x=CV 1, p=p} (Eql (Var X) (C 0)) = st{x=CV 0, p=p+1}
-- Error              
updateState st inst = error $ "Update error," ++ errorMsg st inst

produceSequence :: St -> [Inst] -> [(Inst, St)]
produceSequence _ [] = []
produceSequence st (i:is) =
  let st' = updateState st i in (i, st'):produceSequence st' is

day24a :: _ :~> _
day24a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . produceSequence initState
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
