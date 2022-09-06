module AOC.Challenge.Day20 (
  day20a
  , day20b
 ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import qualified Data.Array.Unboxed as A
import Data.List.Split(splitOn)
import qualified Data.Set as S

type Ix = (Int, Int)
type Lookup = A.Array Int Bool
data Image = Image
 { array :: S.Set Ix,
   view :: (Ix, Ix),
   exterior_on :: Bool } deriving stock (Show)

parseEnhancementString :: String -> Lookup
parseEnhancementString s = A.listArray (0, length s - 1) (map ('#' ==) s)

parseImage :: String -> Image
parseImage s =
 let lls = lines s
     nlls = length (lines s)
 in Image { array = foldr processRow S.empty $ zip (lines s) [0..nlls],
            view = ((0, 0), (nlls, length (head lls))),
            exterior_on = False }
 where
   processRow (row, ri) arr = foldr (maybeInsert ri) arr (zip row [0..])
   maybeInsert ri (c, ci) img = if c == '#' then S.insert (ri, ci) img else img

parser :: String -> Maybe (Lookup, Image)
parser s =
 let lookupAndImage = splitOn "\n\n" s
 in Just (parseEnhancementString $ head lookupAndImage,
          parseImage $ lookupAndImage !! 1)

inBounds :: (Ix, Ix) -> Ix -> Bool
inBounds ((min_r, min_c), (max_r, max_c)) (r, c) =
 min_r <= r && r <= max_r && min_c <= c && c <= max_c

getLookupString :: Image -> Ix -> [Bool]
getLookupString img (r, c) =
 let offsets = [(r', c') | r' <- [-1..1], c' <- [-1..1]]
 in map (\(r', c') -> lookupOne (r + r', c + c')) offsets
 where lookupOne ix =
         if inBounds (view img) ix
         then S.member ix (array img)
         else exterior_on img

boolsToInt :: [Bool] -> Int
boolsToInt = foldr (\b s -> 2 * s + (if b then 1 else 0)) 0 . reverse

expandView :: (Ix, Ix) -> Int -> (Ix, Ix)
expandView ((min_r, min_c), (max_r, max_c)) n =
 ((min_r-n, min_c-n), (max_r+n, max_c+n))

enhanceImage :: Lookup -> Image -> Image
enhanceImage lookupTable img =
 let new_view@((min_r, min_c), (max_r, max_c)) = expandView (view img) 2
     inds = [(r, c) | r <- [min_r..max_r], c <- [min_c..max_c]]
 in Image { array = foldr processInd S.empty inds,
            view = new_view,
            exterior_on = lookupTable A.! 0 && not (exterior_on img) }
 where processInd ix new_img =
         if lookupTable A.! boolsToInt (getLookupString img ix)
         then S.insert ix new_img else new_img

enhanceN :: Int -> (Lookup, Image) -> Int
enhanceN 0 (_, img) = length (array img)
enhanceN n (lt, img) = enhanceN (n-1) (lt, enhanceImage lt img)
  
day20a :: (Lookup, Image) :~> Int
day20a = MkSol
   { sParse = parser
   , sShow  = show
   , sSolve = Just . enhanceN 2
   }

day20b :: (Lookup, Image) :~> Int
day20b = MkSol
   { sParse = parser
   , sShow  = show 
   , sSolve = Just . enhanceN 50
   }
