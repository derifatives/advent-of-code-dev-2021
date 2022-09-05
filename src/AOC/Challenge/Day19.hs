module AOC.Challenge.Day19 (
  day19a
  , day19b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Foldable(asum)
import Data.List(find, nub)
import Data.List.Split(splitOn)
import qualified Data.Map as Map
import Linear.Matrix(M33, (!*!), (!*), identity)
import Linear.V3
import Linear.Vector((^+^), (^-^))

newtype Beacon = B (V3 Int) deriving stock (Eq, Show)
type Scanner = [Beacon]
data RelativeScanner = RelativeScanner
  { position :: V3 Int
  , scanner :: Scanner } deriving stock (Show)

makeOriginScanner :: Scanner -> RelativeScanner
makeOriginScanner s = RelativeScanner { position = V3 0 0 0, scanner = s }

parser :: String -> Maybe [Scanner]
parser = Just . map parseOneScanner . splitOn "\n\n"

parseOneScanner :: String -> Scanner
parseOneScanner = map ((parseOneBeacon . map read) . splitOn ",")  . tail . lines

parseOneBeacon :: [Int] -> Beacon
parseOneBeacon [x', y', z'] = B (V3 x' y' z')
parseOneBeacon _ = error "Unexpected pattern."

newtype Rotation = R (M33 Int) deriving stock (Eq, Show)
x, y :: Rotation
x = R (V3 (V3 1 0 0) (V3 0 0 (-1)) (V3 0 1 0))
y = R (V3 (V3 0 0 1) (V3 0 1 0) (V3 (-1) 0 0))

allAtomizedRotations :: [[Rotation]]
allAtomizedRotations = [
  [R identity],
  [x], [y],
  [x, x], [x, y], [y, x], [y, y],
  [x, x, x], [x, x, y], [x, y, x], [y, x, x], [x, y, y], [y, y, x], [y, y, y],
  [x, x, x, y], [x, x, y, x], [x, y, x, x], [y, x, x, x],
  [x, x, y, y], [x, y, y, y], [y, y, y, x],
  [x, x, x, y, x], [x, y, x, x, x], [x, y, y, y, x]]

assembleRotation :: [Rotation] -> Rotation
assembleRotation = foldl1 (\(R m1) (R m2) -> R (m1 !*! m2))

allRotations :: [Rotation]
allRotations = map assembleRotation allAtomizedRotations

applyRotation :: Scanner -> Rotation -> Scanner
applyRotation s (R m) = map (\(B v) -> B (m !* v)) s

countsMap :: Ord k => [k] -> Map.Map k Int
countsMap = foldr (\k m -> Map.insertWith (+) k 1 m) Map.empty

matchScanner :: RelativeScanner -> Int -> Scanner -> Maybe RelativeScanner
matchScanner rs threshold s' =
  let diffs = [v ^-^ v' | B v <- scanner rs, B v' <- s']
      counts = countsMap diffs
      goodDiff = find (\(_, c) -> c >= threshold) (Map.assocs counts)
  in fmap (\(d, _) ->  RelativeScanner {
              position = d,
              scanner = [B (v' ^+^ d) | B v' <- s']}) goodDiff

partitioner :: (a -> Maybe b) -> [a] -> ([b], [a])
partitioner f input = p' input [] []
  where p' [] bs as = (bs, as)
        p' (i:is) bs as =
          case f i of
            Nothing -> p' is bs (i:as)
            Just b -> p' is (b:bs) as

buildGraph :: Int -> [Scanner] -> [RelativeScanner]
buildGraph threshold ss =
  let initialScanner = makeOriginScanner (head ss)
      rotatedScanners = map (\s -> map (applyRotation s) allRotations) (tail ss)
  in bg' [] [initialScanner] rotatedScanners
  where
    bg' done found [] = done ++ found
    bg' _ [] _ = error "Failed to build graph."
    bg' done (f:fs) open =
      let (newFound, stillOpen) =
            partitioner (asum . map (matchScanner f threshold)) open
      in bg' (done ++ [f]) (fs ++ newFound) stillOpen

manhattan :: V3 Int -> V3 Int -> Int
manhattan v1 v2  =
  let (V3 x' y' z') = v1 ^-^ v2
  in abs x' + abs y' + abs z'

maxManhattan :: [V3 Int] -> Int
maxManhattan bs = maximum [manhattan b1 b2 | b1 <- bs, b2 <- bs]
  
day19a :: [Scanner] :~> Int
day19a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . length . nub . concatMap scanner . buildGraph 12
    }

day19b :: [Scanner] :~> Int
day19b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . maxManhattan . map position . buildGraph 12
    }
