module AOC.Challenge.Day22 (
  day22a
  , day22b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Monad
import Data.Either
import Data.List(delete, find)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Dir = On | Off deriving stock (Eq, Show)

data Range = Range {
  bottom :: Int,
  top :: Int
  } deriving stock (Eq, Show)

data Cuboid = Cuboid {
  dir :: Dir,
  x :: Range,
  y :: Range,
  z :: Range } deriving stock (Eq, Show)

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

parseCuboid :: Parser Cuboid
parseCuboid = do
  dir <- parseDir
  void (string " x=")
  x <- parseRange
  void (string ",y=")
  y <- parseRange
  void (string ",z=")
  z <- parseRange
  return Cuboid { dir=dir, x=x, y=y, z=z }

size :: Cuboid -> Integer
size Cuboid{x=Range{bottom=xb, top=xt},
            y=Range{bottom=yb, top=yt},
            z=Range{bottom=zb, top=zt}} =
  toInteger $ (xt - xb + 1) * (yt - yb + 1) * (zt - zb + 1)

rangeOverlapP :: Range -> Range -> Bool
rangeOverlapP Range{bottom=b1, top=t1} Range{bottom=b2, top=t2} =
  (b2 <= b1 && b1 <= t2) ||
  (b2 <= t1 && t1 <= t2) ||
  (b1 <= b2 && b2 <= t1) ||
  (b1 <= t2 && t2 <= t1)

overlapP :: Cuboid -> Cuboid -> Bool
overlapP Cuboid{x=x1, y=y1, z=z1} Cuboid{x=x2, y=y2, z=z2} =
  rangeOverlapP x1 x2 &&
  rangeOverlapP y1 y2 &&
  rangeOverlapP z1 z2
  
cuboidParser :: String -> Cuboid
cuboidParser s = fromRight undefined (runParser parseCuboid "" s)

parser :: String -> Maybe [Cuboid]
parser = Just . map cuboidParser . lines

initialRange :: Range
initialRange = Range{bottom=(-50), top=50}

initialCuboid :: Cuboid
initialCuboid = Cuboid{dir=On, x=initialRange, y=initialRange, z=initialRange}

cuboidIntersectsInitial :: Cuboid -> Bool
cuboidIntersectsInitial = overlapP initialCuboid

reactorSize :: [Cuboid] -> Integer
reactorSize = sum . map size

atomizeCuboids :: Cuboid -> Cuboid -> (Cuboid, ([Cuboid], [Cuboid]))
atomizeCuboids new@Cuboid{dir=dirn, x=xn@Range{bottom=xnb, top=xnt}, y=yn@Range{bottom=ynb, top=ynt}, z=zn@Range{bottom=znb, top=znt}}
  old@Cuboid{x=xo@Range{bottom=xob, top=xot}, y=yo@Range{bottom=yob, top=yot}, z=zo@Range{bottom=zob, top=zot}} =
  let xv=Range{bottom=max xnb xob, top=min xnt xot}
      yv=Range{bottom=max ynb yob, top=min ynt yot}
      zv=Range{bottom=max znb zob, top=min znt zot}
      overlap = Cuboid{dir=dirn, x=xv, y=yv, z=zv}
      xLowNew = if xnb < xob then [new{x=xn{top=xob-1}}] else []
      xLowOld = if xob < xnb then [old{x=xo{top=xnb-1}}] else []
      xHighNew = if xot < xnt then [new{x=xn{bottom=xot+1}}] else []
      xHighOld = if xnt < xot then [old{x=xo{bottom=xnt+1}}] else []
      yLowNew = if ynb < yob then [new{x=xv, y=yn{top=yob-1}}] else []
      yLowOld = if yob < ynb then [old{x=xv, y=yo{top=ynb-1}}] else []
      yHighNew = if yot < ynt then [new{x=xv, y=yn{bottom=yot+1}}] else []
      yHighOld = if ynt < yot then [old{x=xv, y=yo{bottom=ynt+1}}] else []
      zLowNew = if znb < zob then [new{x=xv, y=yv, z=zn{top=zob-1}}] else []
      zLowOld = if zob < znb then [old{x=xv, y=yv, z=zo{top=znb-1}}] else []
      zHighNew = if zot < znt then [new{x=xv, y=yv, z=zn{bottom=zot+1}}] else []
      zHighOld = if znt < zot then [old{x=xv, y=yv, z=zo{bottom=znt+1}}] else []
  in (overlap,
      (concat([xLowNew, xHighNew, yLowNew, yHighNew, zLowNew, zHighNew]),
       concat([xLowOld, xHighOld, yLowOld, yHighOld, zLowOld, zHighOld])))

bootReactor :: [Cuboid] -> [Cuboid] -> [Cuboid]
bootReactor [] rs = rs
bootReactor (c:cs) rs =
  let d = dir c in
    case find (overlapP c) rs of
      Nothing -> bootReactor cs (if d == On then (c:rs) else rs)
      Just r ->
        let (o, (new, old)) = atomizeCuboids c r
            rs' = delete r rs
            o' = if d == On then [o] else []
        in bootReactor (new ++ cs) (old ++ o' ++ rs')

day22a :: _ :~> _
day22a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . reactorSize . (flip bootReactor) [] . filter cuboidIntersectsInitial
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . reactorSize . (flip bootReactor) []
    }
