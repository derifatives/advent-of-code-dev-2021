-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
  day16a
  , day16b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Control.Monad.State(State, get, put, evalState, runState)
import Data.Char (digitToInt)
import Data.List.Extra (snoc, splitAt, take)
import Numeric (readHex)

charToBits' :: Char -> [Int]
charToBits' = zeroPad . toBinary . digitToInt

toBinary :: Int -> [Int]
toBinary n = if n > 0 then toBinary (div n 2) `snoc` (mod n 2) else []

zeroPad :: [Int] -> [Int]
zeroPad is = (replicate (4 - length is) 0) ++ is

charsToBits :: [Char] -> [Int]
charsToBits = concatMap charToBits'

fromBinary :: [Int] -> Int
fromBinary = foldl (\t d -> 2*t + d) 0

type Version = Int
type TypeId = Int
type Bitstream = State [Int]
data Packet = Packet Version TypeId PacketContents deriving stock (Show)
data Op = Sum | Product | Min | Max | Greater | Lesser | Equals deriving stock (Show)
data PacketContents
  = Value Int | Packets Op [Packet] deriving stock (Show)

parsePacket :: Bitstream Packet
parsePacket = do
  versionBits <- grabNBits 3
  let version = fromBinary versionBits
  typeBits <- grabNBits 3
  let typeId = fromBinary typeBits
  subpacket <- parseValueOrOp typeId
  pure (Packet version typeId subpacket)
  
parseValueOrOp :: Int -> Bitstream PacketContents
parseValueOrOp typeId = do
  if typeId == 4
    then parseValue
    else parsePacketContents (typeIdToOp typeId)

parseValue :: Bitstream PacketContents
parseValue = do
  valueBits <- grabValueBits []
  pure (Value (fromBinary valueBits))

grabNBits :: Int -> Bitstream [Int]
grabNBits n = do
  bitstream <- get
  let block = take n bitstream
  put $ drop n bitstream
  pure block

grabValueBits :: [Int] -> Bitstream [Int]
grabValueBits acc = do
  block <- grabNBits 5
  let new_acc = acc ++ (drop 1 block)
  if head block == 1
    then grabValueBits new_acc
    else pure new_acc

typeIdToOp :: Int -> Op
typeIdToOp 0 = Sum
typeIdToOp 1 = Product
typeIdToOp 2 = Min
typeIdToOp 3 = Max
typeIdToOp 5 = Greater
typeIdToOp 6 = Lesser
typeIdToOp 7 = Equals
typeIdToOp n = error $ "Unrecognized opcode: " ++ (show n)

parsePacketContents :: Op -> Bitstream PacketContents
parsePacketContents op = do
  b <- grabNBits 1
  if head b == 0
    then do lengthBits <- grabNBits 15
            subpacketsBits <- grabNBits (fromBinary lengthBits)
            rest <- get
            put subpacketsBits
            packets <- parseAllPackets []
            put rest
            pure (Packets op packets)
    else do numPacketsBits <- grabNBits 11
            let numPackets = fromBinary numPacketsBits
            packets <- parseNPackets numPackets []
            pure (Packets op packets)

parseAllPackets :: [Packet] -> Bitstream [Packet]
parseAllPackets packets = do
  bitstream <- get
  if bitstream == []
    then pure packets
    else do packet <- parsePacket
            parseAllPackets (packets `snoc` packet)
          
parseNPackets :: Int -> [Packet] -> Bitstream [Packet]
parseNPackets n packets = do
  if n == 0
    then pure packets
    else do packet <- parsePacket
            parseNPackets (n-1) (packets `snoc` packet)
            
versionSum :: Packet -> Int
versionSum (Packet v _ (Value _)) = v
versionSum (Packet v _ (Packets _ ps)) = v + sum (map versionSum ps)

scorePacket :: Packet -> Int
scorePacket (Packet _ _ (Value v)) = v
scorePacket (Packet _ _ (Packets op ps)) =
  let scores = map scorePacket ps
  in case op of
    Sum -> sum scores
    Product -> product scores
    Min -> minimum scores
    Max -> maximum scores
    Greater -> if (scores !! 0) > (scores !! 1) then 1 else 0
    Lesser -> if (scores !! 0) < (scores !! 1) then 1 else 0
    Equals -> if (scores !! 0) == (scores !! 1) then 1 else 0
  
day16a :: String :~> Int
day16a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . versionSum . evalState parsePacket . charsToBits
    }

day16b :: String :~> Int
day16b = MkSol
    { sParse = Just 
    , sShow  = show
    , sSolve = Just . scorePacket . evalState parsePacket . charsToBits
    }
