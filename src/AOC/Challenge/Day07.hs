{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
  day07a
  , buildDirSizes
  , parser
  , testInput
  , day07b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Path = [String]
type Subdirs = [String]
type File = (String, Int)
type Contents = (Subdirs, [File])
type FileSys = M.Map Path Contents
type DirSizes = M.Map Path Int

parser :: String -> Maybe [Int]
parser lls = let fs = snd $ parseWithPath [] M.empty $ lines lls
  in Just $ map snd $ M.assocs $ buildDirSizes fs ["/"] M.empty

parseWithPath :: Path -> FileSys -> [String] -> (Path, FileSys)
parseWithPath p fs lls =
  if null lls then (p, fs)
  else
    case words (head lls) of
      ["$", "cd", ".."] -> parseWithPath (tail p) fs (tail lls)
      ["$", "cd", d] -> parseWithPath (d:p) fs (tail lls)
      ["$", "ls"] -> parseDirectory p fs (tail lls)
      _ -> undefined
      
parseDirectory :: Path -> FileSys -> [String] -> (Path, FileSys)
parseDirectory p fs lls =
  let (dirlines, rest) = span (('$' /=) . head) lls
      contents = foldr parseContentLine ([], []) dirlines
  in parseWithPath p (M.insert p contents fs) rest

parseContentLine :: String -> Contents -> Contents
parseContentLine l (subdirs, files) =
  let ws = words l
  in if head ws == "dir"
     then (ws !! 1 : subdirs, files)
     else (subdirs, (ws !! 1, read $ ws !! 0) : files)

buildDirSizes :: FileSys -> Path -> DirSizes -> DirSizes
buildDirSizes fs p ds =
  let (subdirs, files) = forceLookup p fs
      withSubdirs = foldr (\sd -> buildDirSizes fs (sd:p)) ds subdirs
      subdirTotal = sum $ map ((flip forceLookup) withSubdirs . (flip (:)) p) subdirs
      localTotal = sum $ map snd $ files
  in M.insert p (subdirTotal + localTotal) withSubdirs
  where forceLookup k m = fromJust $ M.lookup k m

minSizeToRemove :: [Int] -> Int
minSizeToRemove sizes =
  let currentSpace = 70000000 - (maximum sizes)
      neededSpace = 30000000 - currentSpace
  in minimum (filter (>= neededSpace) sizes)

day07a :: _ :~> _
day07a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . filter (<= 100000)
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minSizeToRemove
    }

testInput :: String
testInput = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
