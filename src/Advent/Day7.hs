-- | Solutions for Advent of Code 2022, Day 7
module Advent.Day7 (solve1, solve2) where

import Advent.Util (bshow, compareMaybe, readInteger)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable (foldl', minimumBy)
import Data.Function (on)
import Data.Int (Int64)
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq

-- | Solution for part 1.
solve1 :: ByteString -> ByteString
solve1 = bshow . snd . smallDirs 0 . parseFileSystem

-- | Solution for part 2.
solve2 :: ByteString -> ByteString
solve2 s = do
  let fs = parseFileSystem s
      (totalSpace, neededSpace) = (70000000, 30000000)
      usedSpace = fst . largeEnoughSmallest 0 $ fs
      unusedSpace = totalSpace - usedSpace
      needToDelete = max 0 (neededSpace - unusedSpace)
  bshow . maybe 0 snd . snd . largeEnoughSmallest needToDelete $ fs

parseFileSystem :: ByteString -> Node Size
parseFileSystem = zipperToNode . foldl command emptyZipper . parseCommands

-- | Returns (size of node, updated sum of sizes of small directories).
-- A small directory is defined as one having size at most 100,000.
smallDirs :: Size -> Node Size -> (Size, Size)
smallDirs s EmptyNode = (0, s)
smallDirs s (Dir _ ns) =
  -- compute the size of this directory and also update the sum of small directory sizes seen
  let (dirSize, s') = foldl' (\(rds, rs) -> first (rds +) . smallDirs rs) (0, s) ns
   in if dirSize <= 100000 then (dirSize, s' + dirSize) else (dirSize, s')
smallDirs s (File _ fileSize) = (fileSize, s)

-- Returns a tuple of size of the provided node and
-- maybe name and size of the smallest directory under the node
-- that is larger than the provided threshold.
largeEnoughSmallest :: Size -> Node Size -> (Size, Maybe (Name, Size))
largeEnoughSmallest _ EmptyNode = (0, Nothing)
largeEnoughSmallest threshold (Dir name ns) = do
  let results = fmap (largeEnoughSmallest threshold) ns
      dsize = sum . fmap fst $ results
      dresult = if dsize >= threshold then Just (name, dsize) else Nothing
      result = minimumBy (compareMaybe `on` fmap snd) (dresult :<| fmap snd results)
  (dsize, result)
largeEnoughSmallest _ (File _ size) = (size, Nothing)

type Name = ByteString

type Size = Int64

-- | Represents a file system node - empty, directory, or a file
data Node a = EmptyNode | Dir Name (Seq (Node a)) | File Name a deriving (Show)

-- | Represents a single command - cd or ls
data Command a = Cd ByteString | Ls [Node a] deriving (Show)

-- | Crumb type for the zipper used to create the file system.
data Crumb a = Crumb Name (Seq (Node a)) (Seq (Node a)) deriving (Show)

-- | Zipper for file system creation. Essentially a builder.
-- Contains the current node in focus and a list of crumbs used to reach that node.
type Zipper a = (Node a, [Crumb a])

-- | Creates an empty zipper.
emptyZipper :: Zipper a
emptyZipper = (EmptyNode, [])

-- | Continuously "zips up" the zipper to its root.
zipperRoot :: Show a => Zipper a -> Zipper a
zipperRoot z@(_, []) = z
zipperRoot z@(Dir _ _, _) = zipperRoot . command z $ Cd ".."
zipperRoot z@(File _ _, _) = zipperRoot . command z $ Cd ".."
zipperRoot z = error $ "invalid zipper state when finding root: " <> show z

-- | Converts the file system zipper to the file system it has built.
zipperToNode :: Show a => Zipper a -> Node a
zipperToNode = fst . zipperRoot

-- | Executes a single command on the file system zipper. Returns a new updated zipper.
-- `cd /`  from empty node initializes the file system with the root node.
-- `cd ..` zips up the zipper by one level by consuming the first crumb and converting it to
--         parent directory.
-- `cd x`  zips down the zipper by one level by creating a new crumb for the current directory
--         and adding it to the list of crumbs. The new focus directory is found by scanning
--         the list of subdirectories of the current directory by their names.
-- `ls`    populates the current directory with the contents of the ls command iff the current
--         directory is empty.
-- Throws an error for invalid cases.
command :: Show a => Zipper a -> Command a -> Zipper a
command (EmptyNode, []) (Cd "/") = (Dir "/" Seq.empty, [])
command (n, Crumb name ls rs : cs) (Cd "..") = (Dir name (ls >< pure n >< rs), cs)
command (Dir name ns, cs) (Cd x) =
  let (ls, result) = Seq.breakl (nameIs x) ns
   in case result of
        (n :<| rs) -> (n, Crumb name ls rs : cs)
        _ -> error "invalid cd command has no argument"
command (Dir name Seq.Empty, cs) (Ls ns) = (Dir name $ Seq.fromList ns, cs)
command (Dir name ns, cs) (Ls _) = (Dir name ns, cs) -- don't replace known dir
command z c = error $ "invalid case: " <> show z <> " | " <> show c

-- | Checks if the name of the directory or file matches the provided name.
nameIs :: Name -> Node a -> Bool
nameIs n (Dir n' _) = n == n'
nameIs n (File n' _) = n == n'
nameIs _ _ = False

-- | Parses all commands from the bytestring.
-- This function starts by splitting the bytestring at '$' symbols. Then for each split,
-- it checks if the command starts with "cd" or "ls".
-- For "cd" commands, the word following "cd" is read.
-- For "ls" commands, all lines following the "ls" line are read.
-- Throws an error if parsing fails for any reason (indicating invalid input).
parseCommands :: ByteString -> [Command Size]
parseCommands = fmap (parseCommand . B.tail) . tail . B.split '$'
  where
    parseCommand s | "cd" `B.isPrefixOf` s = Cd . B.dropEnd 1 . B.drop 3 $ s
    parseCommand s | "ls" `B.isPrefixOf` s = Ls . fmap parseLsItem . tail . B.lines $ s
    parseCommand s = error $ "invalid input: couldn't parse " <> B.unpack s

    parseLsItem s =
      case B.words s of
        ["dir", name] -> Dir name Seq.empty
        [size, name] -> File name . fromInteger . readInteger $ size
        _ -> error $ "invalid input: unknown ls item: " <> B.unpack s
