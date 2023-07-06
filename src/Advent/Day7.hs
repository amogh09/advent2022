module Advent.Day7 (solve1) where

import Advent.Util (bshow, readInteger)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq

solve1 :: ByteString -> ByteString
solve1 = bshow . snd . smallDirs 0 . zipperToNode . foldl command emptyZipper . parseCommands

-- Returns (size of node, updated sum of sizes of small directories).
-- A small directory is defined as one having size at most 100,000.
smallDirs :: Int64 -> Node Int64 -> (Int64, Int64)
smallDirs s EmptyNode = (0, s)
smallDirs s (Dir _ ns) =
  let (dirSize, s') = foldl' (\(rds, rs) -> first (rds +) . smallDirs rs) (0, s) ns
   in if dirSize <= 100000 then (dirSize, s' + dirSize) else (dirSize, s')
smallDirs s (File _ fileSize) = (fileSize, s)

type Name = ByteString

data Node a = EmptyNode | Dir Name (Seq (Node a)) | File Name a deriving (Show)

data Command a = Cd ByteString | Ls [Node a] deriving (Show)

data Crumb a = Crumb Name (Seq (Node a)) (Seq (Node a)) deriving (Show)

type Zipper a = (Node a, [Crumb a])

emptyZipper :: Zipper a
emptyZipper = (EmptyNode, [])

zipperRoot :: Show a => Zipper a -> Zipper a
zipperRoot z@(_, []) = z
zipperRoot z@(Dir _ _, _) = zipperRoot . command z $ Cd ".."
zipperRoot z@(File _ _, _) = zipperRoot . command z $ Cd ".."
zipperRoot z = error $ "invalid zipper state when finding root: " <> show z

zipperToNode :: Show a => Zipper a -> Node a
zipperToNode = fst . zipperRoot

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

nameIs :: Name -> Node a -> Bool
nameIs n (Dir n' _) = n == n'
nameIs _ _ = False

parseCommands :: ByteString -> [Command Int64]
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
