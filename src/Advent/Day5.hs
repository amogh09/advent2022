module Advent.Day5 (solve1) where

import Advent.Util (maybeHead, readInt, splitAtEmptyLines)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

solve1 :: ByteString -> ByteString
solve1 s = do
  let (stacks, moves) = bimap parseStacks (fmap parseMove) . parseSections $ s
  B.pack . V.toList . fmap (fromMaybe ' ' . maybeHead) . foldl' move stacks $ moves

parseSections :: ByteString -> ([ByteString], [ByteString])
parseSections s =
  case splitAtEmptyLines s of
    [x, y] -> (x, y)
    xs -> error $ "expected two sections split by an empty line but found " <> show (length xs)

type Stack = [Char]

type Stacks = Vector Stack

data Move = Move Int Int Int deriving (Show)

parseStacks :: [ByteString] -> Stacks
parseStacks s' =
  case reverse s' of
    [] -> error "invalid input: stacks section is empty"
    (l : ls) -> do
      let n = readInt . last . B.words $ l
      foldl' parseStackLine (V.replicate n []) ls

parseStackLine :: Stacks -> ByteString -> Stacks
parseStackLine xs s =
  V.accum (flip (:)) xs
    . filter ((/= ' ') . snd)
    . zip [0 ..]
    . fmap (B.index s)
    $ [4 * i + 1 | i <- [0 .. (n - 1)]]
  where
    n = fromInteger . toInteger $ V.length xs

parseMove :: ByteString -> Move
parseMove s =
  case B.words s of
    ["move", n, "from", x, "to", y] -> Move (readInt n) (readInt x) (readInt y)
    _ -> error $ "invalid move line: " <> B.unpack s

move :: Stacks -> Move -> Stacks
move stacks (Move n x y) = do
  let (src, dst) = (stacks V.! (x - 1), stacks V.! (y - 1))
      (xs, src') = splitAt n src
      dst' = reverse xs ++ dst
  stacks V.// [(x - 1, src'), (y - 1, dst')]
