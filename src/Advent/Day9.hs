module Advent.Day9 (solve1, solve2) where

import Advent.Util (bshow, readInt)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

type Position = (Int, Int)

type Rope = [Position]

data Direction = Up | Down | Left | Right

ropeZero :: Int -> Rope
ropeZero x = replicate x (0, 0)

solve1 :: ByteString -> ByteString
solve1 = solve 2

solve2 :: ByteString -> ByteString
solve2 = solve 10

solve :: Int -> ByteString -> ByteString
solve l =
  bshow
    . length
    . Set.toList
    . Set.fromList
    . fmap last
    . scanl moveDir (ropeZero l)
    . parseDirections

parseDirections :: ByteString -> [Direction]
parseDirections = concatMap parseDirection . B.lines

parseDirection :: ByteString -> [Direction]
parseDirection s =
  case B.words s of
    ["R", v] -> replicate (readInt v) Right
    ["L", v] -> replicate (readInt v) Left
    ["U", v] -> replicate (readInt v) Up
    ["D", v] -> replicate (readInt v) Down
    _ -> error $ "invalid moves input: " <> B.unpack s

moveDir :: Rope -> Direction -> Rope
moveDir r d =
  let r' = movePosition (head r) d : fmap (uncurry tailPos) (zip r' (tail r))
   in r'

-- | From new head and current tail position, returns the new tail position.
tailPos :: Position -> Position -> Position
tailPos h@(hx, hy) t@(tx, ty)
  | dist t h <= 1 = t
  | hx == tx = (tx, (hy + ty) `div` 2)
  | hy == ty = ((hx + tx) `div` 2, ty)
  | hx > tx && hy > ty = (tx + 1, ty + 1)
  | hx > tx && hy < ty = (tx + 1, ty - 1)
  | hx < tx && hy > ty = (tx - 1, ty + 1)
  | otherwise = (tx - 1, ty - 1)

movePosition :: Position -> Direction -> Position
movePosition (x, y) Up = (x, y + 1)
movePosition (x, y) Down = (x, y - 1)
movePosition (x, y) Right = (x + 1, y)
movePosition (x, y) Left = (x - 1, y)

dist :: Position -> Position -> Int
dist (x, y) (x', y') = abs (x - x') `max` abs (y - y')
