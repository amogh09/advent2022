module Advent.Day10 (solve1, solve2) where

import Advent.Util (bshow, readInt, third)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')
import Data.List.Split (chunksOf)

data Op = Noop | Addx Int

solve1 :: ByteString -> ByteString
solve1 = bshow . third . foldl' f (0 :: Int, 1 :: Int, 0 :: Int) . parseOps
  where
    f (!c, !r, !s) Noop = do
      let c' = c + 1
          s' = if (c + 1) `elem` interestingCycles then s + (c + 1) * r else s
      (c', r, s')
    f (!c, !r, !s) (Addx x) = do
      let s' = case ((c + 1) `elem` interestingCycles, (c + 2) `elem` interestingCycles) of
            (True, _) -> s + (c + 1) * r
            (_, True) -> s + (c + 2) * r
            _ -> s
      (c + 2, r + x, s')

    interestingCycles = [20, 60, 100, 140, 180, 220]

solve2 :: ByteString -> ByteString
solve2 = draw . reverse . third . foldl' f (0 :: Int, 1 :: Int, []) . parseOps
  where
    f (!c, !r, ps) Noop = (c + 1, r, pixel c r : ps)
    f (!c, !r, ps) (Addx x) = (c + 2, r + x, pixel (c + 1) r : pixel c r : ps)

    pixel p r = if (p `mod` width) `elem` [r - 1, r, r + 1] then '#' else '.'

draw :: [Char] -> ByteString
draw = B.unlines . fmap B.pack . chunksOf width

width :: Int
width = 40

parseOps :: ByteString -> [Op]
parseOps = fmap parseOp . B.lines

parseOp :: ByteString -> Op
parseOp s =
  case B.words s of
    ["noop"] -> Noop
    ["addx", x] -> Addx (readInt x)
    _ -> error $ "invalid input: " <> B.unpack s
