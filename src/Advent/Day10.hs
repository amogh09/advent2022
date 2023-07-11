module Advent.Day10 (solve1) where

import Advent.Util (bshow, readInt, third)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')

data Op = Noop | Addx Int

solve1 :: ByteString -> ByteString
solve1 = bshow . third . foldl' f (0 :: Int, 1 :: Int, 0 :: Int) . parseOps
  where
    f (!c, !r, !s) Noop = do
      let c' = c + 1
          s' = if (c + 1) `elem` interestingCycles then s + (c + 1) * r else s
      (c', r, s')
    f (!c, !r, !s) (Addx x) = do
      let c' = c + 2
          r' = r + x
          s' = case ((c + 1) `elem` interestingCycles, (c + 2) `elem` interestingCycles) of
            (True, _) -> s + (c + 1) * r
            (_, True) -> s + (c + 2) * r
            _ -> s
      (c', r', s')

    interestingCycles = [20, 60, 100, 140, 180, 220]

parseOps :: ByteString -> [Op]
parseOps = fmap parseOp . B.lines

parseOp :: ByteString -> Op
parseOp s =
  case B.words s of
    ["noop"] -> Noop
    ["addx", x] -> Addx (readInt x)
    _ -> error $ "invalid input: " <> B.unpack s
