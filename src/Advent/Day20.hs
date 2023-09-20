module Advent.Day20 (solve1, solve2) where

import Advent.Util (bshow, readInt)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (delete, elemIndex, foldl')
import Data.Maybe (fromJust)

mix :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mix xs curr = foldl' f curr xs
  where
    f :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    f ns t@(_, n) = do
      let ns' = t `delete` ns
          i = fromJust $ elemIndex t ns
          j = (i + n) `mod` length ns'
      take j ns' ++ [t] ++ drop j ns'

compute :: (Eq a, Num a) => [a] -> a
compute mixed = do
  let i = fromJust $ elemIndex 0 mixed
  sum [mixed !! ((i + j) `mod` length mixed) | j <- [1000, 2000, 3000]]

solve1 :: B.ByteString -> B.ByteString
solve1 s = do
  let ns = zip [0, 1 ..] . fmap readInt . B.lines $ s
  bshow . compute . fmap snd . mix ns $ ns

solve2 :: B.ByteString -> B.ByteString
solve2 s = do
  let ns = zip [0, 1 ..] . fmap ((* 811589153) . readInt) . B.lines $ s
  bshow . compute . fmap snd . (!! 10) . iterate (mix ns) $ ns
