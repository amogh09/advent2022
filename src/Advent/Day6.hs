module Advent.Day6 (solve1, solve2) where

import Advent.Util (bshow)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable (toList)
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

solve1 :: B.ByteString -> B.ByteString
solve1 = solve 4

solve2 :: B.ByteString -> B.ByteString
solve2 = solve 14

solve :: Int -> B.ByteString -> B.ByteString
solve len s = do
  let (candidate, rest) = splitAt len . B.unpack $ s
  bshow . rec len (Seq.fromList candidate) 0 $ rest

rec :: Int -> Seq Char -> Int -> String -> Int
rec !len !candidate !i _ | Set.size (Set.fromList . toList $ candidate) == len = i + len
rec !len (_ :<| cs) !i (x : xs) = rec len (cs :|> x) (i + 1) xs
rec _ _ _ _ = 0
