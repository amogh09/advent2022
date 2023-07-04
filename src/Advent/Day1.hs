module Advent.Day1 (solve1, solve2) where

import Advent.Util (readInt, splitAtEmptyLines)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (sortOn)

solve1 :: ByteString -> ByteString
solve1 = B.pack . show . maximum . calories

solve2 :: ByteString -> ByteString
solve2 = B.pack . show . sum . take 3 . sortOn negate . calories

calories :: ByteString -> [Int]
calories = fmap (sum . fmap readInt) . splitAtEmptyLines
