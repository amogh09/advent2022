module Advent.Day1 (solve, solve2) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (sortOn)
import Data.List.Split (splitWhen)

solve :: ByteString -> ByteString
solve = B.pack . show . maximum . calories

solve2 :: ByteString -> ByteString
solve2 = B.pack . show . sum . take 3 . sortOn negate . calories

calories :: ByteString -> [Int]
calories = fmap (sum . fmap read') . splitWhen B.null . B.lines

read' :: ByteString -> Int
read' s = case B.readInt s of
  Just (x, _) -> x
  Nothing -> error "invalid input, couldn't parse int"
