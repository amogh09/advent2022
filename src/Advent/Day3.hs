module Advent.Day3 (solve, solve2) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isAlpha, isUpper, ord)
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import qualified Data.Set as S

solve :: ByteString -> ByteString
solve = B.pack . show . sum . fmap (sum . fmap priority . common . tupToList . halve) . B.lines

solve2 :: ByteString -> ByteString
solve2 = B.pack . show . sum . fmap (priority . head . common) . chunksOf 3 . B.lines

priority :: Char -> Int
priority c | not $ isAlpha c = error $ "non alpha character found: " <> [c]
priority c | isUpper c = ord c - ord 'A' + 1 + 26
priority c = ord c - ord 'a' + 1

tupToList :: (a, a) -> [a]
tupToList (x, y) = [x, y]

toSet :: ByteString -> S.Set Char
toSet = S.fromList . B.unpack

common :: [ByteString] -> [Char]
common = S.toList . foldl1' S.intersection . fmap toSet

halve :: ByteString -> (ByteString, ByteString)
halve bs = B.splitAt (B.length bs `div` 2) bs
