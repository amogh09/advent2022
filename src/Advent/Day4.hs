module Advent.Day4 (solve1, solve2) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

solve1 :: ByteString -> ByteString
solve1 = solve (fromEnum . uncurry anyIncludes . parsePairs)

solve2 :: ByteString -> ByteString
solve2 = solve (fromEnum . uncurry overlaps . parsePairs)

solve :: (Show b, Num b) => (ByteString -> b) -> ByteString -> ByteString
solve f = B.pack . show . sum . fmap f . B.lines

type Sections = (Int, Int)

parsePairs :: ByteString -> (Sections, Sections)
parsePairs s =
  case B.split ',' s of
    [x, y] -> (parseSections x, parseSections y)
    _ -> error "invalid input: not exactly two pairs on a line"

parseSections :: ByteString -> Sections
parseSections s =
  case B.split '-' s of
    [x, y] -> (readInt x, readInt y)
    _ -> error $ "invalid input: sections format invalid: " <> B.unpack s

readInt :: ByteString -> Int
readInt s =
  case B.readInt s of
    Just (x, s') | B.null s' -> x
    _ -> error $ "invalid input: failed to parse to int: " <> B.unpack s

anyIncludes :: Sections -> Sections -> Bool
anyIncludes s s' = s `includes` s' || s' `includes` s

includes :: Sections -> Sections -> Bool
includes (x, y) (x', y') = (max x x', min y y') == (x', y')

overlaps :: Sections -> Sections -> Bool
overlaps (x, y) (x', y') = max x x' <= min y y'
