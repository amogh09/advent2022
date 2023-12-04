module Advent.Y2023.Day1 (solve1, solve2) where

import Advent.Util (bshow)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (inits, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type DigMap = Map String Int

digits :: DigMap -> String -> [Int]
digits digs s = tails s >>= (mapMaybe (isDig digs) . inits)

isDig :: DigMap -> String -> Maybe Int
isDig digs s = Map.lookup s digs

letterDigs :: DigMap
letterDigs =
  Map.fromList
    [ ("0", 0),
      ("1", 1),
      ("2", 2),
      ("3", 3),
      ("4", 4),
      ("5", 5),
      ("6", 6),
      ("7", 7),
      ("8", 8),
      ("9", 9)
    ]

wordDigs :: DigMap
wordDigs =
  Map.fromList
    [ ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    ]

calibration :: DigMap -> String -> Int
calibration dm s =
  let ds = digits dm s
   in head ds * 10 + last ds

solve :: DigMap -> ByteString -> ByteString
solve digMap = bshow . sum . fmap (calibration digMap . B.unpack) . B.lines

solve1 :: ByteString -> ByteString
solve1 = solve letterDigs

solve2 :: ByteString -> ByteString
solve2 = solve (letterDigs `Map.union` wordDigs)
