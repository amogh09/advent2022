module Advent.Solver (solve) where

import qualified Advent.Day1 as Day1
import qualified Advent.Day2 as Day2
import qualified Advent.Day3 as Day3
import qualified Advent.Day4 as Day4
import qualified Advent.Day5 as Day5
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

solve :: String -> String -> String -> IO ByteString
solve day part inputType = do
  let inputFile = case inputType of
        "real" -> "inputs/" <> day <> ".txt"
        "example" -> "inputs/" <> day <> ".example.txt"
        _ -> error "bad input type"
  table day part <$> B.readFile inputFile

table :: String -> String -> ByteString -> ByteString
table "day1" "part1" = Day1.solve1
table "day1" "part2" = Day1.solve2
table "day2" "part1" = Day2.solve1
table "day2" "part2" = Day2.solve2
table "day3" "part1" = Day3.solve1
table "day3" "part2" = Day3.solve2
table "day4" "part1" = Day4.solve1
table "day4" "part2" = Day4.solve2
table "day5" "part1" = Day5.solve1
table _ _ = error "bad input: day and part"