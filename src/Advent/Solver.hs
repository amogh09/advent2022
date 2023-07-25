module Advent.Solver (solve) where

import qualified Advent.Day1 as Day1
import qualified Advent.Day10 as Day10
import qualified Advent.Day11 as Day11
import qualified Advent.Day12 as Day12
import qualified Advent.Day13 as Day13
import qualified Advent.Day2 as Day2
import qualified Advent.Day3 as Day3
import qualified Advent.Day4 as Day4
import qualified Advent.Day5 as Day5
import qualified Advent.Day6 as Day6
import qualified Advent.Day7 as Day7
import qualified Advent.Day8 as Day8
import qualified Advent.Day9 as Day9
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
table "day5" "part2" = Day5.solve2
table "day6" "part1" = Day6.solve1
table "day6" "part2" = Day6.solve2
table "day7" "part1" = Day7.solve1
table "day7" "part2" = Day7.solve2
table "day8" "part1" = Day8.solve1
table "day8" "part2" = Day8.solve2
table "day9" "part1" = Day9.solve1
table "day9" "part2" = Day9.solve2
table "day10" "part1" = Day10.solve1
table "day10" "part2" = Day10.solve2
table "day11" "part1" = Day11.solve1
table "day11" "part2" = Day11.solve2
table "day12" "part1" = Day12.solve1
table "day12" "part2" = Day12.solve2
table "day13" "part1" = Day13.solve1
table "day13" "part2" = Day13.solve2
table _ _ = error "bad input: day and part"
