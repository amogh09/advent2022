module Advent.Solver (solve) where

import qualified Advent.Day1 as Day1
import qualified Advent.Day10 as Day10
import qualified Advent.Day11 as Day11
import qualified Advent.Day12 as Day12
import qualified Advent.Day13 as Day13
import qualified Advent.Day14 as Day14
import qualified Advent.Day15 as Day15
import qualified Advent.Day16 as Day16
import qualified Advent.Day17 as Day17
import qualified Advent.Day18 as Day18
import qualified Advent.Day19 as Day19
import qualified Advent.Day2 as Day2
import qualified Advent.Day20 as Day20
import qualified Advent.Day21 as Day21
import qualified Advent.Day22 as Day22
import qualified Advent.Day23 as Day23
import qualified Advent.Day24 as Day24
import qualified Advent.Day25 as Day25
import qualified Advent.Day3 as Day3
import qualified Advent.Day4 as Day4
import qualified Advent.Day5 as Day5
import qualified Advent.Day6 as Day6
import qualified Advent.Day7 as Day7
import qualified Advent.Day8 as Day8
import qualified Advent.Day9 as Day9
import qualified Advent.Y2023.Day1 as Y2023.Day1
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath ((</>))

solve :: String -> String -> String -> String -> IO ByteString
solve year day part inputType = do
  let inputFile = case inputType of
        "real" -> "inputs" </> year </> day <> ".txt"
        "example" -> "inputs" </> year </> day <> ".example.txt"
        _ -> error "bad input type"
  table year day part <$> B.readFile inputFile

table :: String -> String -> String -> ByteString -> ByteString
table "2022" "day1" "part1" = Day1.solve1
table "2022" "day1" "part2" = Day1.solve2
table "2022" "day2" "part1" = Day2.solve1
table "2022" "day2" "part2" = Day2.solve2
table "2022" "day3" "part1" = Day3.solve1
table "2022" "day3" "part2" = Day3.solve2
table "2022" "day4" "part1" = Day4.solve1
table "2022" "day4" "part2" = Day4.solve2
table "2022" "day5" "part1" = Day5.solve1
table "2022" "day5" "part2" = Day5.solve2
table "2022" "day6" "part1" = Day6.solve1
table "2022" "day6" "part2" = Day6.solve2
table "2022" "day7" "part1" = Day7.solve1
table "2022" "day7" "part2" = Day7.solve2
table "2022" "day8" "part1" = Day8.solve1
table "2022" "day8" "part2" = Day8.solve2
table "2022" "day9" "part1" = Day9.solve1
table "2022" "day9" "part2" = Day9.solve2
table "2022" "day10" "part1" = Day10.solve1
table "2022" "day10" "part2" = Day10.solve2
table "2022" "day11" "part1" = Day11.solve1
table "2022" "day11" "part2" = Day11.solve2
table "2022" "day12" "part1" = Day12.solve1
table "2022" "day12" "part2" = Day12.solve2
table "2022" "day13" "part1" = Day13.solve1
table "2022" "day13" "part2" = Day13.solve2
table "2022" "day14" "part1" = Day14.solve1
table "2022" "day14" "part2" = Day14.solve2
table "2022" "day15" "part1" = Day15.solve1
table "2022" "day15" "part2" = Day15.solve2
table "2022" "day16" "part1" = Day16.solve1
table "2022" "day16" "part2" = Day16.solve2
table "2022" "day17" "part1" = Day17.solve1
table "2022" "day17" "part2" = Day17.solve2
table "2022" "day18" "part1" = Day18.solve1
table "2022" "day18" "part2" = Day18.solve2
table "2022" "day19" "part1" = Day19.solve1
table "2022" "day19" "part2" = Day19.solve2
table "2022" "day20" "part1" = Day20.solve1
table "2022" "day20" "part2" = Day20.solve2
table "2022" "day21" "part1" = Day21.solve1
table "2022" "day21" "part2" = Day21.solve2
table "2022" "day22" "part1" = Day22.solve1
table "2022" "day22" "part2" = Day22.solve2
table "2022" "day23" "part1" = Day23.solve1
table "2022" "day23" "part2" = Day23.solve2
table "2022" "day24" "part1" = Day24.solve1
table "2022" "day24" "part2" = Day24.solve2
table "2022" "day25" "part1" = Day25.solve1
table "2023" "day1" "part1" = Y2023.Day1.solve1
table "2023" "day1" "part2" = Y2023.Day1.solve2
table _ _ _ = error "bad input: day and part"
