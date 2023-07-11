import Advent.Solver (solve)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Advent of Code 2022" tests

tests :: [TestTree]
tests =
  [ testCase "Day 1 Part 1 example" $ solve "day1" "part1" "example" >>= (@?= "24000"),
    testCase "Day 1 Part 1 real" $ solve "day1" "part1" "real" >>= (@?= "67450"),
    testCase "Day 1 Part 2 example" $ solve "day1" "part2" "example" >>= (@?= "45000"),
    testCase "Day 1 Part 2 real" $ solve "day1" "part2" "real" >>= (@?= "199357"),
    testCase "Day 2 Part 1 example" $ solve "day2" "part1" "example" >>= (@?= "15"),
    testCase "Day 2 Part 1 real" $ solve "day2" "part1" "real" >>= (@?= "8890"),
    testCase "Day 2 Part 2 example" $ solve "day2" "part2" "example" >>= (@?= "12"),
    testCase "Day 2 Part 2 real" $ solve "day2" "part2" "real" >>= (@?= "10238"),
    testCase "Day 3 Part 1 example" $ solve "day3" "part1" "example" >>= (@?= "157"),
    testCase "Day 3 Part 1 real" $ solve "day3" "part1" "real" >>= (@?= "7917"),
    testCase "Day 3 Part 2 example" $ solve "day3" "part2" "example" >>= (@?= "70"),
    testCase "Day 3 Part 2 real" $ solve "day3" "part2" "real" >>= (@?= "2585"),
    testCase "Day 4 Part 1 example" $ solve "day4" "part1" "example" >>= (@?= "2"),
    testCase "Day 4 Part 1 real" $ solve "day4" "part1" "real" >>= (@?= "487"),
    testCase "Day 4 Part 2 example" $ solve "day4" "part2" "example" >>= (@?= "4"),
    testCase "Day 4 Part 2 real" $ solve "day4" "part2" "real" >>= (@?= "849"),
    testCase "Day 5 Part 1 example" $ solve "day5" "part1" "example" >>= (@?= "CMZ"),
    testCase "Day 5 Part 1 real" $ solve "day5" "part1" "real" >>= (@?= "JRVNHHCSJ"),
    testCase "Day 5 Part 2 example" $ solve "day5" "part2" "example" >>= (@?= "MCD"),
    testCase "Day 5 Part 2 real" $ solve "day5" "part2" "real" >>= (@?= "GNFBSBJLH"),
    testCase "Day 6 Part 1 example" $ solve "day6" "part1" "example" >>= (@?= "7"),
    testCase "Day 6 Part 1 real" $ solve "day6" "part1" "real" >>= (@?= "1356"),
    testCase "Day 6 Part 2 example" $ solve "day6" "part2" "example" >>= (@?= "19"),
    testCase "Day 6 Part 2 real" $ solve "day6" "part2" "real" >>= (@?= "2564"),
    testCase "Day 7 Part 1 example" $ solve "day7" "part1" "example" >>= (@?= "95437"),
    testCase "Day 7 Part 1 real" $ solve "day7" "part1" "real" >>= (@?= "1723892"),
    testCase "Day 7 Part 2 example" $ solve "day7" "part2" "example" >>= (@?= "24933642"),
    testCase "Day 7 Part 2 real" $ solve "day7" "part2" "real" >>= (@?= "8474158"),
    testCase "Day 8 Part 1 example" $ solve "day8" "part1" "example" >>= (@?= "21"),
    testCase "Day 8 Part 1 real" $ solve "day8" "part1" "real" >>= (@?= "1843"),
    testCase "Day 8 Part 2 example" $ solve "day8" "part2" "example" >>= (@?= "8"),
    testCase "Day 8 Part 2 real" $ solve "day8" "part2" "real" >>= (@?= "180000"),
    testCase "Day 9 Part 1 example" $ solve "day9" "part1" "example" >>= (@?= "13"),
    testCase "Day 9 Part 1 real" $ solve "day9" "part1" "real" >>= (@?= "6503"),
    testCase "Day 9 Part 2 example" $ solve "day9" "part2" "example" >>= (@?= "1"),
    testCase "Day 9 Part 2 real" $ solve "day9" "part2" "real" >>= (@?= "2724")
  ]
