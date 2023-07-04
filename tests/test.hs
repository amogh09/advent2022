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
    testCase "Day 4 Part 2 real" $ solve "day4" "part2" "real" >>= (@?= "849")
  ]
