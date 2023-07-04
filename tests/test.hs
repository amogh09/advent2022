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
    testCase "Day 1 Part 2 real" $ solve "day1" "part2" "real" >>= (@?= "199357")
  ]
