import Advent.Solver (solve)
import qualified Data.ByteString.Lazy.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Advent of Code 2022" tests

tests :: [TestTree]
tests =
  [ tc "day1" "part1" "example" "24000",
    tc "day1" "part1" "real" "67450",
    tc "day1" "part2" "example" "45000",
    tc "day1" "part2" "real" "199357",
    tc "day2" "part1" "example" "15",
    tc "day2" "part1" "real" "8890",
    tc "day2" "part2" "example" "12",
    tc "day2" "part2" "real" "10238",
    tc "day3" "part1" "example" "157",
    tc "day3" "part1" "real" "7917",
    tc "day3" "part2" "example" "70",
    tc "day3" "part2" "real" "2585",
    tc "day4" "part1" "example" "2",
    tc "day4" "part1" "real" "487",
    tc "day4" "part2" "example" "4",
    tc "day4" "part2" "real" "849",
    tc "day5" "part1" "example" "CMZ",
    tc "day5" "part1" "real" "JRVNHHCSJ",
    tc "day5" "part2" "example" "MCD",
    tc "day5" "part2" "real" "GNFBSBJLH",
    tc "day6" "part1" "example" "7",
    tc "day6" "part1" "real" "1356",
    tc "day6" "part2" "example" "19",
    tc "day6" "part2" "real" "2564",
    tc "day7" "part1" "example" "95437",
    tc "day7" "part1" "real" "1723892",
    tc "day7" "part2" "example" "24933642",
    tc "day7" "part2" "real" "8474158",
    tc "day8" "part1" "example" "21",
    tc "day8" "part1" "real" "1843",
    tc "day8" "part2" "example" "8",
    tc "day8" "part2" "real" "180000",
    tc "day9" "part1" "example" "13",
    tc "day9" "part1" "real" "6503",
    tc "day9" "part2" "example" "1",
    tc "day9" "part2" "real" "2724",
    tc "day10" "part1" "example" "13140",
    tc "day10" "part1" "real" "14820",
    tc "day10" "part2" "example" day10Part2ExampleResult,
    tc "day10" "part2" "real" day10Part2Result,
    tc "day11" "part1" "example" "10605",
    tc "day11" "part1" "real" "101436",
    tc "day11" "part2" "example" "2713310158",
    tc "day11" "part2" "real" "19754471646",
    tc "day12" "part1" "example" "31",
    tc "day12" "part1" "real" "423",
    tc "day12" "part2" "example" "29",
    tc "day12" "part2" "real" "416",
    tc "day13" "part1" "example" "13",
    tc "day13" "part1" "real" "6623",
    tc "day13" "part2" "example" "140",
    tc "day13" "part2" "real" "23049",
    tc "day14" "part1" "example" "24",
    tc "day14" "part1" "real" "1016",
    tc "day14" "part2" "example" "93",
    tc "day14" "part2" "real" "25402",
    tc "day15" "part1" "real" "4748135",
    tc "day15" "part2" "real" "13743542639657",
    tc "day16" "part1" "example" "1651",
    tc "day16" "part1" "real" "2181",
    tc "day16" "part2" "example" "1707",
    tc "day16" "part2" "real" "2824",
    tc "day17" "part1" "example" "3068",
    tc "day17" "part1" "real" "3211",
    tc "day17" "part2" "example" "1514285714288",
    tc "day17" "part2" "real" "1589142857183",
    tc "day18" "part1" "example" "64",
    tc "day18" "part1" "real" "4400",
    tc "day18" "part2" "example" "58",
    tc "day18" "part2" "real" "2522",
    tc "day19" "part1" "example" "33",
    tc "day19" "part1" "real" "1834",
    tc "day19" "part2" "example" "3472",
    tc "day19" "part2" "real" "2240",
    tc "day20" "part1" "example" "3",
    tc "day20" "part1" "real" "4267",
    tc "day20" "part2" "example" "1623178306",
    tc "day20" "part2" "real" "6871725358451",
    tc "day21" "part1" "example" "152",
    tc "day21" "part1" "real" "72664227897438",
    tc "day21" "part2" "example" "301",
    tc "day21" "part2" "real" "3916491093817",
    tc "day22" "part1" "example" "6032",
    tc "day22" "part1" "real" "36518",
    tc "day22" "part2" "real" "143208",
    tc "day23" "part1" "example" "110",
    tc "day23" "part1" "real" "3990",
    tc "day23" "part2" "example" "20",
    tc "day23" "part2" "real" "1057"
  ]
  where
    day10Part2ExampleResult =
      B.unlines
        [ "##..##..##..##..##..##..##..##..##..##..",
          "###...###...###...###...###...###...###.",
          "####....####....####....####....####....",
          "#####.....#####.....#####.....#####.....",
          "######......######......######......####",
          "#######.......#######.......#######....."
        ]
    day10Part2Result =
      B.unlines
        [ "###..####.####.#..#.####.####.#..#..##..",
          "#..#....#.#....#.#..#....#....#..#.#..#.",
          "#..#...#..###..##...###..###..####.#..#.",
          "###...#...#....#.#..#....#....#..#.####.",
          "#.#..#....#....#.#..#....#....#..#.#..#.",
          "#..#.####.####.#..#.####.#....#..#.#..#."
        ]

    tc day part input expected =
      testCase (unwords [day, part, input]) $ solve day part input >>= (@?= expected)
