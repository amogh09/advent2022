module Main where

import qualified Advent.Day1 as Day1
import qualified Advent.Day2 as Day2
import qualified Advent.Day3 as Day3
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)

main :: IO ()
main = do
  [day, part, inputType] <- getArgs
  let inputFile = case inputType of
        "real" -> "inputs/" <> day <> ".txt"
        "example" -> "inputs/" <> day <> ".example.txt"
        _ -> error "bad input type"
  B.readFile inputFile >>= B.putStrLn . solve day part

solve :: String -> String -> ByteString -> ByteString
solve "day1" "part1" = Day1.solve
solve "day1" "part2" = Day1.solve2
solve "day2" "part1" = Day2.solve
solve "day2" "part2" = Day2.solve2
solve "day3" "part1" = Day3.solve
solve "day3" "part2" = Day3.solve2
solve _ _ = error "bad input day and part"
