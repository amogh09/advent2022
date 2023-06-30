module Main where

import qualified Advent.Day1 as Day1
import qualified Advent.Day2 as Day2
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)

main :: IO ()
main = do
  [day, part, inputType] <- getArgs
  let inputFile = case inputType of
        "real" -> "inputs/" <> day <> ".txt"
        "example" -> "inputs/" <> day <> ".example.txt"
        _ -> error "bad input type"
  input <- B.readFile inputFile
  let res = case part of
        "1" -> Day2.solve input
        "2" -> Day2.solve2 input
        _ -> error "bad part"
  B.putStrLn res
