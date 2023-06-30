module Advent.Day2 (solve, solve2) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

solve :: ByteString -> ByteString
solve = solveAny table

solve2 :: ByteString -> ByteString
solve2 = solveAny table2

solveAny :: (ByteString -> Int) -> ByteString -> ByteString
solveAny f = B.pack . show . sum . fmap f . B.lines

table :: ByteString -> Int
table "A X" = rock + draw
table "A Y" = paper + win
table "A Z" = scissors + lose
table "B X" = rock + lose
table "B Y" = paper + draw
table "B Z" = scissors + win
table "C X" = rock + win
table "C Y" = paper + lose
table "C Z" = scissors + draw
table s = error $ "bad input: " <> B.unpack s

table2 :: ByteString -> Int
table2 "A X" = lose + scissors
table2 "A Y" = draw + rock
table2 "A Z" = win + paper
table2 "B X" = lose + rock
table2 "B Y" = draw + paper
table2 "B Z" = win + scissors
table2 "C X" = lose + paper
table2 "C Y" = draw + scissors
table2 "C Z" = win + rock
table2 s = error $ "bad input: " <> B.unpack s

lose :: Int
lose = 0

draw :: Int
draw = 3

win :: Int
win = 6

rock :: Int
rock = 1

paper :: Int
paper = 2

scissors :: Int
scissors = 3
