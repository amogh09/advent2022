module Advent.Day9 (solve1) where

import Advent.Util (bshow, readInt)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

type Position = (Int, Int)

type Rope = (Position, Position)

data Direction = Up | Down | Left | Right

data Move = Move Direction Int

-- >>> solve1 "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n"
solve1 :: ByteString -> ByteString
solve1 =
  bshow
    . length
    . Set.toList
    . Set.fromList
    . fmap snd
    . scanl moveDir ((0, 0), (0, 0))
    . parseDirections

parseDirections :: ByteString -> [Direction]
parseDirections = concatMap parseDirection . B.lines

parseDirection :: ByteString -> [Direction]
parseDirection s =
  case B.words s of
    ["R", v] -> replicate (readInt v) Right
    ["L", v] -> replicate (readInt v) Left
    ["U", v] -> replicate (readInt v) Up
    ["D", v] -> replicate (readInt v) Down
    _ -> error $ "invalid moves input: " <> B.unpack s

parseMoves :: ByteString -> [Move]
parseMoves = fmap parseMove . B.lines

parseMove :: ByteString -> Move
parseMove s =
  case B.words s of
    ["R", x] -> Move Right . readInt $ x
    ["L", x] -> Move Left . readInt $ x
    ["U", y] -> Move Up . readInt $ y
    ["D", y] -> Move Down . readInt $ y
    _ -> error $ "invalid moves input: " <> B.unpack s

moveDir :: Rope -> Direction -> Rope
moveDir (h, t) d = do
  let h' = movePosition h (Move d 1)
  if dist t h' > 1 then (h', h) else (h', t)

-- >>> move ((0,0), (0,0)) (parseMove "R 5")
-- ((5,0),(4,0))
-- >>> move ((0,1), (0,0)) (parseMove "R 1")
-- ((1,1),(0,0))
-- >>> move ((1,1), (0,0)) (parseMove "D 2")
-- ((1,-1),(0,0))
-- >>> move ((1,1), (0,0)) (parseMove "R 1")
-- ((2,1),(1,1))
move :: Rope -> Move -> Rope
move (h, t) m = do
  let h' = movePosition h m
  if dist t h' > 1 then (h', movePosition h' $ setMove m (-1)) else (h', t)

movePosition :: Position -> Move -> Position
movePosition (x, y) (Move Up y') = (x, y + y')
movePosition (x, y) (Move Down y') = (x, y - y')
movePosition (x, y) (Move Right x') = (x + x', y)
movePosition (x, y) (Move Left x') = (x - x', y)

dist :: Position -> Position -> Int
dist (x, y) (x', y') = abs (x - x') `max` abs (y - y')

setMove :: Move -> Int -> Move
setMove (Move d _) = Move d
