module Advent.Day22 (solve1) where

import Advent.Util (bshow, splitAtEmptyLines)
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Data.Array (Array)
import qualified Data.Array as Array
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Prelude hiding (Left, Right)

{- ORMOLU_DISABLE -}
data Move = Forward | Clockwise | CounterClockwise deriving (Show)
type Moves = [Move]
data Position = Position {posCoords :: (Int, Int), posFacing :: Facing} deriving (Show)
data Facing = Right | Down | Left | Up deriving (Ord, Enum, Eq, Show)
data Cell = Empty | Open | Wall deriving (Show, Eq)
type Grid = Array (Int, Int) Cell
{- ORMOLU_ENABLE -}

move :: MonadReader Grid m => Position -> Move -> m Position
move p Forward = Reader.asks (`moveForward` p)
move p Clockwise = pure $ p {posFacing = turnCW p.posFacing}
move p CounterClockwise = pure $ p {posFacing = turnCCW p.posFacing}

moveForward :: Grid -> Position -> Position
moveForward g p = do
  case head . tail . dropWhile ((/= p.posCoords) . fst) . cycle . cellsAlong p $ g of
    (_, Wall) -> p
    (nextCoords, _) -> p {posCoords = nextCoords}

cellsAlong :: Position -> Grid -> [((Int, Int), Cell)]
cellsAlong p g =
  case p.posFacing of
    Right -> filter ((/= Empty) . snd) . row (fst p.posCoords) $ g
    Left -> reverse . filter ((/= Empty) . snd) . row (fst p.posCoords) $ g
    Down -> filter ((/= Empty) . snd) . col (snd p.posCoords) $ g
    Up -> reverse . filter ((/= Empty) . snd) . col (snd p.posCoords) $ g

row :: Int -> Array (Int, Int) a -> [((Int, Int), a)]
row i g =
  [ ((i, j), g Array.! (i, j))
    | j <- [(snd . fst . Array.bounds $ g) .. (snd . snd . Array.bounds $ g)]
  ]

col :: Int -> Array (Int, Int) a -> [((Int, Int), a)]
col j g =
  [ ((i, j), g Array.! (i, j))
    | i <- [(fst . fst . Array.bounds $ g) .. (fst . snd . Array.bounds $ g)]
  ]

turnCW :: Facing -> Facing
turnCW Right = Down
turnCW Down = Left
turnCW Left = Up
turnCW Up = Right

turnCCW :: Facing -> Facing
turnCCW Right = Up
turnCCW Up = Left
turnCCW Left = Down
turnCCW Down = Right

parseMoves :: ByteString -> Moves
parseMoves s | B.null s = []
parseMoves s =
  case B.readInt s of
    Nothing -> parseTurn (B.head s) : parseMoves (B.tail s)
    Just (v, s') -> replicate v Forward ++ parseMoves s'

parseTurn :: Char -> Move
parseTurn 'R' = Clockwise
parseTurn 'L' = CounterClockwise
parseTurn c = error $ "invalid turn: " <> show c

parseGrid :: [ByteString] -> Grid
parseGrid ls = do
  let initArr = Array.listArray (lo, hi) $ repeat Empty
      updates = concatMap parseRow . zip [1, 2 ..] $ ls
  initArr Array.// updates
  where
    lo = (1, 1)
    hi = (length ls, maximum . fmap (length . B.unpack) $ ls)

    parseRow (i, r) = fmap parseCell . zip (repeat i `zip` [1, 2 ..]) . B.unpack $ r

    parseCell (i, ' ') = (i, Empty)
    parseCell (i, '.') = (i, Open)
    parseCell (i, '#') = (i, Wall)
    parseCell (_, c) = error $ "cannot parse " <> show c

parseInput :: ByteString -> (Grid, Moves)
parseInput s = do
  case splitAtEmptyLines s of
    [g, m] -> (parseGrid g, parseMoves . head $ m)
    ls -> error $ "invalid number of sections in input: " <> show (length ls)

solve1 :: ByteString -> ByteString
solve1 s = do
  let (grid, moves) = parseInput s
      start = fst . head . dropWhile ((== Empty) . snd) . Array.assocs $ grid
      p = Reader.runReader (foldM move (Position start Right) moves) grid
  bshow $ 1000 * fst p.posCoords + 4 * snd p.posCoords + fromEnum p.posFacing
