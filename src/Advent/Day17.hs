module Advent.Day17 (solve1, solve2) where

import Advent.Util (bshow, interleave, maximumMaybe)
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

{- ORMOLU_DISABLE -}
type Coordinates = (Int,Int)
type Moves = [Move]
type Rectangle = (Coordinates, Coordinates)
{- ORMOLU_ENABLE -}

data Rock
  = Horizontal {horizLeftEdge :: Coordinates}
  | Plus {plusCenter :: Coordinates}
  | LMirror {lmirrorBottomRight :: Coordinates}
  | Vertical {vertBottomEdge :: Coordinates}
  | Square {sqTopLeft :: Coordinates}
  deriving (Eq, Show)

data Chamber = Chamber
  { chamberLeftEdge :: Int,
    chamberRightEdge :: Int,
    chamberFloor :: Int,
    chamberRocks :: Map Coordinates [Rock],
    chamberHeighestPoint :: Int
  }

data Move = Left | Right | Down

simulateRock :: Moves -> Rock -> Chamber -> (Chamber, Moves)
simulateRock (m : ms) r c = do
  case (m, movedRock) of
    (Down, r') | r == r' -> (addRock c r', ms)
    (_, r') -> simulateRock ms r' c
  where
    movedRock = let r' = moveRock r m in if collides r' c then r else r'
simulateRock _ _ _ = error "ran out of moves"

simulateRocks :: Moves -> [Chamber]
simulateRocks ms = do
  let moves = interleave (cycle ms) (repeat Down)
      og = (0, 0)
      rocks = cycle [Horizontal og, Plus og, LMirror og, Vertical og, Square og]
  fmap tup3Snd
    . iterate (\(r : rs, c, ms) -> addTuple rs $ simulateRock ms (newRock r c) c)
    $ (rocks, Chamber 0 8 0 Map.empty 0, moves)
  where
    addTuple x (y, z) = (x, y, z)
    tup3Snd (_, y, _) = y

    newRock :: Rock -> Chamber -> Rock
    newRock (Horizontal _) c = Horizontal (c.chamberLeftEdge + 3, c.chamberHeighestPoint + 4)
    newRock (Plus _) c = Plus (c.chamberLeftEdge + 4, c.chamberHeighestPoint + 5)
    newRock (LMirror _) c = LMirror (c.chamberLeftEdge + 5, c.chamberHeighestPoint + 4)
    newRock (Vertical _) c = Vertical (c.chamberLeftEdge + 3, c.chamberHeighestPoint + 4)
    newRock (Square _) c = Square (c.chamberLeftEdge + 3, c.chamberHeighestPoint + 4)

addRock :: Chamber -> Rock -> Chamber
addRock c r =
  c
    { chamberRocks =
        Map.unionWith
          (++)
          (Map.fromList . (`zip` repeat [r]) . rectPoints . boundingRect $ r)
          c.chamberRocks,
      chamberHeighestPoint = max c.chamberHeighestPoint (topMost r)
    }

moveRock :: Rock -> Move -> Rock
moveRock (Horizontal c) m = Horizontal (moveDelta m c)
moveRock (Plus c) m = Plus (moveDelta m c)
moveRock (LMirror c) m = LMirror (moveDelta m c)
moveRock (Vertical c) m = Vertical (moveDelta m c)
moveRock (Square c) m = Square (moveDelta m c)

moveDelta :: Move -> (Coordinates -> Coordinates)
moveDelta Left = first pred
moveDelta Right = first succ
moveDelta Down = second pred

collides :: Rock -> Chamber -> Bool
collides r c = do
  leftMost r <= c.chamberLeftEdge
    || rightMost r >= c.chamberRightEdge
    || bottomMost r <= c.chamberFloor
    || any
      (any (rockCollides r) . fromMaybe [] . flip Map.lookup c.chamberRocks)
      (rectPoints . boundingRect $ r)

rockCollides :: Rock -> Rock -> Bool
rockCollides r r' | not (boundingRect r `rectsIntersect` boundingRect r') = False
rockCollides r r' = not $ Set.fromList (allPoints r) `Set.disjoint` Set.fromList (allPoints r')

boundingRect :: Rock -> Rectangle
boundingRect r = ((leftMost r, bottomMost r), (rightMost r, topMost r))

rectPoints :: Rectangle -> [Coordinates]
rectPoints ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

rectsIntersect :: Rectangle -> Rectangle -> Bool
rectsIntersect ((r1x1, r1y1), (r1x2, r1y2)) ((r2x1, r2y1), (r2x2, r2y2)) =
  max r1x1 r2x1 <= min r1x2 r2x2 && max r1y1 r2y1 <= min r1y2 r2y2

topMost :: Rock -> Int
topMost (Horizontal (_, y)) = y
topMost (Plus (_, y)) = y + 1
topMost (LMirror (_, y)) = y + 2
topMost (Vertical (_, y)) = y + 3
topMost (Square (_, y)) = y + 1

leftMost :: Rock -> Int
leftMost (Horizontal (x, _)) = x
leftMost (Plus (x, _)) = x - 1
leftMost (LMirror (x, _)) = x - 2
leftMost (Vertical (x, _)) = x
leftMost (Square (x, _)) = x

rightMost :: Rock -> Int
rightMost (Horizontal (x, _)) = x + 3
rightMost (Plus (x, _)) = x + 1
rightMost (LMirror (x, _)) = x
rightMost (Vertical (x, _)) = x
rightMost (Square (x, _)) = x + 1

bottomMost :: Rock -> Int
bottomMost (Horizontal (_, y)) = y
bottomMost (Plus (_, y)) = y - 1
bottomMost (LMirror (_, y)) = y
bottomMost (Vertical (_, y)) = y
bottomMost (Square (_, y)) = y

allPoints :: Rock -> [Coordinates]
allPoints (Horizontal (x, y)) = [(x + x', y) | x' <- [0 .. 3]]
allPoints (Plus (x, y)) = [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
allPoints (LMirror (x, y)) = [(x, y), (x - 1, y), (x - 2, y), (x, y + 1), (x, y + 2)]
allPoints (Vertical (x, y)) = [(x, y + y') | y' <- [0 .. 3]]
allPoints (Square (x, y)) = [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)]

parseMoves :: B.ByteString -> [Move]
parseMoves = fmap parseMove . B.unpack
  where
    parseMove '<' = Left
    parseMove '>' = Right
    parseMove x = error $ "invalid move: " <> show x

solve :: Int -> B.ByteString -> B.ByteString
solve n = bshow . chamberHeighestPoint . (!! n) . simulateRocks . parseMoves

solve1 :: B.ByteString -> B.ByteString
solve1 = solve 2022

solve2 :: B.ByteString -> B.ByteString
solve2 = solve 1000000000000
