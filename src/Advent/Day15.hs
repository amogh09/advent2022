module Advent.Day15 (solve1) where

import Advent.Util (bshow, uniq)
import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)

{- ORMOLU_DISABLE -}
type Coordinates = (Int, Int)
type Parallelogram = (LineSegment, LineSegment, LineSegment, LineSegment)
type LineSegment = (Coordinates, Coordinates)
type Interval = (Int, Int)
{- ORMOLU_ENABLE -}

sbParalellogram :: Coordinates -> Coordinates -> Parallelogram
sbParalellogram (sx, sy) (bx, by) =
  ( ((sx - d, sy), (sx, sy + d)),
    ((sx + d, sy), (sx, sy + d)),
    ((sx - d, sy), (sx, sy - d)),
    ((sx + d, sy), (sx, sy - d))
  )
  where
    d = abs (sx - bx) + abs (sy - by)

parseSourceBeacon :: ByteString -> (Coordinates, Coordinates)
parseSourceBeacon s = do
  let ws = B.words s
  ( (parseCoord $ ws !! 2, parseCoord $ ws !! 3),
    (parseCoord $ ws !! 8, parseCoord $ ws !! 9)
    )
  where
    parseCoord = readInt . B.drop 2
    readInt x = case B.readInt x of
      Just (v, _) -> v
      Nothing -> error "invalid input"

-- | Intersection between a horizontal line and a parallelogram.
hlineParIntersect :: Int -> Parallelogram -> Maybe LineSegment
hlineParIntersect y (a, b, c, d) =
  ((,) <$> hlineIntersect y a <*> hlineIntersect y b)
    <|> ((,) <$> hlineIntersect y c <*> hlineIntersect y d)

-- | Intersection between a horizontal line and a line segment.
hlineIntersect :: Int -> LineSegment -> Maybe Coordinates
hlineIntersect y ls@((x1, y1), (x2, y2))
  | y1 == y2 = Nothing
  | otherwise =
      case ((y - y1) * (x1 - x2)) `divMod` (y1 - y2) of
        (d, 0) | (d + x1, y) `liesBetween` ls -> Just (d + x1, y)
        _ -> Nothing

liesBetween :: Coordinates -> LineSegment -> Bool
liesBetween (x, y) ((x1, y1), (x2, y2)) =
  x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y2 && y <= max y1 y2

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = reverse . foldl' f [] . sortOn fst
  where
    f [] i = [i]
    f (i1@(s1, e1) : is) i2@(s2, e2)
      | s2 <= e1 = (s1, max e1 e2) : is
      | otherwise = i2 : i1 : is

intervalLength :: Interval -> Int
intervalLength (s, e) = e - s + 1

lineSegmentX :: LineSegment -> Interval
lineSegmentX = bimap fst fst

inInterval :: Int -> Interval -> Bool
inInterval x (x1, x2) = x >= x1 && x <= x2

solve1 :: ByteString -> ByteString
solve1 s = do
  let srcBeacons = fmap parseSourceBeacon . B.lines $ s
      parallelograms = uncurry sbParalellogram <$> srcBeacons
      y = 2000000
      intervals =
        mergeIntervals
          . fmap lineSegmentX
          . mapMaybe (hlineParIntersect y)
          $ parallelograms
      yBeaconsCount =
        length
          . fmap fst
          . filter (\c -> any (fst c `inInterval`) intervals)
          . filter ((== y) . snd)
          . uniq
          . fmap snd
          $ srcBeacons
      totalIntervalLength = sum . fmap intervalLength $ intervals
  bshow $ totalIntervalLength - yBeaconsCount
