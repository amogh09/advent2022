module Advent.Day15 (solve1, solve2) where

import Advent.Util (bshow, pairs, uniq)
import Control.Monad (liftM2)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl', sortOn)
import Data.Maybe (catMaybes, mapMaybe)

{- ORMOLU_DISABLE -}
type Coordinates = (Int, Int)
type Interval = (Int, Int)
type SourceBeacon = (Coordinates, Coordinates)
{- ORMOLU_ENABLE -}

parseSourceBeacon :: ByteString -> SourceBeacon
parseSourceBeacon s = do
  let ws = B.words s
  ( (parseCoord $ ws !! 2, parseCoord $ ws !! 3),
    (parseCoord $ ws !! 8, parseCoord $ ws !! 9)
    )
  where
    parseCoord = readInt . B.drop 2
    readInt = maybe (error "invalid input") fst . B.readInt

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = reverse . foldl' f [] . sortOn fst
  where
    f [] i = [i]
    f (i1@(s1, e1) : is) i2@(s2, e2)
      | s2 <= e1 = (s1, max e1 e2) : is
      | otherwise = i2 : i1 : is

intervalLength :: Interval -> Int
intervalLength (s, e) = e - s + 1

inInterval :: Int -> Interval -> Bool
inInterval x (x1, x2) = x >= x1 && x <= x2

noBeaconIntervals :: Int -> [SourceBeacon] -> [Interval]
noBeaconIntervals y = mergeIntervals . mapMaybe (noBeaconInterval y)

noBeaconInterval :: Int -> SourceBeacon -> Maybe Interval
noBeaconInterval y (src@(sx, sy), beacon) = do
  let d = dist src beacon
      dy = abs (y - sy)
  if dy > d then Nothing else Just (sx - d + dy, sx + d - dy)

dist :: Coordinates -> Coordinates -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Returns y-intercepts of the diamond's sides in pairs.
-- The first pair contains intercepts with positive slopes and the second contains
-- intercepts with negative slopes.
yIntercepts :: SourceBeacon -> ([Int], [Int])
yIntercepts (src@(sx, sy), beacon) = do
  let d = dist src beacon
  ([sy - (sx - d), sy - (sx + d)], [sy + (sx - d), sy + (sx + d)])

liesOn :: SourceBeacon -> Coordinates -> Bool
liesOn (s, b) c = dist s b == dist s c

covers :: SourceBeacon -> Coordinates -> Bool
covers (s, b) c = dist s b >= dist s c

neighbors :: Coordinates -> [Coordinates]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- >>> intersections ((0,0), (2,0)) ((5,0), (0,0))
-- [(1,-1),(1,1)]
intersections :: SourceBeacon -> SourceBeacon -> [Coordinates]
intersections sb1 sb2 = do
  let (pos1, neg1) = yIntercepts sb1
      (pos2, neg2) = yIntercepts sb2
  uniq
    . filter (liesOn sb2)
    . filter (liesOn sb1)
    . catMaybes
    $ liftM2 intersection pos1 neg2 <> liftM2 intersection pos2 neg1
  where
    intersection :: Int -> Int -> Maybe Coordinates
    intersection c1 c2 = do
      case (c2 - c1) `divMod` 2 of
        (q, 0) -> Just (q, q + c1)
        _ -> Nothing

solve1 :: ByteString -> ByteString
solve1 s = do
  let srcBeacons = fmap parseSourceBeacon . B.lines $ s
      y = 2000000
      intervals = noBeaconIntervals y srcBeacons
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

inBounds :: Int -> Int -> Coordinates -> Bool
inBounds start end (x, y) = x >= start && x <= end && y >= start && y <= end

solve2 :: ByteString -> ByteString
solve2 s = do
  let srcBeacons = fmap parseSourceBeacon . B.lines $ s
      (start, end) = (0, 4000000)
  bshow
    . freq
    . head
    . filter (inBounds start end)
    . filter (\c -> not (any (`covers` c) srcBeacons))
    . concatMap neighbors
    . concatMap (uncurry intersections)
    . pairs
    $ srcBeacons

freq :: Num a => (a, a) -> a
freq (x, y) = x * 4000000 + y
