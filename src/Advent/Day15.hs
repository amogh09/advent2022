module Advent.Day15 (solve1, solve2) where

import Advent.Util (bshow, uniq)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)

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
    readInt x = case B.readInt x of
      Just (v, _) -> v
      Nothing -> error "invalid input"

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
  if dy > d
    then Nothing
    else Just (sx - d + dy, sx + d - dy)
  where
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

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

solve2 :: ByteString -> ByteString
solve2 s = do
  let srcBeacons = fmap parseSourceBeacon . B.lines $ s
      (start, end) = (0, 4000000)
  bshow . freq . head . mapMaybe (f srcBeacons start end) $ [start .. end]
  where
    f srcBeacons start end y = do
      let intervals =
            takeWhile ((<= end) . fst)
              . dropWhile ((< start) . snd)
              . noBeaconIntervals y
              $ srcBeacons
      case intervals of
        [(_, ie), (is', _)] | ie + 2 == is' -> Just (ie + 1, y)
        [(is, _)] | is == start + 1 -> Just (start, y)
        [(_, ie)] | ie == end - 1 -> Just (end, y)
        _ -> Nothing

    freq (x, y) = x * 4000000 + y
