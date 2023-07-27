module Advent.Day14 (solve1) where

import Advent.Util (bshow, readInt)
import Control.Monad ((<=<))
import Data.Bifunctor (bimap, second)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (mapMaybe)

type Coordinates = (Int, Int)

data Barrier = LineSegment Coordinates Coordinates | Point Coordinates
  deriving (Show)

newtype VerticalLine = VerticalLine Coordinates

newtype Particle = Particle Coordinates

-- | Intersection of a vertical line with a barrier.
intersection :: VerticalLine -> Barrier -> Maybe Coordinates
intersection (VerticalLine (x, y)) ls@(LineSegment (x1, y1) (x2, y2))
  | x == x1 && x1 == x2 && y <= min y1 y2 = Just (x1, min y1 y2)
  | x1 == x2 = Nothing
  | otherwise = case ((y2 - y1) * (x - x1)) `divMod` (x2 - x1) of
      (q, 0) -> if y <= q + y1 && (x, q + y1) `liesOn` ls then Just (x, q + y1) else Nothing
      _ -> Nothing
intersection (VerticalLine (x, y)) (Point (x', y'))
  | x == x' && y <= y' = Just (x', y')
  | otherwise = Nothing

-- | Whether the given coordinates lie on the barrier.
liesOn :: Coordinates -> Barrier -> Bool
liesOn (x, y) (Point (x', y')) = x == x' && y == y'
liesOn (x, y) (LineSegment (x1, y1) (x2, y2)) =
  let (x', y') = (x - x1, y - y1)
      (x2', y2') = (x2 - x1, y2 - y1)
   in x' * y2' - y' * x2' == 0
        && abs (x - x1) + abs (x - x2) == abs (x2 - x1)
        && abs (y - y1) + abs (y - y2) == abs (y2 - y1)

-- | Given a list of barriers, find the final resting point of sand particle.
--   Returns Nothing if the particle lies on a barrier or interacts with no barriers.
--
--   >>> restPoint (Particle (500,0)) (parseRocks "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
restPoint :: Particle -> [Barrier] -> Maybe Coordinates
restPoint (Particle p) bs
  | any (p `liesOn`) bs = Nothing
  | otherwise = case fmap (second pred) . mapMaybe (intersection $ VerticalLine p) $ bs of
      [] -> Nothing
      is -> do
        let (x, y) = minimum is
        case (any ((x - 1, y + 1) `liesOn`) bs, any ((x + 1, y + 1) `liesOn`) bs) of
          (True, True) -> Just (x, y)
          (True, False) -> restPoint (Particle (x + 1, y + 1)) bs
          (False, _) -> restPoint (Particle (x - 1, y + 1)) bs

parseRock :: ByteString -> [Barrier]
parseRock s =
  let xs = fmap snd . filter (even . fst) . zip [0 :: Int, 1 ..] . B.words $ s
   in (parseLineSegment <$> zip xs (tail xs))

parseLineSegment :: (ByteString, ByteString) -> Barrier
parseLineSegment (s, s') = LineSegment (parseCoordinates s) (parseCoordinates s')

parseCoordinates :: ByteString -> Coordinates
parseCoordinates = bimap readInt (readInt . B.tail) . B.break (== ',')

parseRocks :: ByteString -> [Barrier]
parseRocks = parseRock <=< B.lines

-- >>> simulate $ parseRocks "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
simulate :: [Barrier] -> [Barrier]
simulate bs = case restPoint (Particle (500, 0)) bs of
  Just c -> simulate $ Point c : bs
  Nothing -> bs

solve1 :: ByteString -> ByteString
solve1 s =
  let rocks = parseRocks s
      after = simulate rocks
   in bshow $ length after - length rocks
