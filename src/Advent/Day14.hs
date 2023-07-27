module Advent.Day14 (solve1) where

import Advent.Util (bshow, readInt)
import Control.Monad ((<=<))
import Data.Bifunctor (bimap, second)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (mapMaybe)

{- ORMOLU_DISABLE -}
type Coordinates = (Int, Int)
data Barrier = LineSegment Coordinates Coordinates | Point Coordinates
newtype VerticalLine = VerticalLine Coordinates
newtype Particle = Particle Coordinates
{- ORMOLU_ENABLE -}

-- | Intersection of a vertical line with a barrier.
intersection :: VerticalLine -> Barrier -> Maybe Coordinates
intersection (VerticalLine (x, y)) (LineSegment (x1, y1) (x2, y2))
  | y >= min y1 y2 = Nothing
  | x1 == x2 && x /= x1 = Nothing
  | x1 == x2 = Just (x, min y1 y2)
  | x >= min x1 x2 && x <= max x1 x2 = Just (x, y1)
  | otherwise = Nothing
intersection (VerticalLine (x, y)) (Point (x', y'))
  | x == x' && y <= y' = Just (x', y')
  | otherwise = Nothing

-- | Whether the given coordinates lie on the barrier.
liesOn :: Coordinates -> Barrier -> Bool
liesOn (x, y) (Point (x', y')) = x == x' && y == y'
liesOn (x, y) (LineSegment (x1, y1) (x2, y2))
  | x1 == x2 = x == x1 && y >= min y1 y2 && y <= max y1 y2
  | otherwise = y == y1 && x >= min x1 x2 && x <= max x1 x2

-- | Given a list of barriers, find the final resting point of sand particle.
--   Returns Nothing if the particle lies on a barrier or interacts with no barriers.
restPoint :: Particle -> [Barrier] -> Maybe Coordinates
restPoint (Particle p) bs =
  case fmap (second pred) . mapMaybe (intersection $ VerticalLine p) $ bs of
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

simulate :: [Barrier] -> [Barrier]
simulate bs = case restPoint (Particle (500, 0)) bs of
  Just c -> simulate $ Point c : bs
  Nothing -> bs

solve1 :: ByteString -> ByteString
solve1 s =
  let rocks = parseRocks s
      after = simulate rocks
   in bshow $ length after - length rocks
