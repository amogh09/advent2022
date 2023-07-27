module Advent.Day14 (solve1) where

import Advent.Util (bshow, readInt)
import Data.Bifunctor (bimap, second)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

{- ORMOLU_DISABLE -}
type Coordinates = (Int, Int)
data Barrier = LineSegment Coordinates Coordinates | Point Coordinates deriving Show
newtype VerticalLine = VerticalLine Coordinates
newtype Particle = Particle Coordinates
data Barriers = Barriers {
      barriersHoriz :: Map Int [Barrier],
      barriersVert :: Map Int [Barrier]
  } deriving Show
{- ORMOLU_ENABLE -}

emptyBarriers :: Barriers
emptyBarriers = Barriers M.empty M.empty

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
restPoint :: Particle -> Barriers -> Maybe Coordinates
restPoint (Particle p@(px, py)) bsm = do
  case M.lookupGT py (barriersHoriz bsm) of
    Nothing -> Nothing
    Just (k, bs) -> do
      case fmap (second pred) . mapMaybe (intersection $ VerticalLine p) $ bs of
        [] -> restPoint (Particle (px, k)) bsm
        (x, y) : _ -> case (isBlocked (x - 1, y + 1) bsm, isBlocked (x + 1, y + 1) bsm) of
          (True, True) -> Just (x, y)
          (True, False) -> restPoint (Particle (x + 1, y + 1)) bsm
          (False, _) -> restPoint (Particle (x - 1, y + 1)) bsm

isBlocked :: Coordinates -> Barriers -> Bool
isBlocked c bs = isBlockedHoriz c bs || isBlockedVert c bs

isBlockedHoriz :: Coordinates -> Barriers -> Bool
isBlockedHoriz (x, y) = maybe False (any ((x, y) `liesOn`)) . M.lookup y . barriersHoriz

isBlockedVert :: Coordinates -> Barriers -> Bool
isBlockedVert (x, y) = maybe False (any ((x, y) `liesOn`)) . M.lookup x . barriersVert

parseRock :: ByteString -> [Barrier]
parseRock s =
  let xs = fmap snd . filter (even . fst) . zip [0 :: Int, 1 ..] . B.words $ s
   in (parseLineSegment <$> zip xs (tail xs))

parseLineSegment :: (ByteString, ByteString) -> Barrier
parseLineSegment (s, s') = LineSegment (parseCoordinates s) (parseCoordinates s')

parseCoordinates :: ByteString -> Coordinates
parseCoordinates = bimap readInt (readInt . B.tail) . B.break (== ',')

parseRocks :: ByteString -> Barriers
parseRocks = foldl' f emptyBarriers . mconcat . fmap parseRock . B.lines
  where
    f bsm b@(Point (_, y)) = bsm {barriersHoriz = M.insertWith (++) y [b] . barriersHoriz $ bsm}
    f bsm b@(LineSegment (x, y) (x', y')) | x == x' = bsm {barriersVert = M.insertWith (++) x [b] . barriersVert $ bsm, barriersHoriz = M.insertWith (++) (min y y') [b] . barriersHoriz $ bsm}
    f bsm b@(LineSegment (_, y) (_, y')) | y == y' = bsm {barriersHoriz = M.insertWith (++) y [b] . barriersHoriz $ bsm}
    f _ _ = error "invalid input: found a barrier that's not horizontal nor vertical"

simulate :: Barriers -> Barriers
simulate bs = case restPoint (Particle (500, 0)) bs of
  Just (x, y) ->
    simulate $
      bs
        { barriersHoriz = M.insertWith (++) y [Point (x, y)] . barriersHoriz $ bs
        }
  Nothing -> bs

solve1 :: ByteString -> ByteString
solve1 s =
  let rocks = parseRocks s
      after = simulate rocks
      sz = length . mconcat . M.elems
      size bs = sz (barriersHoriz bs) + sz (barriersVert bs)
   in bshow $ size after - size rocks
