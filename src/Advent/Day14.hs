module Advent.Day14 (solve1, solve2) where

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
data HorizBarrier = HorizBarrier Int (Int, Int)
data VertBarrier = VertBarrier Int (Int, Int)
newtype VerticalLine = VerticalLine Coordinates
data Barriers = Barriers {
      barriersHoriz :: Map Int [HorizBarrier],
      barriersVert :: Map Int [VertBarrier]
  }
{- ORMOLU_ENABLE -}

emptyBarriers :: Barriers
emptyBarriers = Barriers M.empty M.empty

-- | Intersection of a vertical line with a horizontal barrier.
intersection :: VerticalLine -> HorizBarrier -> Maybe Coordinates
intersection (VerticalLine (x, y)) (HorizBarrier y' (x1, x2))
  | y > y' = Nothing
  | x >= min x1 x2 && x <= max x1 x2 = Just (x, y')
  | otherwise = Nothing

-- | Whether the given coordinates lie on the horizontal barrier.
liesOnHoriz :: Coordinates -> HorizBarrier -> Bool
liesOnHoriz (x, y) (HorizBarrier y' (x1, x2))
  | y /= y' = False
  | otherwise = x >= min x1 x2 && x <= max x1 x2

-- | Whether the given coordinates lie on the vertical barrier.
liesOnVert :: Coordinates -> VertBarrier -> Bool
liesOnVert (x, y) (VertBarrier x' (y1, y2))
  | x /= x' = False
  | otherwise = y >= min y1 y2 && y <= max y1 y2

-- | Given a list of barriers, find the final resting point of sand particle.
--   Returns Nothing if the particle lies on a barrier or interacts with no barriers.
restPoint :: Coordinates -> Barriers -> Maybe Coordinates
restPoint p@(px, py) bsm = M.lookupGT py (barriersHoriz bsm) >>= checkLevel
  where
    checkLevel (k, bs) =
      case fmap (second pred) . mapMaybe (intersection $ VerticalLine p) $ bs of
        [] -> restPoint (px, k) bsm
        (x, y) : _ -> case (isBlocked (x - 1, y + 1) bsm, isBlocked (x + 1, y + 1) bsm) of
          (False, _) -> restPoint (x - 1, y + 1) bsm
          (_, False) -> restPoint (x + 1, y + 1) bsm
          _ -> Just (x, y)

isBlocked :: Coordinates -> Barriers -> Bool
isBlocked (x, y) bs = isBlockedHoriz || isBlockedVert
  where
    isBlockedHoriz = maybe False (any ((x, y) `liesOnHoriz`)) . M.lookup y . barriersHoriz $ bs
    isBlockedVert = maybe False (any ((x, y) `liesOnVert`)) . M.lookup x . barriersVert $ bs

parseRock :: ByteString -> [Either HorizBarrier VertBarrier]
parseRock s =
  let xs = fmap snd . filter (even . fst) . zip [0 :: Int, 1 ..] . B.words $ s
   in (parseBarrier <$> zip xs (tail xs))

parseBarrier :: (ByteString, ByteString) -> Either HorizBarrier VertBarrier
parseBarrier (s, s') = case (parseCoordinates s, parseCoordinates s') of
  ((x1, y1), (x2, y2)) | x1 == x2 -> Right $ VertBarrier x1 (y1, y2)
  ((x1, y1), (x2, y2)) | y1 == y2 -> Left $ HorizBarrier y1 (x1, x2)
  _ -> error "invalid input: barrier is nor horizontal nor vertical"

parseCoordinates :: ByteString -> Coordinates
parseCoordinates = bimap readInt (readInt . B.tail) . B.break (== ',')

mkBarriers :: [Either HorizBarrier VertBarrier] -> Barriers
mkBarriers = foldl' f emptyBarriers
  where
    f bsm = either (addHorizBarrier bsm) (addVertBarrier bsm)

addHorizBarrier :: Barriers -> HorizBarrier -> Barriers
addHorizBarrier bsm b@(HorizBarrier y _) =
  bsm {barriersHoriz = M.insertWith (++) y [b] . barriersHoriz $ bsm}

addVertBarrier :: Barriers -> VertBarrier -> Barriers
addVertBarrier bsm b@(VertBarrier x (y1, y2)) =
  bsm
    { barriersVert = M.insertWith (++) x [b] . barriersVert $ bsm,
      barriersHoriz =
        let y = min y1 y2
         in M.insertWith (++) y [HorizBarrier y (x, x)] . barriersHoriz $ bsm
    }

simulate :: Barriers -> Barriers
simulate bs = maybe bs updateAndRepeat . restPoint (500, 0) $ bs
  where
    updateAndRepeat (500, 0) = bs
    updateAndRepeat (x, y) =
      simulate $
        bs {barriersHoriz = M.insertWith (++) y [HorizBarrier y (x, x)] . barriersHoriz $ bs}

listMapLength :: Map k [v] -> Int
listMapLength = length . mconcat . M.elems

solve1 :: ByteString -> ByteString
solve1 s =
  let rocks = mkBarriers . mconcat . fmap parseRock . B.lines $ s
      after = simulate rocks
      size bs = listMapLength (barriersHoriz bs) + listMapLength (barriersVert bs)
   in bshow $ size after - size rocks

solve2 :: ByteString -> ByteString
solve2 s =
  let rocks = mconcat . fmap parseRock . B.lines $ s
      lowestPoint = maximum . fmap (either hbLowestPoint vbLowestPoint) $ rocks
      barriers =
        mkBarriers
          . (Left (HorizBarrier (2 + lowestPoint) (minBound, maxBound)) :)
          $ rocks
      after = simulate barriers
      size bs = listMapLength (barriersHoriz bs) + listMapLength (barriersVert bs)
   in bshow $ size after - size barriers + 1
  where
    hbLowestPoint (HorizBarrier y _) = y
    vbLowestPoint (VertBarrier _ (y1, y2)) = min y1 y2
