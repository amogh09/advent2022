module Advent.Day17 (solve1, solve2) where

import Advent.Util (bshow, interleave)
import Data.Bifunctor (bimap, first, second)
import Data.Bits (Bits (shiftR), shiftL, (.&.), (.|.))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

{- ORMOLU_DISABLE -}
type YCoordinate = Int
type Moves = [Move]
type BitMask = Int
type Rock = [(YCoordinate, BitMask)]
{- ORMOLU_ENABLE -}

data Chamber = Chamber {chamberRocks :: Map YCoordinate BitMask, chamberTop :: YCoordinate}
  deriving (Show)

data Move = Left | Right | Down

newHorizontal :: Rock
newHorizontal = [(0, shiftL 15 5)]

newPlus :: Rock
newPlus = zip [2, 1, 0] . fmap (`shiftL` 6) $ [2, 7, 2]

newLMirror :: Rock
newLMirror = zip [2, 1, 0] . fmap (`shiftL` 6) $ [1, 1, 7]

newVertical :: Rock
newVertical = zip (reverse [0 .. 3]) . fmap (`shiftL` 8) $ repeat 1

newSquare :: Rock
newSquare = zip [1, 0] . fmap (`shiftL` 7) $ [3, 3]

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
      rocks = cycle [newHorizontal, newPlus, newLMirror, newVertical, newSquare]
  fmap tup3Snd
    . iterate (\(r : rs, c, ms) -> addTuple rs $ simulateRock ms (newRock r c) c)
    $ (rocks, Chamber (Map.singleton 0 511) 0, moves)
  where
    addTuple x (y, z) = (x, y, z)
    tup3Snd (_, y, _) = y

    newRock :: Rock -> Chamber -> Rock
    newRock r c = fmap (bimap ((+ 4) . (+ c.chamberTop)) (`shiftR` 3)) r

addRock :: Chamber -> Rock -> Chamber
addRock c r =
  c
    { chamberRocks =
        Map.unionWith
          (.|.)
          (Map.fromList . fmap (second (.|. walls)) $ r)
          c.chamberRocks,
      chamberTop = max c.chamberTop (topMost r)
    }

topMost :: Rock -> YCoordinate
topMost = fst . head

moveRock :: Rock -> Move -> Rock
moveRock r Down = fmap (first pred) r
moveRock r Left = fmap (second (`shiftL` 1)) r
moveRock r Right = fmap (second (`shiftR` 1)) r

walls :: Int
walls = shiftL 1 8 .|. 1

collides :: Rock -> Chamber -> Bool
collides r c = any rowCollides r
  where
    rowCollides :: (YCoordinate, BitMask) -> Bool
    rowCollides (y, x) = (/= 0) . (.&. x) . fromMaybe walls . Map.lookup y $ c.chamberRocks

parseMoves :: B.ByteString -> [Move]
parseMoves = fmap parseMove . B.unpack
  where
    parseMove '<' = Left
    parseMove '>' = Right
    parseMove x = error $ "invalid move: " <> show x

solve :: Int -> B.ByteString -> B.ByteString
solve n = bshow . chamberTop . (!! n) . simulateRocks . parseMoves

solve1 :: B.ByteString -> B.ByteString
solve1 = solve 2022

solve2 :: B.ByteString -> B.ByteString
solve2 = solve 1000000000000
