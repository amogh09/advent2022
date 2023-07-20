module Advent.Day12 (solve1, solve2) where

import Advent.Util (bshow)
import Data.Array (Array)
import qualified Data.Array as A
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (ord)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

{- ORMOLU_DISABLE -}
type Coordinates = (Int, Int)
type HeightMap = Array Coordinates Char
type Layer = [Coordinates]
type Visited = Set Coordinates
{- ORMOLU_ENABLE -}

solve1 :: ByteString -> ByteString
solve1 s = do
  let (hm, start, end) = parseHeightMap s
      smallToLarge c c' = ord (hm A.! c') < ord (hm A.! c)
  bshow
    . fromMaybe 0
    . shortest smallToLarge (elem end) start
    $ hm

solve2 :: ByteString -> ByteString
solve2 s = do
  let (hm, _, end) = parseHeightMap s
      largeToSmall c c' = ord (hm A.! c) < ord (hm A.! c')
  bshow
    . fromMaybe 0
    . shortest largeToSmall (any ((== 'a') . (hm A.!))) end
    $ hm

shortest ::
  (Coordinates -> Coordinates -> Bool) ->
  (Layer -> Bool) ->
  Coordinates ->
  HeightMap ->
  Maybe Int
shortest hasAccessTo hasDestination start =
  findIndex hasDestination . takeWhile (not . null) . layers hasAccessTo start

layers :: (Coordinates -> Coordinates -> Bool) -> Coordinates -> HeightMap -> [Layer]
layers hasAccessTo start hm =
  fmap fst . iterate nextLayer $ ([start], Set.singleton start)
  where
    nextLayer :: (Layer, Visited) -> (Layer, Visited)
    nextLayer (cs, visited) = do
      let ns = Set.toList . Set.fromList $ cs >>= neighbors
      (filter (`Set.notMember` visited) ns, visited `Set.union` Set.fromList ns)

    neighbors :: Coordinates -> [Coordinates]
    neighbors c@(x, y) =
      [ c'
        | c' <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)],
          A.inRange (A.bounds hm) c',
          c `hasAccessTo` c'
      ]

parseHeightMap :: ByteString -> (HeightMap, Coordinates, Coordinates)
parseHeightMap str = do
  let grid = fmap B.unpack . B.lines $ str
      (m, n) = (length grid, length . head $ grid)
      assocs = A.range ((0, 0), (m - 1, n - 1)) `zip` mconcat grid
      (start, end, grid') = foldr f ((0, 0), (0, 0), []) assocs
  (A.listArray ((0, 0), (m - 1, n - 1)) grid', start, end)
  where
    f (c, x) (s, e, xs)
      | x == 'S' = (c, e, 'a' : xs)
      | x == 'E' = (s, c, 'z' : xs)
      | otherwise = (s, e, x : xs)
