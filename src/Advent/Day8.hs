module Advent.Day8 (solve1, solve2) where

import Advent.Util (bshow)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (digitToInt)
import GHC.Arr (Array)
import qualified GHC.Arr as A
import GHC.List (foldl')

{- ORMOLU_DISABLE -}
type Matrix a = Array (Int, Int) a
type Grid = Matrix Int
type Coordinates = (Int, Int)
{- ORMOLU_ENABLE -}

-- | Solution from part 1.
--
-- >>> solve1 ("30373\n25512\n65332\n33549\n35390")
-- "21"
solve1 :: B.ByteString -> B.ByteString
solve1 = bshow . sum . fmap fromEnum . A.elems . visibleGrid . parseGrid

-- Parses a ByteString to a grid of tree heights.
--
-- >>> parseGrid "0123\n2345"
-- array ((0,0),(1,3)) [((0,0),0),((0,1),1),((0,2),2),((0,3),3),
--                      ((1,0),2),((1,1),3),((1,2),4),((1,3),5)]
parseGrid :: B.ByteString -> Grid
parseGrid s = do
  let ls = B.lines s
      (m, n) = (length ls, fromInteger . toInteger . B.length $ head ls)
      (lo, hi) = ((0, 0), (m - 1, n - 1))
  A.listArray (lo, hi) . mconcat . fmap (fmap digitToInt . B.unpack) $ ls

-- | Converts a grid of tree heights to a grid of booleans where True means the tree
-- is visible and False means that the tree is not visible.
--
-- >>> visibleGrid (parseGrid "30373\n25512")
-- array ((0,0),(1,4)) [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),
--                      ((1,0),True),((1,1),True),((1,2),True),((1,3),True),((1,4),True)]
visibleGrid :: Grid -> Matrix Bool
visibleGrid g = A.accumArray (||) False (A.bounds g) . (`zip` repeat True) . visibleAny $ g

-- | Returns a list of coordinates of trees visible from any direction in the grid.
-- Duplicate coordinates are returned if a tree on a coordinate is visible from more than
-- one direction.
visibleAny :: Grid -> [Coordinates]
visibleAny g =
  mconcat (fmap (visibleLeftOrRight g) (rows g) <> fmap (visibleLeftOrRight g) (cols g))

-- | Given a grid and a list of coordinates of a row/column, returns a list of coordinates
-- of trees that are visible from left/top or right/bottom side. Duplicate coordinates are
-- returned if a tree is visible from more than one direction.
--
-- >>> visibleLeftOrRight (parseGrid "25512") (A.range ((0,0), (0, 4)))
-- [(0,1),(0,0),(0,2),(0,4)]
visibleLeftOrRight :: Grid -> [Coordinates] -> [Coordinates]
visibleLeftOrRight g is = visibleLeft g is <> (visibleLeft g . reverse $ is)

-- | Given a grid and a list of coordinates of a raw/column, returns a list of coordinates
-- of trees that are visible from left/top direction.
--
-- >>> visibleLeft (parseGrid "25512") (A.range ((0,0), (0, 4)))
-- [(0,1),(0,0)]
visibleLeft :: Grid -> [Coordinates] -> [Coordinates]
visibleLeft grid = snd . foldl' f (minBound, [])
  where
    f (currentMax, is) i
      | grid A.! i > currentMax = (grid A.! i, i : is) -- found new tallest
      | otherwise = (currentMax, is)

-- | Solution for part 2.
--
-- >>> solve2 "30373\n25512\n65332\n33549\n35390"
-- "8"
solve2 :: B.ByteString -> B.ByteString
solve2 s = do
  let g = parseGrid s
  bshow . maximum . fmap (visibility g) . A.indices $ g

-- | Returns the visibility of tree at the given coordinate.
--
-- >>> visibleTreeCount (parseGrid "30373\n25512\n65332\n33549\n35390") (3,2)
-- 8
visibility :: (Ord a) => Matrix a -> Coordinates -> Int
visibility g c =
  foldl' (*) 1
    . fmap (visibleCount (g A.! c) . fmap (g A.!))
    $ [coordsToLeft g c, coordsToRight g c, coordsToTop g c, coordsToBottom g c]

-- | Counts the visiblity from height h with respect to given successive heights in any direction.
visibleCount :: (Ord a) => a -> [a] -> Int
visibleCount _ [] = 0
visibleCount h (h' : hs)
  | h > h' = 1 + visibleCount h hs
  | otherwise = 1

-- | Returns a list of coordinates of the matrix row-by-row.
--
-- >>> rows (parseGrid "0123\n2345")
-- [[(0,0),(0,1),(0,2),(0,3)],
--  [(1,0),(1,1),(1,2),(1,3)]]
rows :: Matrix a -> [[Coordinates]]
rows g = fmap (row g) . A.range . bimap fst fst . A.bounds $ g

-- | Returns a list of coordinates of row i.
row :: Matrix a -> Int -> [Coordinates]
row g i = zip (repeat i) . A.range . bimap snd snd . A.bounds $ g

-- | Returns a list of coordinates of the matrix column-by-column.
--
-- >>> cols (parseGrid "0123\n2345")
-- [[(0,0),(1,0)],
--  [(0,1),(1,1)],
--  [(0,2),(1,2)],
--  [(0,3),(1,3)]]
cols :: Matrix a -> [[Coordinates]]
cols g = fmap (col g) . A.range . bimap snd snd . A.bounds $ g

-- | Returns a list of coordinates of column j.
col :: Matrix a -> Int -> [Coordinates]
col g j = (`zip` repeat j) . A.range . bimap fst fst . A.bounds $ g

-- | Returns a list of coordinates from the given coordinates to the left edge of the grid.
--
-- >>> coordsToLeft (parseGrid "30373\n25512") (1,3)
-- [(1,2),(1,1),(1,0)]
coordsToLeft :: Matrix a -> Coordinates -> [Coordinates]
coordsToLeft g (i, j) = reverse . take j . row g $ i

-- | Returns a list of coordinates following the given coordinates to the right edge of the grid.
--
-- >>> coordsToRight (parseGrid "30373\n25512") (1,1)
-- [(1,2),(1,3),(1,4)]
coordsToRight :: Matrix a -> Coordinates -> [Coordinates]
coordsToRight g (i, j) = drop (j + 1) . row g $ i

-- | Returns a list of coordinates from the given coordinates to the top of the grid.
--
-- >>> coordsToTop (parseGrid "30\n25\n65\n33\n35") (3,1)
-- [(2,1),(1,1),(0,1)]
coordsToTop :: Matrix a -> Coordinates -> [Coordinates]
coordsToTop g (i, j) = reverse . take i . col g $ j

-- | Returns a list of coordinates following the given coordinates to the bottom of the grid.
--
-- >>> coordsToBottom (parseGrid "30\n25\n65\n33\n35") (1,1)
-- [(2,1),(3,1),(4,1)]
coordsToBottom :: Matrix a -> Coordinates -> [Coordinates]
coordsToBottom g (i, j) = drop (i + 1) . col g $ j
