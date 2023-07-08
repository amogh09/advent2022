module Advent.Day8 (solve1) where

import Advent.Util (bshow)
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

solve1 :: B.ByteString -> B.ByteString
solve1 = bshow . sum . fmap fromEnum . A.elems . visibleGrid . parseGrid

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
visibleGrid :: Matrix Int -> Matrix Bool
visibleGrid g = A.accumArray (||) False (A.bounds g) . (`zip` repeat True) . visibleAny $ g

-- | Returns a list of coordinates of trees visible from any direction in the grid.
-- Duplicate coordinates are returned if a tree on a coordinate is visible from more than
-- one direction.
visibleAny :: Grid -> [Coordinates]
visibleAny g =
  mconcat (fmap (visibleLeftOrRight g) (rows g) <> fmap (visibleLeftOrRight g) (cols g))

-- >>> visibleLeftOrRight (parseGrid "25512") (A.range ((0,0), (0, 4)))
-- [(0,1),(0,0),(0,2),(0,4)]
visibleLeftOrRight :: Grid -> [Coordinates] -> [Coordinates]
visibleLeftOrRight g is = visibleLeft g is <> (visibleLeft g . reverse $ is)

-- >>> visibleLeft (parseGrid "25512") (A.range ((0,0), (0, 4)))
-- [(0,1),(0,0)]
visibleLeft :: Grid -> [Coordinates] -> [Coordinates]
visibleLeft grid = snd . foldl' f (minBound, [])
  where
    f (currentMax, is) i
      | grid A.! i > currentMax = (grid A.! i, i : is)
      | otherwise = (currentMax, is)

-- >>> rows (parseGrid "0123\n2345")
-- [[(0,0),(0,1),(0,2),(0,3)],
--  [(1,0),(1,1),(1,2),(1,3)]]
rows :: Matrix a -> [[Coordinates]]
rows g = do
  let ((x1, y1), (x2, y2)) = A.bounds g
  [[(i, j) | j <- A.range (y1, y2)] | i <- A.range (x1, x2)]

-- >>> cols (parseGrid "0123\n2345")
-- [[(0,0),(1,0)],
--  [(0,1),(1,1)],
--  [(0,2),(1,2)],
--  [(0,3),(1,3)]]
cols :: Matrix a -> [[Coordinates]]
cols g = do
  let ((x1, y1), (x2, y2)) = A.bounds g
  [[(i, j) | i <- A.range (x1, x2)] | j <- A.range (y1, y2)]
