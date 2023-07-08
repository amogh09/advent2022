module Advent.Day8 (solve1) where

import Advent.Util (bshow)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (digitToInt)
import Data.List (transpose)
import GHC.Arr (Array)
import qualified GHC.Arr as A
import GHC.List (foldl')

type Matrix a = Array (Int, Int) a

type Grid = Matrix Int

solve1 :: B.ByteString -> B.ByteString
solve1 =
  bshow
    . sum
    . fmap fromEnum
    . A.elems
    . visibleGrid
    . fmap (fmap digitToInt . B.unpack)
    . B.lines

-- >>> parseGrid "0123\n2345"
-- array ((0,0),(1,3)) [((0,0),0),((0,1),1),((0,2),2),((0,3),3),
--                      ((1,0),2),((1,1),3),((1,2),4),((1,3),5)]
-- parseGrid :: B.ByteString -> Grid
-- parseGrid s = do
--   let ls = B.lines s
--       (m, n) = (length ls, fromInteger . toInteger . B.length $ head ls)
--       (lo, hi) = ((0, 0), (m - 1, n - 1))
--   A.listArray (lo, hi) . mconcat . fmap (fmap digitToInt . B.unpack) $ ls

-- visibleLeft :: Grid -> [(Int, Int)] -> [(Int, Int)]
-- visibleLeft grid = snd . foldl' f (minBound, [])
--   where
--     f :: (Int, [(Int, Int)]) -> (Int, Int) -> (Int, [(Int, Int)])
--     f = undefined

-- f (currentMax, indices) (i, x)
--   | x > currentMax = (x, i : indices)
--   | otherwise = (currentMax, indices)

-- | Given a grid of tree heights, returns a grid of Booleans (one for each height) where
-- True means that the tree is visible from at least one direction, and False otherwise.
--
-- >>> visibleGrid [[3,0,3,7,3], [2,5,5,1,2], [6,5,3,3,2]]
-- array ((0,0),(2,4)) [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),
--                      ((1,0),True),((1,1),True),((1,2),True),((1,3),False),((1,4),True),
--                      ((2,0),True),((2,1),True),((2,2),True),((2,3),True),((2,4),True)]
visibleGrid :: [[Int]] -> Matrix Bool
visibleGrid grid = visibleLeftOrRightGrid grid `merge` visibleLeftOrRightGrid (transpose grid)
  where
    merge :: Matrix Bool -> Matrix Bool -> Matrix Bool
    merge m1 m2 =
      let b = A.bounds m1
       in A.array b [((i, j), m1 A.! (i, j) || m2 A.! (j, i)) | (i, j) <- A.range b]

-- | Given a grid of heights, returns a grid of booleans in which an element is True
-- if its corresponding index in the input grid is visible from either left or right, and
-- is False otherwise.
--
-- >>> visibleLeftOrRightGrid [[3, 0, 3, 7, 3], [2, 5, 5, 1, 2]]
-- array ((0,0),(1,4))
--       [((0,0),True),((0,1),False),((0,2),False),((0,3),True),((0,4),True),
--        ((1,0),True),((1,1),True),((1,2),True),((1,3),False),((1,4),True)]
visibleLeftOrRightGrid :: [[Int]] -> Matrix Bool
visibleLeftOrRightGrid grid =
  A.accumArray (||) False ((0, 0), (m - 1, n - 1))
    . mconcat
    . fmap (\(i :: Int, row) -> (`zip` repeat True) . fmap (i,) . visibleLeftOrRight $ row)
    . zip [0 ..]
    $ grid
  where
    m = length grid
    n = length $ head grid

-- | Given a list of heights, returns a list of indices of trees that are visible
-- from either the left or the right side. Returns duplicate indices.
--
-- >>> visibleLeftOrRight [2, 5, 5, 1, 2]
-- [1,0,2,4]
--
-- >>> visibleLeftOrRight [1]
-- [0,0]
visibleLeftOrRight :: [Int] -> [Int]
visibleLeftOrRight hs = visibleLeft hs <> (fmap wrap . visibleLeft . reverse $ hs)
  where
    wrap i = n - 1 - i
    n = length hs

-- | Given a list of heights, returns a list of indices of trees that are
-- visible from the left side.
--
-- >>> visibleLeft [2, 5, 5, 1, 2]
-- [1,0]
visibleLeft :: [Int] -> [Int]
visibleLeft = snd . foldl' f (minBound, []) . zip [0 ..]
  where
    f :: (Int, [Int]) -> (Int, Int) -> (Int, [Int])
    f (currentMax, indices) (i, x)
      | x > currentMax = (x, i : indices)
      | otherwise = (currentMax, indices)
