module Advent.Day23 (solve1, solve2) where

import Advent.Util (bshow)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (round)

{- ORMOLU_DISABLE -}
type Elf = (Int, Int)
type Elves = Set Elf
type Proposal = Elves -> Elf -> Maybe Elf
type Rectangle = (Elf, Elf)
{- ORMOLU_ENABLE -}

proposals :: [Proposal]
proposals = [northProposal, southProposal, westProposal, eastProposal]
  where
    northProposal es (r, c) = mkProposal es [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)] (r - 1, c)
    southProposal es (r, c) = mkProposal es [(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)] (r + 1, c)
    westProposal es (r, c) = mkProposal es [(r + 1, c - 1), (r, c - 1), (r - 1, c - 1)] (r, c - 1)
    eastProposal es (r, c) = mkProposal es [(r + 1, c + 1), (r, c + 1), (r - 1, c + 1)] (r, c + 1)
    mkProposal elves ps p = if ps `noneInSet` elves then Just p else Nothing

noneInSet :: (Foldable t, Ord a) => t a -> Set a -> Bool
noneInSet xs set = not . any (`S.member` set) $ xs

elfProposal :: Elves -> [Proposal] -> Elf -> Maybe Elf
elfProposal es ps e = listToMaybe . mapMaybe (\p -> p es e) $ ps

proposeAll :: [Proposal] -> Elves -> [Elf] -> [(Elf, Elf)]
proposeAll ps elves es =
  fmap (second fromJust) . filter (isJust . snd) . zip es . fmap (elfProposal elves ps) $ es

hasNeighbor :: Elves -> Elf -> Bool
hasNeighbor elves = any (`S.member` elves) . neighborhood
  where
    neighborhood p = topLevel p ++ sameLevel p ++ bottomLevel p
    topLevel (r, c) = [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)]
    sameLevel (r, c) = [(r, c - 1), (r, c + 1)]
    bottomLevel (r, c) = [(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]

round :: ([Proposal], Elves) -> ([Proposal], Elves)
round (ps, elves) = do
  let (prevs, news) =
        unzip . removeDups . proposeAll ps elves . filter (hasNeighbor elves) . S.toList $ elves
  (tail ps ++ [head ps], S.fromList news `S.union` (elves `S.difference` S.fromList prevs))
  where
    removeDups es = do
      let proposedCount = counter . fmap snd $ es
      filter ((== 1) . fromMaybe 0 . (`Map.lookup` proposedCount) . snd) es

counter :: (Foldable t, Ord a) => t a -> Map a Integer
counter = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty

parseElves :: ByteString -> Elves
parseElves =
  S.fromList . concat . zipWith (\r cs -> fmap (r,) cs) [0, 1 ..] . fmap parseRow . B.lines
  where
    parseRow = fmap fst . filter ((== '#') . snd) . zip [0, 1 ..] . B.unpack

boundingRect :: Elves -> Rectangle
boundingRect elves = (topLeft, bottomRight)
  where
    topLeft = (lowestRow, lowestCol)
    bottomRight = (highestRow, highestCol)
    lowestRow = minimum . fmap fst . S.toList $ elves
    lowestCol = minimum . fmap snd . S.toList $ elves
    highestRow = maximum . fmap fst . S.toList $ elves
    highestCol = maximum . fmap snd . S.toList $ elves

rectArea :: Rectangle -> Int
rectArea ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

solve1 :: ByteString -> ByteString
solve1 s = do
  let elves = parseElves s
  bshow
    . subtract (length elves)
    . rectArea
    . boundingRect
    . snd
    . (!! 10)
    . iterate round
    $ (proposals, elves)

solve2 :: ByteString -> ByteString
solve2 s = do
  let rounds = fmap snd . iterate round . (proposals,) . parseElves $ s
  bshow
    . fst
    . head
    . filter (uncurry (==) . snd)
    . zip [1 :: Int, 2 ..]
    . zip (tail rounds)
    $ rounds
