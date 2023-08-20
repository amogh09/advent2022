module Advent.Day16 (solve1, solve2) where

import Advent.Util (bshow, maximumOnMaybe, pairs, stripComma)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Tuple (swap)

{- ORMOLU_DISABLE -}
type Graph = Map RoomName (FlowRate, [RoomName])
type RoomName = SB.ByteString
type Indices = Map RoomName Int
type FlowRate = Int
type Minutes = Int
type Pressure = Int
type Distance = Int
type Distances = Map RoomName [(RoomName, Distance)]
type Visited = Int
{- ORMOLU_ENABLE -}

hasPositiveFlowRate :: RoomName -> Graph -> Bool
hasPositiveFlowRate r = maybe False ((> 0) . fst) . Map.lookup r

connectedRooms :: Graph -> RoomName -> [RoomName]
connectedRooms g r = maybe [] snd . Map.lookup r $ g

flowRate :: RoomName -> Graph -> FlowRate
flowRate r = maybe 0 fst . Map.lookup r

indices :: Graph -> Indices
indices =
  Map.fromList
    . fmap (second (1 `shiftL`))
    . (`zip` [0, 1 ..])
    . fmap fst
    . filter ((> 0) . fst . snd)
    . Map.toList

parseGraph :: LB.ByteString -> Graph
parseGraph = Map.fromList . fmap parseRoom . LB.lines
  where
    parseRoom :: LB.ByteString -> (RoomName, (FlowRate, [RoomName]))
    parseRoom s =
      let ws = LB.words s
          name = LB.toStrict $ ws !! 1
          rate = readInt . LB.drop 5 $ ws !! 4
          connections = fmap (LB.toStrict . stripComma) . drop 9 $ ws
       in (name, (rate, connections))

    readInt s = case LB.readInt s of
      Just (x, _) -> x
      _ -> error $ "failed to read int: " <> LB.unpack s

distances :: Graph -> RoomName -> [(RoomName, Distance)]
distances g r =
  concatMap (\(d, rs) -> fmap (,d) rs)
    . zip [1, 2 ..]
    . tail
    . fmap snd
    . takeWhile (not . null . snd)
    . iterate nextLayer
    $ (Set.singleton r, [r])
  where
    nextLayer (v, rs) =
      let layer = filter (not . flip Set.member v) (rs >>= connectedRooms g)
       in (Set.fromList layer `Set.union` v, layer)

allDistances :: RoomName -> Graph -> Distances
allDistances start g =
  Map.fromList
    . fmap (\r -> (r, filter (shouldKeep . fst) $ distances g r))
    . filter shouldKeep
    . Map.keys
    $ g
  where
    shouldKeep r' = hasPositiveFlowRate r' g || r' == start

dfs :: Graph -> Indices -> Distances -> Visited -> (RoomName, Minutes, Pressure) -> [(Pressure, Visited)]
dfs g is ds v (r, minsLeft, pressureSecured) = do
  ((pressureSecured, fromMaybe 0 . Map.lookup r $ is) :)
    . fmap (second (`addVisited` r))
    . concatMap (uncurry next)
    . filter ((< minsLeft) . snd)
    . filter (maybe False ((== 0) . (v .&.)) . flip Map.lookup is . fst)
    . filter ((/= "AA") . fst)
    . fromMaybe []
    . flip Map.lookup ds
    $ r
  where
    next r' d =
      let left = minsLeft - d - 1
       in dfs g is ds (addVisited v r) (r', left, pressureSecured + flowRate r' g * left)
    addVisited x y = maybe x (x .|.) . Map.lookup y $ is

solve1 :: LB.ByteString -> LB.ByteString
solve1 s =
  let g = parseGraph s
      ds = allDistances "AA" g
      is = indices g
   in bshow . maybe 0 fst . maximumOnMaybe fst . dfs g is ds 0 $ ("AA", 30, 0)

solve2 :: LB.ByteString -> LB.ByteString
solve2 s = do
  let g = parseGraph s
      start = ("AA", 26, 0)
      ds = allDistances "AA" g
      is = indices g
  bshow
    . maximum
    . fmap (uncurry (+) . bimap snd snd)
    . filter ((== 0) . uncurry (.&.) . bimap fst fst)
    . pairs
    . Map.toList
    . foldl' (\m (v, p) -> Map.insertWith max v p m) Map.empty
    . fmap swap
    . dfs g is ds 0
    $ start
