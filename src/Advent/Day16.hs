module Advent.Day16 (solve1) where

import Advent.Util (bshow, maximumMaybe, stripComma)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

{- ORMOLU_DISABLE -}
type Graph = Map RoomName (FlowRate, [RoomName])
type RoomName = SB.ByteString
type FlowRate = Int
type Minutes = Int
type Pressure = Int
type Distance = Int
type Distances = Map RoomName [(RoomName, Distance)]
{- ORMOLU_ENABLE -}

hasPositiveFlowRate :: RoomName -> Graph -> Bool
hasPositiveFlowRate r = maybe False ((> 0) . fst) . Map.lookup r

connectedRooms :: Graph -> RoomName -> [RoomName]
connectedRooms g r = maybe [] snd . Map.lookup r $ g

flowRate :: RoomName -> Graph -> FlowRate
flowRate r = maybe 0 fst . Map.lookup r

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

dfs :: Graph -> Distances -> Set RoomName -> (RoomName, Minutes, Pressure) -> Pressure
dfs g ds v (r, minsLeft, pressureSecured) = do
  fromMaybe pressureSecured
    . maximumMaybe
    . fmap (uncurry next)
    . filter ((< minsLeft) . snd)
    . filter (not . (`Set.member` v) . fst)
    . fromMaybe []
    . flip Map.lookup ds
    $ r
  where
    next r' d =
      let left = minsLeft - d - 1
       in dfs g ds (Set.insert r v) (r', left, pressureSecured + flowRate r' g * left)

solve1 :: LB.ByteString -> LB.ByteString
solve1 s =
  let g = parseGraph s
      ds = allDistances "AA" g
   in bshow $ dfs g ds Set.empty ("AA", 30, 0)
