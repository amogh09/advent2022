module Advent.Day19 (solve1, solve2) where

import Advent.Util (bshow, maximumMaybe, readInt)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

{- ORMOLU_DISABLE -}
type Ore = Int
type Clay = Int
type Obsidian = Int
type Geode = Int
{- ORMOLU_ENABLE -}

data Inventory = Inventory
  { invOre :: Ore,
    invClay :: Clay,
    invObsidian :: Obsidian,
    invGeode :: Geode,
    invOreRobot :: Int,
    invClayRobot :: Int,
    invObsidianRobot :: Int,
    invGeodeRobot :: Int,
    invMinutesRemaining :: Int
  }
  deriving (Show, Eq, Ord)

data Blueprint = Blueprint
  { bpOre :: Ore,
    bpClay :: Ore,
    bpObsidian :: (Ore, Clay),
    bpGeode :: (Ore, Obsidian)
  }

calcMinsNeeded :: Int -> Int -> Int -> Int
calcMinsNeeded current desired robots
  | current >= desired = 1
  | otherwise = ceiling (fromIntegral (desired - current) / fromIntegral robots :: Float) + 1

makeOreRobotNext :: Blueprint -> Inventory -> Maybe Inventory
makeOreRobotNext _ inv | inv.invOreRobot <= 0 = Nothing
makeOreRobotNext bp inv = do
  let minsNeeded = calcMinsNeeded inv.invOre bp.bpOre inv.invOreRobot
      inv' = updateResources minsNeeded (bp.bpOre, 0, 0, 0) inv
  if minsNeeded > inv.invMinutesRemaining
    then Nothing
    else Just $ inv' {invOreRobot = succ inv.invOreRobot}

makeClayRobotNext :: Blueprint -> Inventory -> Maybe Inventory
makeClayRobotNext _ inv | inv.invOreRobot <= 0 = Nothing
makeClayRobotNext bp inv = do
  let minsNeeded = calcMinsNeeded inv.invOre bp.bpClay inv.invOreRobot
      inv' = updateResources minsNeeded (bp.bpClay, 0, 0, 0) inv
  if minsNeeded > inv.invMinutesRemaining
    then Nothing
    else Just $ inv' {invClayRobot = succ inv.invClayRobot}

makeObsidianRobotNext :: Blueprint -> Inventory -> Maybe Inventory
makeObsidianRobotNext _ inv | inv.invOreRobot <= 0 || inv.invClayRobot <= 0 = Nothing
makeObsidianRobotNext bp inv = do
  let (oreNeeded, clayNeeded) = bp.bpObsidian
      oreMinsNeeded = calcMinsNeeded inv.invOre oreNeeded inv.invOreRobot
      clayMinsNeeded = calcMinsNeeded inv.invClay clayNeeded inv.invClayRobot
      minsNeeded = max oreMinsNeeded clayMinsNeeded
      inv' = updateResources minsNeeded (oreNeeded, clayNeeded, 0, 0) inv
  if minsNeeded > inv.invMinutesRemaining
    then Nothing
    else Just $ inv' {invObsidianRobot = succ inv.invObsidianRobot}

makeGeodeRobotNext :: Blueprint -> Inventory -> Maybe Inventory
makeGeodeRobotNext _ inv | inv.invOreRobot <= 0 || inv.invObsidianRobot <= 0 = Nothing
makeGeodeRobotNext bp inv = do
  let (oreNeeded, obsidianNeeded) = bp.bpGeode
      oreMinsNeeded = calcMinsNeeded inv.invOre oreNeeded inv.invOreRobot
      obsidianMinsNeeded = calcMinsNeeded inv.invObsidian obsidianNeeded inv.invObsidianRobot
      minsNeeded = max oreMinsNeeded obsidianMinsNeeded
      inv' = updateResources minsNeeded (oreNeeded, 0, obsidianNeeded, 0) inv
  if minsNeeded > inv.invMinutesRemaining
    then Nothing
    else Just $ inv' {invGeodeRobot = succ inv.invGeodeRobot}

updateResources :: Int -> (Ore, Clay, Obsidian, Geode) -> Inventory -> Inventory
updateResources minsElapsed (ore, clay, obsidian, geode) inv =
  inv
    { invOre = inv.invOre + inv.invOreRobot * minsElapsed - ore,
      invClay = inv.invClay + inv.invClayRobot * minsElapsed - clay,
      invObsidian = inv.invObsidian + inv.invObsidianRobot * minsElapsed - obsidian,
      invGeode = inv.invGeode + inv.invGeodeRobot * minsElapsed - geode,
      invMinutesRemaining = inv.invMinutesRemaining - minsElapsed
    }

doNothing :: Inventory -> Inventory
doNothing inv = updateResources inv.invMinutesRemaining (0, 0, 0, 0) inv

nextStates :: Blueprint -> Inventory -> [Inventory]
nextStates bp inv = do
  let maxOreRobots = maximum [bp.bpOre, bp.bpClay, fst bp.bpObsidian, fst bp.bpGeode]
      maxClayRobots = snd bp.bpObsidian
      maxObsidianRobots = snd bp.bpGeode
  if inv.invOreRobot >= fst bp.bpGeode && inv.invObsidianRobot >= snd bp.bpGeode
    then catMaybes [makeGeodeRobotNext bp inv]
    else
      mapMaybe (\f -> f bp inv) . catMaybes $
        [ Just makeGeodeRobotNext,
          if inv.invOreRobot < maxOreRobots then Just makeOreRobotNext else Nothing,
          if inv.invClayRobot < maxClayRobots then Just makeClayRobotNext else Nothing,
          if inv.invObsidianRobot < maxObsidianRobots then Just makeObsidianRobotNext else Nothing
        ]

data SearchState = SearchState
  { searchCache :: Map Inventory Geode,
    searchCurrentBest :: Geode
  }

search ::
  (MonadReader Blueprint m, MonadState SearchState m) => Inventory -> m Geode
search inv | inv.invMinutesRemaining == 0 = pure $ inv.invGeode
search inv = do
  s <- State.get
  let cache = s.searchCache
      currentBest = s.searchCurrentBest
      bestPossible =
        inv.invGeode
          + inv.invGeodeRobot * inv.invMinutesRemaining
          + (inv.invMinutesRemaining * (inv.invMinutesRemaining - 1) `div` 2)
  if bestPossible <= currentBest
    then pure 0
    else maybe compute pure $ Map.lookup inv cache
  where
    compute = do
      bp <- Reader.ask
      res <-
        fmap (fromMaybe (invGeode . doNothing $ inv) . maximumMaybe)
          . mapM search
          . nextStates bp
          $ inv
      State.modify $ \s ->
        s
          { searchCache = Map.insert inv res s.searchCache,
            searchCurrentBest = max res s.searchCurrentBest
          }
      pure res

parseBps :: ByteString -> [Blueprint]
parseBps = fmap parseBp . B.lines

parseBp :: ByteString -> Blueprint
parseBp s = do
  let ws = B.words s
  Blueprint
    { bpOre = readInt $ ws !! 6,
      bpClay = readInt $ ws !! 12,
      bpObsidian = (readInt $ ws !! 18, readInt $ ws !! 21),
      bpGeode = (readInt $ ws !! 27, readInt $ ws !! 30)
    }

solve1 :: ByteString -> ByteString
solve1 = bshow . sum . zipWith (*) [1, 2 ..] . solve (Inventory 0 0 0 0 1 0 0 0 24) . parseBps

solve2 :: ByteString -> ByteString
solve2 = bshow . product . solve (Inventory 0 0 0 0 1 0 0 0 32) . take 3 . parseBps

solve :: Functor f => Inventory -> f Blueprint -> f Geode
solve start =
  fmap (\bp -> State.evalState (Reader.runReaderT (search start) bp) (SearchState Map.empty 0))
