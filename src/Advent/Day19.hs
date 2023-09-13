module Advent.Day19 (solve1) where

import Advent.Util (bshow, maximumMaybe, readInt)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (traceShow, traceShowId)

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
nextStates bp inv =
  mapMaybe (\f -> f bp inv) [makeGeodeRobotNext, makeObsidianRobotNext, makeClayRobotNext, makeOreRobotNext]

simulate ::
  (MonadReader Blueprint m, MonadState (Map Inventory Geode) m) => Inventory -> m Geode
-- simulate inv = traceShow inv $ do
simulate inv = do
  cache <- State.get
  if invMinutesRemaining inv == 0
    then pure $ invGeode inv
    else maybe compute pure $ Map.lookup inv cache
  where
    compute = do
      bp <- Reader.ask
      res <-
        fmap (fromMaybe (invGeode . doNothing $ inv) . maximumMaybe)
          . mapM simulate
          . nextStates bp
          $ inv
      State.modify $ Map.insert inv res
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
solve1 s = do
  let start =
        Inventory
          { invOre = 0,
            invClay = 0,
            invObsidian = 0,
            invGeode = 0,
            invOreRobot = 1,
            invClayRobot = 0,
            invObsidianRobot = 0,
            invGeodeRobot = 0,
            invMinutesRemaining = 24
          }
  bshow
    . sum
    . zipWith (*) [1, 2 ..]
    . fmap fst
    . traceShowId
    . fmap (second Map.size)
    . fmap (\bp -> State.runState (Reader.runReaderT (simulate start) bp) Map.empty)
    . parseBps
    $ s
