module Advent.Day18 (solve1, solve2) where

import Advent.Util (bshow, readInt)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.State.Class (MonadState)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{- ORMOLU_DISABLE -}
type Grid = Map Coordinates SurfaceArea
type Coordinates = (Int, Int, Int)
type SurfaceArea = Int
type Bounds = ((Int, Int), (Int, Int), (Int, Int))
type DFSState = Set Coordinates
data DFSEnv = DFSEnv {dfsGrid :: Grid, dfsBounds :: Bounds}
{- ORMOLU_ENABLE -}

addCube :: MonadState Grid m => Coordinates -> m ()
addCube c = do
  covered <- mapM decrementArea . neighbors $ c
  State.modify $ Map.insert c (6 - sum (fromEnum <$> covered))

neighbors :: Coordinates -> [Coordinates]
neighbors (x, y, z) = concat [[(x + d, y, z), (x, y + d, z), (x, y, z + d)] | d <- [-1, 1]]

decrementArea :: MonadState Grid m => Coordinates -> m Bool
decrementArea c = do
  State.modify $ Map.update (Just . max 0 . pred) c
  State.gets (Map.member c)

dfs :: (MonadState DFSState m, MonadReader DFSEnv m) => Coordinates -> m SurfaceArea
dfs c = do
  grid <- Reader.asks dfsGrid
  bounds <- Reader.asks dfsBounds
  visited <- State.get
  let ns = filter (not . (`Set.member` visited)) . neighbors $ c
      walls = Set.fromList . filter (`Map.member` grid) $ ns
      air = filter (inBounds bounds) . filter (not . (`Set.member` walls)) $ ns
  State.put $ Set.union (Set.fromList air) visited
  fmap ((+ length walls) . sum) . mapM dfs $ air

inBounds :: Bounds -> Coordinates -> Bool
inBounds (bx, by, bz) (x, y, z) = x `isBetween` bx && y `isBetween` by && z `isBetween` bz
  where
    isBetween a (amin, amax) = a >= amin && a <= amax

gridBounds :: Grid -> Bounds
gridBounds g = do
  let cs = Map.keys g
      xs = fmap (\(x, _, _) -> x) cs
      ys = fmap (\(_, y, _) -> y) cs
      zs = fmap (\(_, _, z) -> z) cs
      toBounds as = (minimum as - 1, maximum as + 1)
  (toBounds xs, toBounds ys, toBounds zs)

solve1 :: ByteString -> ByteString
solve1 s =
  bshow . sum . Map.elems . State.execState (mapM_ addCube . parseCoordinates $ s) $ Map.empty

solve2 :: ByteString -> ByteString
solve2 s = do
  let grid = State.execState (mapM_ addCube . parseCoordinates $ s) Map.empty
      bounds = gridBounds grid
      start = (\((x, _), (y, _), (z, _)) -> (x, y, z)) bounds
      env = DFSEnv {dfsGrid = grid, dfsBounds = bounds}
      state = Set.empty
  bshow . flip State.evalState state . flip Reader.runReaderT env $ dfs start

parseCoordinates :: ByteString -> [Coordinates]
parseCoordinates = fmap parseSingle . B.lines
  where
    parseSingle s =
      case B.split ',' s of
        [x, y, z] -> (readInt x, readInt y, readInt z)
        _ -> error "invalid input"
