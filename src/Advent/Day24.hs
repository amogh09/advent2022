module Advent.Day24 (solve1, solve2) where

import Advent.Util (bshow)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Reader as ReaderT
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.Bifunctor (second)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Foldable (find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap as Intmap
import Data.Ix (inRange)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

{- ORMOLU_DISABLE -}
data Blizzard = Blizzard Direction Row Col deriving (Show)
type Row = Int
type Col = Int
data Direction = East | South | West | North deriving (Eq, Ord, Enum, Show)
type Blizzards = (IntMap [Blizzard], IntMap [Blizzard])
type Minutes = Int
data Valley = Valley Row Col Blizzards deriving (Show)
{- ORMOLU_ENABLE -}

blizzDir :: Blizzard -> Direction
blizzDir (Blizzard d _ _) = d

blizzRow :: Blizzard -> Row
blizzRow (Blizzard _ r _) = r

blizzCol :: Blizzard -> Col
blizzCol (Blizzard _ _ c) = c

blizzardPos :: Valley -> Minutes -> Blizzard -> Blizzard
blizzardPos (Valley maxRow maxCol _) m (Blizzard d r c) = Blizzard d r' c'
  where
    r' = (r - 1 + m * (rowDelta !! fromEnum d)) `mod` (maxRow - 1) + 1
    c' = (c - 1 + m * (colDelta !! fromEnum d)) `mod` (maxCol - 1) + 1
    rowDelta = [0, 1, 0, -1]
    colDelta = [1, 0, -1, 0]

{- ORMOLU_DISABLE -}
type BFSState = ((Row, Col), Minutes)
type Layer = [BFSState]
type BFSEnv = (Valley, (Row, Col), (Row, Col))
{- ORMOLU_ENABLE -}

bfs :: (MonadReader BFSEnv m, MonadState (Set BFSState) m) => Layer -> m Minutes
bfs layer = do
  (_, _, end) <- Reader.ask
  maybe compute (pure . snd) . find ((== end) . fst) $ layer
  where
    compute = mapM nextLayer layer >>= bfs . concat
    nextLayer ((r, c), mins) = do
      env@(v, _, _) <- Reader.ask
      visited <- State.get
      let nextStates =
            filter (escapesBlizzards v)
              . filter (not . (`Set.member` visited))
              . fmap (,mins + 1)
              . filter (inValley env)
              . neighbors
              $ (r, c)
      State.modify $ Set.union (Set.fromList nextStates)
      pure nextStates
    neighbors p = fmap (tupAdd p) [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]

escapesBlizzards :: Valley -> BFSState -> Bool
escapesBlizzards v@(Valley _ _ (rbs, cbs)) ((r, c), mins) =
  all ((/= (r, c)) . blizzRowCol . blizzardPos v mins) bs
  where
    bs = fromMaybe [] (IntMap.lookup r rbs) ++ fromMaybe [] (IntMap.lookup c cbs)

blizzRowCol :: Blizzard -> (Row, Col)
blizzRowCol (Blizzard _ r c) = (r, c)

tupAdd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

inValley :: BFSEnv -> (Row, Col) -> Bool
inValley (Valley maxRow maxCol _, start, end) (r, c) =
  (r, c) == start
    || (r, c) == end
    || (((1, 1), (maxRow - 1, maxCol - 1)) `inRange` (r, c))

valleyEnd :: Valley -> (Row, Col)
valleyEnd (Valley maxRow maxCol _) = (maxRow, maxCol - 1)

parseValley :: ByteString -> Valley
parseValley s = do
  let bs =
        concat
          . zipWith (\r row -> fmap (\(c, d) -> Blizzard d r c) row) [0, 1 ..]
          . fmap parseRow
          . B.lines
          $ s
      rows = length . B.lines $ s
      cols = length . B.unpack . head . B.lines $ s
  Valley (rows - 1) (cols - 1) (rowMap bs, colMap bs)
  where
    parseRow =
      fmap (second fromJust)
        . filter (isJust . snd)
        . zip [0, 1 ..]
        . fmap parseDir
        . B.unpack
    parseDir '#' = Nothing
    parseDir '.' = Nothing
    parseDir '>' = Just East
    parseDir 'v' = Just South
    parseDir '<' = Just West
    parseDir '^' = Just North
    parseDir c = error $ "invalid char: " <> show c
    rowMap =
      foldl' (\m b -> Intmap.insertWith (++) (blizzRow b) [b] m) IntMap.empty
        . filter ((`elem` [East, West]) . blizzDir)
    colMap =
      foldl' (\m b -> Intmap.insertWith (++) (blizzCol b) [b] m) IntMap.empty
        . filter ((`elem` [North, South]) . blizzDir)

solve1 :: ByteString -> ByteString
solve1 =
  bshow
    . (\v -> State.evalState (ReaderT.runReaderT (bfs [((0, 1), 0)]) (v, (0, 1), valleyEnd v)) Set.empty)
    . parseValley

solve2 :: ByteString -> ByteString
solve2 input = do
  let v = parseValley input
      start = (0, 1)
      end = valleyEnd v
      trip1 = run v 0 start end
      trip2 = run v trip1 end start
      trip3 = run v trip2 start end
  bshow trip3
  where
    run v m s e = State.evalState (ReaderT.runReaderT (bfs [(s, m)]) (v, s, e)) Set.empty
