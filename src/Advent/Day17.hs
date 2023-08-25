module Advent.Day17 (solve1, solve2) where

import Advent.Util (bshow, interleave)
import Data.Bifunctor (bimap, first, second)
import Data.Bits (Bits (shiftR), shiftL, (.&.), (.|.))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find, findIndex, inits, tails)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (traceShow, traceShowId)
import Prelude hiding (Left, Right)

{- ORMOLU_DISABLE -}
type Moves = [Move]
type YCoordinate = Int
type BitMask = Int
type Rock = [(YCoordinate, BitMask)]
type RockCount = Int
type Chamber = [ChamberRow]
{- ORMOLU_ENABLE -}

data ChamberRow = ChamberRow
  { rowYCoord :: YCoordinate,
    rowBitMask :: BitMask,
    rowRockCount :: RockCount,
    rowRocks :: [RockCount]
  }
  deriving (Show)

data Move = Left | Right | Down deriving (Show, Eq)

newHorizontal :: Rock
newHorizontal = [(0, shiftL 15 5)]

newPlus :: Rock
newPlus = zip [2, 1, 0] . fmap (`shiftL` 6) $ [2, 7, 2]

newLMirror :: Rock
newLMirror = zip [2, 1, 0] . fmap (`shiftL` 6) $ [1, 1, 7]

newVertical :: Rock
newVertical = zip (reverse [0 .. 3]) . fmap (`shiftL` 8) $ repeat 1

newSquare :: Rock
newSquare = zip [1, 0] . fmap (`shiftL` 7) $ [3, 3]

simulateRock :: (RockCount, Rock) -> Chamber -> Moves -> (Chamber, Moves)
simulateRock r c = first reconstructChamber . findCollision (deconstructChamber c) r

deconstructChamber :: Chamber -> (Chamber, Chamber)
deconstructChamber chamber = ([], chamber)

reconstructChamber :: (Chamber, Chamber) -> Chamber
reconstructChamber (prev, current) = reverse prev ++ current

findCollision :: (Chamber, Chamber) -> (RockCount, Rock) -> Moves -> ((Chamber, Chamber), Moves)
findCollision c (rc, r) (m : ms) = do
  let r' = moveRock r m
      c' = moveChamber c m
  case (r' `collidesWith` c', m) of
    (True, Down) -> (merge (rc, r) c, ms)
    (True, _) -> findCollision c' (rc, r) ms
    (False, _) -> findCollision c' (rc, r') ms
findCollision _ _ _ = error "ran out of moves"

-- >>> collidesWith ([(2,15)]) ([(3,257),(4,257)],[(2,257),(1,257),(0,511)])
collidesWith :: Rock -> (Chamber, Chamber) -> Bool
collidesWith r (_, c) | fst (head r) /= (rowYCoord . head $ c) = error $ "rock and chamber misaligned: " <> show (r, c)
collidesWith r (_, c) = any ((/= 0) . uncurry (.&.) . bimap snd rowBitMask) . zip r $ c

merge :: (RockCount, Rock) -> (Chamber, Chamber) -> (Chamber, Chamber)
merge (_, r) (_, c) | fst (head r) /= (rowYCoord . head $ c) = error $ "merge rock and chamber misaligned: " <> show (r, c)
merge (rc, r) (pc, c) = do
  let pc' = fmap (\cr -> cr {rowRockCount = rc}) pc
      c' =
        zipWith
          ( \(rockCount, (_, rr)) cr ->
              cr
                { rowBitMask = rr .|. rowBitMask cr,
                  rowRockCount = if rockCount == 0 then cr.rowRockCount else rockCount,
                  rowRocks = if rockCount == 0 then cr.rowRocks else rockCount : cr.rowRocks
                }
          )
          (fmap (rc,) r ++ repeat (0, (0, 0)))
          c
  (pc', c')

moveRock :: Rock -> Move -> Rock
moveRock rock Down = fmap (first pred) rock
moveRock rock Left = fmap (second (`shiftL` 1)) rock
moveRock rock Right = fmap (second (`shiftR` 1)) rock

moveChamber :: (Chamber, Chamber) -> Move -> (Chamber, Chamber)
moveChamber (pc, c) Down = (head c : pc, tail c)
moveChamber (pc, c) _ = (pc, c)

simulateRocks :: Int -> Moves -> Int
simulateRocks n parsedMoves = do
  let moves = interleave (cycle parsedMoves) (repeat Down)
      rocks = cycle [newHorizontal, newPlus, newLMirror, newVertical, newSquare]
      chamber = [ChamberRow {rowYCoord = 0, rowBitMask = 511, rowRockCount = 0, rowRocks = []}]
  go 1 (moves, rocks, chamber)
  where
    go :: RockCount -> (Moves, [Rock], Chamber) -> Int
    go rc (ms, r : rs, c)
      | rc > n = rowYCoord . head . removeWalls $ c
      | otherwise = case isAnyPrefixCyclic rowBitMask . removeWalls $ c of
          Just (cyclicPart, basePart) -> do
            let (cl, bl) = (length cyclicPart, length basePart - 1)
                brc = traceShow (cyclicPart, basePart) $ rowRockCount (head basePart)
                crc = traceShowId (rowRockCount (head cyclicPart)) - traceShowId brc
                (q, r) = (n - brc) `divMod` crc
                modPartLength =
                  maybe (error $ "no chamber row with rock: " <> show r) rowYCoord
                    . find ((r + brc `elem`) . rowRocks)
                    $ cyclicPart
            q * cl + modPartLength
          Nothing -> do
            let (r', c') = newRock r c
                (c'', ms') = simulateRock (rc, r') c' ms
            go (rc + 1) (ms', rs, c'')
    go _ _ = error "ran out of rocks"

    thrd (_, _, z) = z

    newRock :: Rock -> Chamber -> (Rock, Chamber)
    newRock r c =
      let c' = removeWalls c
          h = rowYCoord . head $ c'
          r' = fmap (bimap (+ (h + 4)) (`shiftR` 3)) r
          c'' = fmap wallsRow (reverse [h + 1 .. (fst . head $ r')]) ++ c'
       in (r', c'')

    wallsRow :: YCoordinate -> ChamberRow
    wallsRow y = ChamberRow {rowYCoord = y, rowBitMask = walls, rowRockCount = 0, rowRocks = []}

renderChamber :: Chamber -> String
renderChamber = unlines . fmap (toString . rowBitMask)
  where
    toString :: BitMask -> String

    toChar 1 = '#'
    toChar 0 = '.'
    toChar c = error $ "unknown char: " <> show c

    renderWalls cs = '|' : (init . tail $ cs) ++ "|"
    renderFloor cs | cs == "|#######|" = "+-------+"
    renderFloor cs = cs

    toString =
      renderFloor
        . renderWalls
        . fmap toChar
        . reverse
        . fmap snd
        . tail
        . takeWhile (/= (0, 0))
        . iterate ((`divMod` 2) . fst)
        . (,0)

removeWalls :: Chamber -> Chamber
removeWalls = dropWhile ((== walls) . rowBitMask)

walls :: BitMask
walls = 257

isAnyPrefixCyclic :: (Eq b, Show a, Eq b) => (a -> b) -> [a] -> Maybe ([a], [a])
isAnyPrefixCyclic f xs
  | length xs < threshold = Nothing
  | otherwise = do
      case findIndex isCyclic . inits $ xs of
        Nothing -> Nothing
        Just i -> traceShow ("cycle found", i) $ do
          let (ls, rs) = splitAt i xs
          Just (drop (i `div` 2) ls, rs)
  where
    isCyclic xs = do
      let n = length xs
          (ls, rs) = splitAt (n `div` 2) xs
      n >= threshold && fmap f ls == fmap f rs

    threshold = 100

parseMoves :: B.ByteString -> [Move]
parseMoves = fmap parseMove . B.unpack
  where
    parseMove '<' = Left
    parseMove '>' = Right
    parseMove x = error $ "invalid move: " <> show x

solve :: Int -> B.ByteString -> B.ByteString
solve n = bshow . simulateRocks n . parseMoves

solve1 :: B.ByteString -> B.ByteString
solve1 = solve 2022

solve2 :: B.ByteString -> B.ByteString
solve2 = solve 1000000000000
