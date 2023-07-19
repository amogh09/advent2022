module Advent.Day11 (solve1, solve2) where

import Advent.Util (bshow, splitAtEmptyLines)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl', sortOn)
import qualified Data.Ord as Ord
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- data Monkey = Monkey MonkeyId [Item] Operation Test
data Monkey = Monkey
  { monkeyItems :: Seq Item,
    monkeyOp :: Operation,
    monkeyTest :: Test,
    monkeyDivisor :: Int
  }

{- ORMOLU_DISABLE -}
type MonkeyId = Int
type Item = Int
type Operation = Item -> Item
type Test = Item -> MonkeyId
type InspectCount = Int
type Reduce = Int -> Int -- function to reduce worry
type Rounds = Int
{- ORMOLU_ENABLE -}

solve1 :: ByteString -> ByteString
solve1 = solve (const (`div` 3)) 20

solve2 :: ByteString -> ByteString
solve2 = solve (flip mod . product . fmap monkeyDivisor) 10000

solve :: ([Monkey] -> Reduce) -> Rounds -> ByteString -> ByteString
solve toReducer rounds s = do
  let monkeys = parseMonkeys s
      reduce = toReducer monkeys
      ms = V.fromList $ zip monkeys (repeat 0)
      counts =
        fmap snd
          . sortOn (Ord.Down . snd) -- sort by number of inspections
          . V.toList
          . foldl' (\ms' _ -> monkeyRound reduce ms') ms
          $ replicate rounds ()
  case counts of
    (i1 : i2 : _) -> bshow $ i1 * i2
    _ -> error "expected at least two monkeys in the input"

-- | Performs one round of monkeys throwing items at each other
monkeyRound :: Reduce -> Vector (Monkey, InspectCount) -> Vector (Monkey, InspectCount)
monkeyRound reduce ms = foldl' f ms [0 .. V.length ms - 1]
  where
    f :: Vector (Monkey, InspectCount) -> Int -> Vector (Monkey, InspectCount)
    f ms' i = V.modify (turn i) ms' -- perform turn for Monkey i
    turn i ms' = do
      (m :: Monkey, count :: InspectCount) <- MV.read ms' i -- find monkey at index i
      -- find target monkey for each item with new worry level
      let targets = fmap (targetMonkey m) m.monkeyItems
      -- add items to target monkeys
      mapM_ (\(tid, item) -> MV.modify ms' (`addItem` item) tid) targets
      -- update current monkey's items list to empty
      MV.write ms' i (m {monkeyItems = Seq.empty}, count + length (m.monkeyItems))

    targetMonkey :: Monkey -> Item -> (MonkeyId, Item)
    targetMonkey m item = do
      let item' = reduce . m.monkeyOp $ item
      (m.monkeyTest item', item')

    addItem :: (Monkey, InspectCount) -> Item -> (Monkey, InspectCount)
    addItem (m, c) i = (m {monkeyItems = m.monkeyItems |> i}, c)

parseMonkeys :: ByteString -> [Monkey]
parseMonkeys = fmap parseMonkey . splitAtEmptyLines

parseMonkey :: [ByteString] -> Monkey
parseMonkey (_ : sitems : sop : stest) = do
  let items = case B.words sitems of
        "Starting" : "items:" : is -> Seq.fromList $ fmap readInt is
        _ -> error $ "failed to parse monkey items from: " <> B.unpack sitems
      (test, d) = parseTest stest
  Monkey items (parseOperation sop) test d
  where
    parseOperation s = case B.words s of
      ["Operation:", "new", "=", x, op, y] -> case op of
        "*" -> \old -> oldOrConst old x * oldOrConst old y
        "+" -> \old -> oldOrConst old x + oldOrConst old y
        _ -> error $ "failed to parse operator: " <> B.unpack op
      _ -> error $ "failed to parse operation: " <> B.unpack s

    oldOrConst old "old" = old
    oldOrConst _ x = readInt x

    parseTest :: [ByteString] -> (Test, Int)
    parseTest [s1, s2, s3] = case (B.words s1, B.words s2, B.words s3) of
      ( ["Test:", "divisible", "by", x],
        ["If", "true:", "throw", "to", "monkey", y],
        ["If", "false:", "throw", "to", "monkey", z]
        ) -> do
          let d = readInt x
          (\item -> if item `mod` d == 0 then readInt y else readInt z, d)
      _ -> error $ "failed to parse test: " <> B.unpack (B.unlines [s1, s2, s3])
    parseTest s = error $ "failed to parse test: " <> B.unpack (B.unlines s)
parseMonkey s = error $ "invalid input: " <> B.unpack (B.unlines s)

readInt :: ByteString -> Int
readInt s = case B.readInt s of
  Just (x, _) -> x
  Nothing -> error $ "failed to parse to int: " <> B.unpack s
