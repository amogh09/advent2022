module Advent.Day11 (solve1) where

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
    monkeyTest :: Test
  }

type MonkeyId = Int

type Item = Int

type Operation = Item -> Item

type Test = Item -> MonkeyId

solve1 :: ByteString -> ByteString
solve1 s = do
  let ms = V.fromList $ zip (parseMonkeys s) (repeat 0)
      counts =
        fmap snd
          . sortOn (Ord.Down . snd)
          . V.toList
          . foldr (\_ ms' -> monkeyRound ms') ms
          $ replicate 20 'a'
  case counts of
    (i1 : i2 : _) -> bshow $ i1 * i2
    _ -> error "expected at least two monkeys in the input"

parseMonkeys :: ByteString -> [Monkey]
parseMonkeys = fmap parseMonkey . splitAtEmptyLines

parseMonkey :: [ByteString] -> Monkey
parseMonkey (_ : sitems : sop : stest) = do
  let items = case B.words sitems of
        "Starting" : "items:" : is -> Seq.fromList $ fmap readInt is
        _ -> error $ "failed to parse monkey items from: " <> B.unpack sitems
      op = parseOperation sop
      test = parseTest stest
  Monkey items op test
  where
    parseOperation :: ByteString -> Operation
    parseOperation s = case B.words s of
      ["Operation:", "new", "=", x, op, y] -> case op of
        "*" -> \old -> oldOrConst old x * oldOrConst old y
        "+" -> \old -> oldOrConst old x + oldOrConst old y
        _ -> error $ "failed to parse operator: " <> B.unpack op
      _ -> error $ "failed to parse operation: " <> B.unpack s

    oldOrConst :: Int -> ByteString -> Int
    oldOrConst old "old" = old
    oldOrConst _ x = readInt x

    parseTest :: [ByteString] -> Test
    parseTest [s1, s2, s3] = case (B.words s1, B.words s2, B.words s3) of
      ( ["Test:", "divisible", "by", x],
        ["If", "true:", "throw", "to", "monkey", y],
        ["If", "false:", "throw", "to", "monkey", z]
        ) -> \item -> if item `mod` readInt x == 0 then readInt y else readInt z
      _ -> error $ "failed to parse test: " <> B.unpack (B.unlines [s1, s2, s3])
    parseTest s =
      error $ "failed to parse test, unexpected line count: " <> B.unpack (B.unlines s)
parseMonkey s = error $ "invalid input: " <> B.unpack (B.unlines s)

monkeyRound :: Vector (Monkey, Int) -> Vector (Monkey, Int)
monkeyRound ms = foldl' f ms [0 .. V.length ms - 1]
  where
    f acc i = V.modify (turn i) acc

    turn i acc = do
      (m, count) <- MV.read acc i
      let targets = fmap (targetMonkey m) m.monkeyItems
      mapM_ (\(tid, item) -> MV.modify acc (`addItem` item) tid) targets
      MV.write acc i (m {monkeyItems = Seq.empty}, count + length (m.monkeyItems))

    targetMonkey :: Monkey -> Item -> (MonkeyId, Item)
    targetMonkey m item = do
      let item' = m.monkeyOp item `div` 3
          m' = m.monkeyTest item'
      (m', item')

    addItem :: (Monkey, Int) -> Item -> (Monkey, Int)
    addItem (m, c) i = (m {monkeyItems = m.monkeyItems |> i}, c)

readInt :: ByteString -> Int
readInt s = case B.readInt s of
  Just (x, _) -> x
  Nothing -> error $ "failed to parse to int: " <> B.unpack s
