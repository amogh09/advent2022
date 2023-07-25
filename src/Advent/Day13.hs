module Advent.Day13 (solve1, solve2) where

import Advent.Util (bshow, splitAtEmptyLines)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)

solve1 :: ByteString -> ByteString
solve1 =
  bshow
    . sum
    . fmap fst
    . filter ((== LT) . uncurry compare . snd)
    . zip [1 :: Int, 2 ..]
    . parsePairs

solve2 :: ByteString -> ByteString
solve2 s = do
  let p2 = PList [PList [PInt 2]]
      p6 = PList [PList [PInt 6]]
      sorted = sort . (++ [p2, p6]) . fmap parsePacket . filter (not . B.null) . B.lines $ s
      idx p = (+ 1) <$> elemIndex p sorted
  bshow . fromMaybe 0 $ (*) <$> idx p2 <*> idx p6

type Pair = (Packet, Packet)

data Packet = PInt Int | PList [Packet] deriving (Show)

instance Eq Packet where
  x == y = compare x y == EQ

instance Ord Packet where
  compare = comp

comp :: Packet -> Packet -> Ordering
comp (PInt x) (PInt y) = compare x y
comp (PList []) (PList []) = EQ
comp (PList []) (PList _) = LT
comp (PList _) (PList []) = GT
comp (PList (x : xs)) (PList (y : ys)) = case comp x y of
  EQ -> comp (PList xs) (PList ys)
  r -> r
comp (PList xs) (PInt y) = comp (PList xs) (PList [PInt y])
comp (PInt x) (PList ys) = comp (PList [PInt x]) (PList ys)

parsePairs :: ByteString -> [Pair]
parsePairs = fmap parsePair . splitAtEmptyLines

parsePair :: [ByteString] -> Pair
parsePair [p1, p2] = (parsePacket p1, parsePacket p2)
parsePair ps = error $ "invalid input: expected 2 lines but found " <> show (length ps)

-- | Type of elements in stack used for parsing packets
data StackElem = POpen | StackPacket Packet

elemPacket :: StackElem -> Packet
elemPacket (StackPacket p) = p
elemPacket _ = error "stack element is not a packet"

isOpen :: StackElem -> Bool
isOpen POpen = True
isOpen _ = False

parsePacket :: ByteString -> Packet
parsePacket = elemPacket . head . f []
  where
    f :: [StackElem] -> ByteString -> [StackElem]
    f xs s
      | B.null s = xs
      | B.head s == ',' = f xs (B.tail s)
      | B.head s == '[' = f (POpen : xs) (B.tail s)
      | B.head s == ']' =
          case break isOpen xs of
            (ls, _ : rs) ->
              f (StackPacket (PList . reverse . fmap elemPacket $ ls) : rs) (B.tail s)
            _ -> error "invalid input: mistmatching parens"
      | otherwise = case B.readInt s of
          Just (x, rest) -> f (StackPacket (PInt x) : xs) rest
          Nothing -> error $ "invalid input: could not parse to int: " <> B.unpack s
