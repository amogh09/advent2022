module Advent.Day13 (solve1) where

import Advent.Util (bshow, splitAtEmptyLines)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

solve1 :: ByteString -> ByteString
solve1 =
  bshow
    . sum
    . fmap fst
    . filter ((== LT) . uncurry compare . snd)
    . zip [1 :: Int, 2 ..]
    . parsePairs

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
comp (PList (x : xs)) (PList (y : ys)) =
  case comp x y of
    EQ -> comp (PList xs) (PList ys)
    r -> r
comp (PList xs) (PInt y) = comp (PList xs) (PList [PInt y])
comp (PInt x) (PList ys) = comp (PList [PInt x]) (PList ys)

data StackElem = POpen | StackPacket Packet

elemPacket :: StackElem -> Packet
elemPacket (StackPacket p) = p
elemPacket _ = error "stack element is not a packet"

isOpen :: StackElem -> Bool
isOpen POpen = True
isOpen _ = False

parsePairs :: ByteString -> [(Packet, Packet)]
parsePairs = fmap parsePair . splitAtEmptyLines

parsePair :: [ByteString] -> (Packet, Packet)
parsePair [p1, p2] = (parsePacket p1, parsePacket p2)
parsePair ps = error $ "invalid input: expected 2 lines but found " <> show (length ps)

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
              f (StackPacket (PList . reverse $ (elemPacket <$> ls)) : rs) (B.tail s)
            _ -> error "invalid input: mistmatching parens"
      | otherwise = case B.readInt s of
          Just (x, rest) -> f (StackPacket (PInt x) : xs) rest
          Nothing -> error $ "invalid input: could not parse to int: " <> B.unpack s
