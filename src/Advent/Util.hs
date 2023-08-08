module Advent.Util
  ( splitAtEmptyLines,
    readInt,
    maybeHead,
    bshow,
    readInteger,
    compareMaybe,
    third,
    uniq,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Split (splitWhen)
import qualified Data.Set as Set

splitAtEmptyLines :: ByteString -> [[ByteString]]
splitAtEmptyLines = splitWhen B.null . B.lines

readInt :: ByteString -> Int
readInt s =
  case B.readInt s of
    Just (x, s') | B.null s' -> x
    _ -> error $ "invalid input: failed to parse to int: " <> B.unpack s

readInteger :: ByteString -> Integer
readInteger s =
  case B.readInteger s of
    Just (x, s') | B.null s' -> x
    _ -> error $ "invalid input: failed to parse to int: " <> B.unpack s

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

bshow :: Show a => a -> ByteString
bshow = B.pack . show

compareMaybe :: Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing _ = GT
compareMaybe _ Nothing = LT
compareMaybe (Just x) (Just y) = compare x y

third :: (a, b, c) -> c
third (_, _, z) = z

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList
