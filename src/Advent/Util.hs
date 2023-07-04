module Advent.Util (splitAtEmptyLines, readInt) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Split (splitWhen)

splitAtEmptyLines :: ByteString -> [[ByteString]]
splitAtEmptyLines = splitWhen B.null . B.lines

readInt :: ByteString -> Int
readInt s =
  case B.readInt s of
    Just (x, s') | B.null s' -> x
    _ -> error $ "invalid input: failed to parse to int: " <> B.unpack s
