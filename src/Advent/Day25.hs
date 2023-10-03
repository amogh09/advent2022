module Advent.Day25 (solve1) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')

type SNAFU = String

snafuFacorize :: Integral t => t -> [t]
snafuFacorize 0 = []
snafuFacorize x = do
  let (q, r) = snafuDivMod x
  r : snafuFacorize q

snafuBit :: (Eq a, Num a, Show a) => a -> Char
snafuBit 2 = '2'
snafuBit 1 = '1'
snafuBit 0 = '0'
snafuBit (-1) = '-'
snafuBit (-2) = '='
snafuBit x = error $ "Invalid SNAFU bit: " <> show x

toSNAFU :: Integer -> SNAFU
toSNAFU 0 = "0"
toSNAFU x = reverse . fmap snafuBit . snafuFacorize $ x

snafuDivMod :: Integral b => b -> (b, b)
snafuDivMod x = do
  let (q, r) = x `divMod` 5
  if r > 2 then (q + 1, r - 5) else (q, r)

fromSNAFU :: Num a => SNAFU -> a
fromSNAFU = foldl' (\s x -> s * 5 + fromSNAFUBit x) 0
  where
    fromSNAFUBit '2' = 2
    fromSNAFUBit '1' = 1
    fromSNAFUBit '0' = 0
    fromSNAFUBit '-' = -1
    fromSNAFUBit '=' = -2
    fromSNAFUBit x = error $ "invalid SNAFU bit" <> show x

solve1 :: B.ByteString -> B.ByteString
solve1 = B.pack . toSNAFU . sum . fmap (fromSNAFU . B.unpack) . B.lines
