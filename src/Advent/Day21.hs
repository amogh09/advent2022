module Advent.Day21 (solve1, solve2) where

import Advent.Util (bshow, readInt)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

type Name = ByteString

data Op = Add | Sub | Mul | Div deriving (Show, Eq)

data RawNumber = RawFixed Int | RawVariable Name | RawEval Op Name Name

data Number = Fixed Int | Variable Name | Eval Op Number Number

type RawNumbers = Map Name RawNumber

instance Show Number where
  show (Fixed x) = show x
  show (Variable x) = show x
  show (Eval op x y) = show (op, x, y)

toOp :: Op -> (Int -> Int -> Int)
toOp Add = (+)
toOp Sub = (-)
toOp Mul = (*)
toOp Div = div

flipOp :: Op -> Op
flipOp Add = Sub
flipOp Sub = Add
flipOp Mul = Div
flipOp Div = Mul

containsVar :: Number -> Bool
containsVar (Variable _) = True
containsVar (Fixed _) = False
containsVar (Eval _ x y) = containsVar x || containsVar y

eval :: Number -> Number
eval (Eval op x y) =
  case (eval x, eval y) of
    (Fixed fx, Fixed fy) -> Fixed (toOp op fx fy)
    (ex, ey) -> Eval op ex ey
eval x = x

parseNumber :: RawNumbers -> ByteString -> RawNumbers
parseNumber nums s =
  case B.words s of
    [name, fixed] -> Map.insert (B.dropEnd 1 name) (RawFixed $ readInt fixed) nums
    [name, n1, "+", n2] -> Map.insert (B.dropEnd 1 name) (RawEval Add n1 n2) nums
    [name, n1, "-", n2] -> Map.insert (B.dropEnd 1 name) (RawEval Sub n1 n2) nums
    [name, n1, "*", n2] -> Map.insert (B.dropEnd 1 name) (RawEval Mul n1 n2) nums
    [name, n1, "/", n2] -> Map.insert (B.dropEnd 1 name) (RawEval Div n1 n2) nums
    _ -> error "invalid input"

toNumber :: RawNumbers -> Name -> Number
toNumber nums n = maybe (error "not found") f . Map.lookup n $ nums
  where
    f (RawFixed x) = Fixed x
    f (RawVariable x) = Variable x
    f (RawEval op x y) = Eval op (toNumber nums x) (toNumber nums y)

parseNumbers :: ByteString -> RawNumbers
parseNumbers = foldl' parseNumber Map.empty . B.lines

solve1 :: ByteString -> ByteString
solve1 = bshow . eval . flip toNumber "root" . parseNumbers

solve2 :: ByteString -> ByteString
solve2 s = do
  let root = flip toNumber "root" . Map.insert "humn" (RawVariable "humn") . parseNumbers $ s
  case root of
    Eval _ x y -> bshow $ solve x y
    _ -> error "no variables found"

solve :: Number -> Number -> Number
solve (Variable _) y = eval y
solve x y | containsVar x && containsVar y = error "both expressions contains a variable"
solve x y | not (containsVar x) && not (containsVar y) = error "no expression contains a variable"
solve x y | containsVar y = solve y x
solve (Eval op l r) y | containsVar l = solve l (Eval (flipOp op) y r)
solve (Eval op l r) y | op == Add || op == Mul = solve (Eval op r l) y
solve (Eval op l r) y = solve l (Eval (flipOp op) r y)
solve x y = error $ "cannot solve for " <> show x <> " and " <> show y
