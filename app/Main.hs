module Main where

import Advent.Solver (solve)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)

main :: IO ()
main = do
  [year, day, part, inputType] <- getArgs
  solve year day part inputType >>= B.putStrLn
