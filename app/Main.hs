module Main where

import Advent.Solver (solve)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)

main :: IO ()
main = do
  [day, part, inputType] <- getArgs
  solve day part inputType >>= B.putStrLn
