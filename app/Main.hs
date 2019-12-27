module Main where

import           Lib

main :: IO ()
main = do
  myApiApplication ["time", "EST"] >>= putStrLn
  myApiApplication ["date"] >>= putStrLn
