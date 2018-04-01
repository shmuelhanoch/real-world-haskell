module Main where

import System.Environment

main :: IO ()
main = mainWith myFunc
  where mainWith f = do
          args <- getArgs
          case args of
            [input, output] -> interactWith f input output
            _               -> error "Usage: Transpose [Input] [Output]"
        myFunc = unlines . transpose . lines

interactWith :: (String -> String) -> String -> String -> IO ()
interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile $ f input

transpose :: [[a]] -> [[a]]
transpose [[], _] = []
transpose xss = (map head xss) : (transpose (map tail xss))
