module Main where

import Data.Char (isSpace)

main :: IO ()
main = interactWith (fst . break isSpace)

interactWith :: (String -> String) -> IO ()
interactWith f =
  let loop = do
      s <- getLine
      if (s == "quit")
        then return ()
        else putStrLn (f s) >> loop
  in loop
