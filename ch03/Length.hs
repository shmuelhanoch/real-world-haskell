length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

main :: IO ()
main = do
  l <- getLine
  print $ length l
  print $ length' l
