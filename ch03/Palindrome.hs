toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome (x:xs) = (x:xs) ++ reverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

main :: IO ()
main = do
  print $ toPalindrome [1, 2, 3, 4, 5]
  print $ isPalindrome [1, 2, 3, 1]
  print $ isPalindrome [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1]
