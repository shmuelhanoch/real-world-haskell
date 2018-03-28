import Data.List (sortBy)

sortByLength :: [[a]] -> [[a]]
sortByLength  = sortBy (\lhs rhs -> compare (length lhs) (length rhs))

main :: IO ()
main = print $ sortByLength [[1, 2, 3], [3], [3, 4], [1, 2, 3, 4, 5], [6, 6, 6], [5]]
