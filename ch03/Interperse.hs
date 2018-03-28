myInterperse :: a -> [a] -> [a]
myInterperse _ [] = []
myInterperse _ [x] = [x]
myInterperse s (x:xs) = x:s:myInterperse s xs 

main :: IO ()
main = print $ myInterperse '1' "hello"
