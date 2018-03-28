import Data.List (genericLength)

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

main :: IO ()
main = print $ mean [1, 2, 3, 4, 5]
