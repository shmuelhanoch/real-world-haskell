import Control.Monad (foldM)
import Data.Char (ord, isSpace)
import Prelude hiding (concat, takeWhile, any, cycle, words, unlines)


-- Safe list methods

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast = foldl (\_ a -> Just a) Nothing

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = Just (x : fromJust (safeInit xs))
  where
    fromJust (Just a) = a


-- split list based on predicate

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p = foldl (\xss x ->
                       if p x
                       then (init xss) ++ [((last xss) ++ [x])]
                       else xss ++ [[x]]) [[]]


-- parse an integer from string

isDigit :: Char -> Bool
isDigit c = let n = (ord c - ord '0')
            in n >=0 && n <= 9

-- no checks here! assumes valid input
toDigit :: Char -> Int
toDigit c = (ord c) - (ord '0')

asInt :: String -> Either String Int
asInt "" = Right 0
asInt s
  | head s == '+' = asInt_fold $ tail s
  | head s == '-' = negate <$> asInt_fold (tail s)
  | otherwise = asInt_fold s

asInt_fold :: String -> Either String Int
asInt_fold = foldM (\acc c ->
                      if isDigit c
                        then Right $ 10 * acc + (toDigit c)
                        else Left ("non-digit '" ++ [c] ++ "'")) 0


concat :: [[a]] -> [a]
concat = foldr (++) []


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else []) []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p  (x:xs) = foldl step [[x]] xs
  where
    step gs y
      | p y (last (lg)) = (init gs) ++ [(lg ++ [y])]
      | otherwise = gs ++ [[y]]
        where
          lg = last gs

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr (||) False $ map p xs

--cycle :: [a] -> [a] couldn't find an elegant solution

-- use tuple to save some state. the first space in a group of consecutive spaces
-- requires a differnt treatment
words :: String -> [String]
words = merge . foldr step ([], [])
  where
    merge (x, y) = x:y
    step c acc@(f, s)
      | isSpace c = if null f
                      then acc
                      else ([], f:s)
      | otherwise = (c:f, s)

unlines :: [String] -> String
unlines = foldr (\cs acc -> cs ++ '\n' : acc) []

main :: IO ()
main = undefined
