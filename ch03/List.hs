data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = (x: toList xs)


main :: IO ()
main = do
  print $ fromList [1, 2, 3, 4, 5]
  print $ toList $ fromList [1, 2, 3, 4, 5]
