-- A BST implementation, instead of just a simple tree

import Data.Maybe 

newtype Tree a = Tree (Maybe (a, Tree a, Tree a))
               deriving (Show)

empty :: (Ord a) => Tree a
empty = Tree Nothing

singleton :: (Ord a) => a -> Tree a
singleton x = Tree $ Just (x, empty, empty)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert empty

insert :: (Ord a) => a -> Tree a -> Tree a
insert x (Tree Nothing) = singleton x
insert x (Tree (Just (root, left, right)))
  | x < root  = Tree (Just (root, insert x left, right))
  | otherwise = Tree (Just (root, left, insert x right))

-- serialize a BST to a sorted list
toList :: (Ord a) => Tree a -> [a]
toList t =
  if isJust m
    then fromJust m : toList (removeMin t)
    else []
  where
    m = treeMin t

treeMin :: (Ord a) => Tree a -> Maybe a
treeMin (Tree Nothing) = Nothing
treeMin (Tree (Just (root, Tree Nothing, _))) = Just root
treeMin (Tree (Just (_, left, _))) = treeMin left

removeMin :: (Ord a) => Tree a -> Tree a
removeMin (Tree Nothing) = Tree Nothing
removeMin (Tree (Just (_, Tree Nothing, Tree Nothing))) = empty
removeMin (Tree (Just (_, Tree Nothing, Tree (Just (newRoot, newLeft, newRight))))) = Tree (Just (newRoot, newLeft, newRight))
removeMin (Tree (Just (root, left, right))) = Tree (Just(root, removeMin left, right))

-- some nice way to test all of this :)
sortWithTree :: (Ord a) => [a] -> [a]
sortWithTree = toList . fromList

main :: IO ()
main = print $ sortWithTree [1, 9, 2, 8, 3, 7, 4, 6, 5]
