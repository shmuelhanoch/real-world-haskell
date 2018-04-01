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

main :: IO ()
main = undefined
