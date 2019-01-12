halve :: [a] -> ([a],[a])
halve xs = splitAt (((length xs) + 1) `div` 2) xs