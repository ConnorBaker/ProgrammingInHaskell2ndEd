list1 :: [(Int,Int)]
list1 =  concat [[(x,y) | y <- [3,4]] | x <- [1,2]]
