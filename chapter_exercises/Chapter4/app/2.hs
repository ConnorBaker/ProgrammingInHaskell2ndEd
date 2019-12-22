thirdHeadAndTail :: [a] -> a
thirdHeadAndTail = head . tail . tail

thirdIndex :: [a] -> a
thirdIndex xs = xs !! 2

thirdPatternMatching :: [a] -> a
thirdPatternMatching (_:_:x:_) = x