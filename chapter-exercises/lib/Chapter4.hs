module Chapter4 where

-- Problem 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (((length xs) + 1) `div` 2) xs

-- Problem 2
thirdHeadAndTail :: [a] -> a
thirdHeadAndTail = head . tail . tail

thirdIndex :: [a] -> a
thirdIndex xs = xs !! 2

thirdPatternMatching :: [a] -> a
thirdPatternMatching (_ : _ : x : _) = x

-- Problem 3
safetailConditional :: [a] -> [a]
safetailConditional xs =
    if null xs
        then []
        else tail xs

safetailGuarded :: [a] -> [a]
safetailGuarded xs
    | null xs = []
    | otherwise = tail xs

safetailPatternMatch :: [a] -> [a]
safetailPatternMatch [] = []
safetailPatternMatch (_ : xs) = xs

-- Problem 4
or1 :: Bool -> Bool -> Bool
True `or1` True = True
True `or1` False = True
False `or1` True = True
False `or1` False = True

or2 :: Bool -> Bool -> Bool
False `or2` False = False
_ `or2` _ = True

or3 :: Bool -> Bool -> Bool
False `or3` b = b
True `or3` _ = True

or4 :: Bool -> Bool -> Bool
a `or4` b
    | a == b = b
    | otherwise = True

-- Problem 5
formalConjunction :: Bool -> Bool -> Bool
formalConjunction a b =
    if a
        then
            if b
                then True
                else False
        else False

-- Problem 6
formalConjunctionProblem6 :: Bool -> Bool -> Bool
formalConjunctionProblem6 a b =
    if a
        then b
        else False

-- Problem 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- Problem 8
luhnDouble :: Int -> Int
luhnDouble n
    | n <= 5 = 2 * n
    | otherwise = 2 * n - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
    (luhnDouble a + b + luhnDouble c + d)
        `mod` 10 == 0
