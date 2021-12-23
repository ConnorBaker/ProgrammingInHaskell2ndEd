module Chapter5 where

import Data.Char (chr, isLower, isUpper, ord)

-- Problem 1
sum100ConsecSquares :: Int
sum100ConsecSquares = sum [x ^ 2 | x <- [1 .. 100]]

-- Problem 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Problem 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Problem 4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1 .. n]]

-- Problem 5
pyths :: Int -> [(Int, Int, Int)]
pyths n =
    [ (a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2
    ]

-- Problem 6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n =
    [ x | x <- [1 .. n], x == sum (factors x) - x
    ]

-- Problem 7
list1 :: [(Int, Int)]
list1 = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

-- Problem 8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0 ..])

-- Problem 9
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Problem 10
-- Returns the number of lowercase characters
lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

-- Returns the number of occurences
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- Converts a character into an integral value
-- Specifically, it maps [a..z]++[A..Z] to the interval [0..51]
let2int :: Char -> Int
let2int c
    | 'a' <= c && c <= 'z' = ord c - ord 'a'
    | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 26
    | otherwise = ord c

-- Converts an integral value into a character
int2let :: Int -> Char
int2let n
    | n <= 25 = chr (ord 'a' + n)
    | n <= 51 = chr (ord 'A' - 26 + n)
    | otherwise = chr n

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = int2let (((let2int c + n - 26) `mod` 26) + 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Frequency distribution of lowercase english letters
table :: [Float]
table =
    [ 8.1
    , 1.5
    , 2.8
    , 4.2
    , 12.7
    , 2.2
    , 2.0
    , 6.1
    , 7.0
    , 0.2
    , 0.8
    , 4.0
    , 2.4
    , 6.7
    , 7.5
    , 1.9
    , 0.1
    , 6.0
    , 6.3
    , 9.0
    , 2.8
    , 1.0
    , 2.4
    , 0.2
    , 2.0
    , 0.1
    ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Only necessary to count the number of lowercase letters present.
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
