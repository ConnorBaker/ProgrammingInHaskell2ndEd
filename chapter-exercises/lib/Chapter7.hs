module Chapter7 where

-- Allows us to avoid a namespace conflict with our own implementation
import Prelude hiding (
    all,
    any,
    curry,
    dropWhile,
    takeWhile,
    uncurry,
 )

import Data.Char (chr, ord)

-- Problem 1
applyFilterAndFnToList :: (a -> Bool) -> (a -> b) -> [a] -> [b]
applyFilterAndFnToList p f xs = map f (filter p xs)

-- Problem 2 Part a
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- Problem 2 Part b
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- Problem 2 Part c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
    | p x = x : takeWhile p xs
    | otherwise = []

-- Problem 2 Part d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p x'@(x : xs)
    | p x = dropWhile p xs
    | otherwise = x'

-- Problem 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
    foldr
        ( \x xs ->
            if p x
                then x : xs
                else xs
        )
        []

-- Problem 4
dec2Int :: [Int] -> Int
dec2Int = foldl (\xs x -> 10 * xs + x) 0

-- Problem 5
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

-- Alternatively:
-- curry f = \a b -> f (a,b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- Alternatively:
-- uncurry f = \(a,b) -> f a b

-- Problem 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- Problem 7
type Bit = Int

bin2Int :: [Bit] -> Int
bin2Int = foldr (\x y -> x + 2 * y) 0

int2Bin :: Int -> [Bit]
int2Bin 0 = []
int2Bin n = mod n 2 : int2Bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2Bin . ord)

encodeWithParityBit :: String -> [Bit]
encodeWithParityBit s = encoded ++ [parityBit]
  where
    encoded = encode s
    parityBit = sum encoded `mod` 2

-- Using previously defined chop8 from Problem 6

decode :: [Bit] -> String
decode = map (chr . bin2Int) . chop8

decodeWithParityBit :: [Bit] -> String
decodeWithParityBit list =
    if last list == sum (init list) `mod` 2
        then decode $ init list
        else error "Parity bit doesn't match input actual string"

-- Problem 8
channelNoisy :: [Bit] -> [Bit]
channelNoisy = tail

transmitNoisy :: String -> String
transmitNoisy =
    decodeWithParityBit
        . channelNoisy
        . encodeWithParityBit

-- Problem 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- Problem 10
luhnDouble :: Int -> Int
luhnDouble n
    | n <= 5 = 2 * n
    | otherwise = 2 * n - 9

luhn :: [Int] -> Bool
luhn [] = False
luhn ns = mod (sum $ altMap luhnDouble id ns) 10 == 0

helloWorld :: IO ()
helloWorld = putStrLn "Chapter 7 Exercises"
