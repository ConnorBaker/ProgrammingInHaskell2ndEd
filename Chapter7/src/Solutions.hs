{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists -Wno-safe #-}

module Solutions
    ( applyFilterAndFnToList
    , all
    , any
    , takeWhile
    , dropWhile
    , map'
    , filter'
    , dec2Int
    , curry
    , uncurry
    , unfold
    , chop8
    , map''
    , iterate'
    , bin2Int
    , int2Bin
    , make8
    , encode
    , encodeWithParityBit
    , decode
    , decodeWithParityBit
    , channelNoisy
    , transmitNoisy
    , helloWorld
    ) where

-- Allows us to avoid a namespace conflict with our own implementation
import Prelude hiding
    ( all
    , any
    , takeWhile
    , dropWhile
    , curry
    , uncurry
    )

import Data.Char

-- #1
applyFilterAndFnToList :: (a -> Bool) -> (a -> b) -> [a] -> [b]
applyFilterAndFnToList p f xs = map f (filter p xs)

-- #2 Part a
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- #2 Part b
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- #2 Part c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

-- #2 Part d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p x'@(x:xs)
    | p x       = dropWhile p xs
    | otherwise = x'

-- #3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
    foldr (\x xs ->
            if p x then
                x:xs
            else
                xs)
          []

-- #4
dec2Int :: [Int] -> Int
dec2Int = foldl (\xs x -> 10*xs + x) 0

-- #5
curry :: ((a,b) -> c) -> a -> b -> c
curry f a b = f (a,b)
-- Alternatively:
-- curry f = \a b -> f (a,b)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b
-- Alternatively:
-- uncurry f = \(a,b) -> f a b

-- #6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- #7
type Bit = Int

bin2Int :: [Bit] -> Int
bin2Int = foldr (\x y -> x + 2*y) 0

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
        encoded   = encode s
        parityBit = sum encoded `mod` 2

-- Using previously defined chop8 from #6

decode :: [Bit] -> String
decode = map (chr . bin2Int) . chop8

decodeWithParityBit :: [Bit] -> String
decodeWithParityBit list =
    if last list == sum (init list) `mod` 2
        then decode $ init list
        else error "Parity bit doesn't match input actual string"

-- #8
-- Discard the leading bit
channelNoisy :: [Bit] -> [Bit]
channelNoisy = tail

transmitNoisy :: String -> String
transmitNoisy = decodeWithParityBit
              . channelNoisy
              . encodeWithParityBit


helloWorld :: IO ()
helloWorld = putStrLn "someFunc"
