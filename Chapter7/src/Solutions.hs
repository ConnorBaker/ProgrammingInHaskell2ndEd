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

helloWorld :: IO ()
helloWorld = putStrLn "someFunc"
