{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists #-}

module Solutions
    ( applyFilterAndFnToList 
    , all
    , any
    , takeWhile
    , dropWhile
    , map'
    , filter'
    , helloWorld
    ) where

-- Allows us to avoid a namespace conflict with our own implementation
import Prelude hiding
    ( all
    , any
    , takeWhile
    , dropWhile
    -- , all
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
    | p x         = x : takeWhile p xs
    | otherwise   = []

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
            if (p x) then 
                x:xs 
            else 
                xs)
          []

helloWorld :: IO ()
helloWorld = putStrLn "someFunc"
