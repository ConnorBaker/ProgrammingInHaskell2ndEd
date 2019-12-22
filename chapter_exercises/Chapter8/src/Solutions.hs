{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists -Wno-safe #-}

module Solutions
    ( helloWorld
    ) where

import Prelude hiding
    ( all
    , any
    , takeWhile
    , dropWhile
    , curry
    , uncurry
    )

helloWorld :: IO ()
helloWorld = putStrLn "Chapter 8 Exercises"