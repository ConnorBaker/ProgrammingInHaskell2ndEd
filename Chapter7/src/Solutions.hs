{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists #-}

module Solutions
    ( helloWorld
    ) where

-- Allows us to avoid a namespace conflict with our own implementation
-- import Prelude hiding
--     (
--     ,
--     )

helloWorld :: IO ()
helloWorld = putStrLn "someFunc"
