{-# OPTIONS_GHC -Weverything -Wno-missing-import-lists -Wno-safe #-}
{-# LANGUAGE DerivingStrategies #-}

module Solutions
  ( helloWorld
  , add
  , mult
  , occurs
  , occurs'
  , numLeaves
  , balanced
  , halve
  , balance
  , folde
  )
where

import           Prelude                 hiding ( )

-- Problem One
data Nat = Zero | Succ Nat
  deriving stock Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ m) n = add n (mult m n)

-- Problem Two
-- Tree for problem two
data TwoTree a = TwoLeaf a | TwoNode (TwoTree a) a (TwoTree a)
  deriving stock Show

occurs :: Ord a => a -> TwoTree a -> Bool
occurs x (TwoLeaf v) = x == v
occurs x (TwoNode l v r) | x < v     = occurs x l
                         | x > v     = occurs x r
                         | otherwise = True

-- Use compare :: Ord a => a -> a -> Ordering
-- This version is more efficient because unlike the guards above,
-- there is only a single comparison performed, whereas the above
-- could require two.
occurs' :: Ord a => a -> TwoTree a -> Bool
occurs' x (TwoLeaf v    ) = x == v
occurs' x (TwoNode l v r) = case compare x v of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

-- Problem Three
numLeaves :: TwoTree a -> Int
numLeaves (TwoLeaf _    ) = 1
numLeaves (TwoNode l _ r) = numLeaves l + numLeaves r

balanced :: TwoTree a -> Bool
balanced (TwoLeaf _    ) = True
balanced (TwoNode l _ r) = abs diff <= 1 && balanced l && balanced r
  where diff = numLeaves l - numLeaves r

-- Problem Four
-- Tree for problem four
data FourTree a = FourLeaf a | FourNode (FourTree a) (FourTree a)
  deriving stock Show

halve :: [a] -> ([a], [a])
halve as = splitAt midLength as where midLength = div (length as) 2

-- Assume list is not empty
balance :: [a] -> FourTree a
balance []  = error "Oh no!"
balance [a] = FourLeaf a
balance as  = FourNode (balance l) (balance r) where (l, r) = halve as

-- Problem Five
data Expr = Val Int | Add Expr Expr

folde
  :: (Int -> a)    -- Transforms the contents of the Val constructor
  -> (a -> a -> a) -- Applied to result of the Add constructor
  -> (Expr -> a)
folde f _ (Val n) = f n 
folde f g (Add a b) = g (folde f g a) (folde f g b)

helloWorld :: IO ()
helloWorld = putStrLn "Chapter 8 Exercises"
