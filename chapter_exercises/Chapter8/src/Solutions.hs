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
  , Expr(..)
  , folde
  , eval'
  , size
  , Prop(..)
  , Assoc
  , Subst
  , find
  , eval
  , vars
  , bools
  , substs
  , rmdups
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
  deriving stock Show

folde
  :: (Int -> a)    -- Transforms the contents of the Val constructor
  -> (a -> a -> a) -- Applied to result of the Add constructor
  -> (Expr -> a)
folde f _ (Val n  ) = f n
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- Problem Six
-- Evaluates an expression to an integer value
eval' :: Expr -> Int
eval' = folde id (+)

-- Calculates the number of values in an expression
size :: Expr -> Int
size = folde (const 1) (+)

-- Problem Seven
-- Commented out to avoid conflicting definitions
-- Recall that we get /= for free as the negation of ==
-- instance Eq a => Eq (Maybe a)
--   where
--   Nothing == Nothing = True
--   Just a  == Just b  = a == b

-- instance Eq a => [a]
--   where
--   []       == []       = True
--   (x : xs) == (y : ys) = x == y && xs == ys
--   _        == _        = False

-- Problem Eight
-- Extend the tautology checker to support the use of logical disjunction
-- (\|) and equivalence (<=>) in propositions.
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
  deriving stock Show

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

eval :: Subst -> Prop -> Bool
eval _ (Const b  ) = b
eval s (Var   x  ) = find x s
eval s (Not   p  ) = not (eval s p)
eval s (And   p q) = eval s p && eval s q
eval s (Or    p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _  ) = []
vars (Var   x  ) = [x]
vars (Not   p  ) = vars p
vars (And   p q) = vars p ++ vars q
vars (Or    p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdups (vars p)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (a : as) = if a `elem` as then rmdups as else a : rmdups as

helloWorld :: IO ()
helloWorld = putStrLn "Chapter 8 Exercises"
