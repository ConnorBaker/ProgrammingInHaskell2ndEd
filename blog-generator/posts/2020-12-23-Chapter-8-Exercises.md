---
title: Chapter 8 Exercises
author: Connor Baker
author-website: https://github.com/connorbaker
date: 2020-12-23
subject: Programming in Haskell, 2nd ed.
keywords: [Chapter 8, Notes]
subtitle: Declaring Types and Classes
---

## 8.9 Exercises

1. In a similar manner to the function `add`, define a recursive multiplication function `mult :: Nat -> Nat -> Nat` for the recursive type of natural numbers:

    Hint: make use of add in your definition.

    Solution:

    ```haskell
    data Nat = Zero | Succ Nat
    deriving stock Show

    add :: Nat -> Nat -> Nat
    add Zero     n = n
    add (Succ m) n = Succ (add m n)

    mult :: Nat -> Nat -> Nat
    mult Zero     _ = Zero
    mult (Succ m) n = add n (mult m n)
    ```

<!--more-->

1. Although not included in appendix B, the standard prelude defines

    `data Ordering = LT | EQ | GT`

    together with a function

    `compare :: Ord a => a -> a -> Ordering`

    that decides if one value in an ordered type is less than (`LT`), equal to (`EQ`), or greater than (`GT`) another value. Using this function, redefine the function `occurs :: Ord a => a -> Tree a -> Bool` for search trees. Why is this new definition more efficient than the original version?

    Solution:

    ```haskell
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
    ```

2. Consider the following type of binary trees:

    `data Tree a = Leaf a | Node (Tree a) (Tree a)`

    Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differs by at most one, with leaves themselves being trivially balanced. Define a function `balanced :: Tree a -> Bool` that decides if a binary tree is balanced or not.

    Hint: first define a function that returns the number of leaves in a tree.

    Solution:

    ```haskell
    numLeaves :: TwoTree a -> Int
    numLeaves (TwoLeaf _    ) = 1
    numLeaves (TwoNode l _ r) = numLeaves l + numLeaves r

    balanced :: TwoTree a -> Bool
    balanced (TwoLeaf _    ) = True
    balanced (TwoNode l _ r) = abs diff <= 1 && balanced l && balanced r
    where diff = numLeaves l - numLeaves r
    ```

3. Define a function `balance :: [a] -> Tree a` that converts a non-empty list into a balanced tree.

   Hint: first define a function that splits a list into two halves whose length differs by at most one.

   Solution:

    ```haskell
    data FourTree a = FourLeaf a | FourNode (FourTree a) (FourTree a)
    deriving stock Show

    halve :: [a] -> ([a], [a])
    halve as = splitAt midLength as where midLength = div (length as) 2

    -- Assume list is not empty
    balance :: [a] -> FourTree a
    balance []  = error "Oh no!"
    balance [a] = FourLeaf a
    balance as  = FourNode (balance l) (balance r) where (l, r) = halve as
    ```

4. Given the type declaration

    `data Expr = Val Int | Add Expr Expr`

    define a higher-order function

    `folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a`

    such that `folde f g` replaces each `Val` constructor in an expression by the function `f`, and each `Add` constructor by the function `g`.

    Solution:

    ```haskell
    data Expr = Val Int | Add Expr Expr
    deriving stock Show

    folde
    :: (Int -> a)    -- Transforms the contents of the Val constructor
    -> (a -> a -> a) -- Applied to result of the Add constructor
    -> (Expr -> a)
    folde f _ (Val n  ) = f n
    folde f g (Add a b) = g (folde f g a) (folde f g b)
    ```

5. Using `folde`, define a function `eval :: Expr -> Int` that evaluates an expression to an integer value, and a function `size :: Expr -> Int` that calculates the number of values in an expression.

    Solution:

    ```haskell
    -- Evaluates an expression to an integer value
    eval'' :: Expr -> Int
    eval'' = folde id (+)

    -- Calculates the number of values in an expression
    size :: Expr -> Int
    size = folde (const 1) (+)
    ```

6. Complete the following instance declarations:

    `instance Eq a => Eq (Maybe a) where ...`

    `instance Eq a => Eq [a] where ...`

    Solution:

    ```haskell
    -- Recall that we get /= for free as the negation of ==
    instance Eq a => Eq (Maybe a)
      where
       Nothing == Nothing = True
       Just a  == Just b  = a == b

    instance Eq a => [a]
      where
        []       == []       = True
        (x : xs) == (y : ys) = x == y && xs == ys
        _        == _        = False
    ```

7. Extend the tautology checker to support the use of logical disjunction ($\lor$) and equivalence ($\iff$) in propositions.

    Solution:

    ```haskell
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
    ```

8. Extend the abstract machine to support the use of multiplication.

    Solution:

    ```haskell
    data Expr' = Val' Int | Add' Expr' Expr' | Mul' Expr' Expr' deriving stock Show
    data Op = EVALa Expr' | ADD Int | EVALm Expr' | MUL Int deriving stock Show
    type Cont = [Op]

    value' :: Expr' -> Int
    value' = flip eval' []

    eval' :: Expr' -> Cont -> Int
    eval' (Val' n  ) c = exec c n
    eval' (Add' n m) c = eval' n (EVALa m : c)
    eval' (Mul' n m) c = eval' n (EVALm m : c)

    exec :: Cont -> Int -> Int
    exec []            n = n
    exec (EVALa y : c) n = eval' y (ADD n : c)
    exec (EVALm y : c) n = eval' y (MUL n : c)
    exec (ADD   n : c) m = exec c (n + m)
    exec (MUL   n : c) m = exec c (n * m)
    ```
