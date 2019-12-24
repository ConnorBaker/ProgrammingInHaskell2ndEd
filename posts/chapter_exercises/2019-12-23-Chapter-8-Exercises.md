---
title: Chapter 8 Exercises
author: [Connor Baker]
date: Compiled on \today\ at \currenttime
subject: Programming in Haskell, 2nd ed.
keywords: [Chapter 8, Notes]
subtitle: Declaring Types and Classes
lang: en
titlepage: true
titlepage-color: FFFFFF
titlepage-text-color: 0d47a1
titlepage-rule-color: 0d47a1
titlepage-rule-height: 2
papersize: letter
fontsize: 10pt
listings-disable-line-numbers: true
table-use-row-colors: true
footnotes-pretty: true
footer-right: \thepage\ of \pageref{LastPage}
header-right: \today
header-includes:
  - \setcounter{page}{0} # So that the titlepage is the zeroth page
  - \lstset{breaklines=true}
  - \lstset{postbreak=\raisebox{0ex}[0ex][0ex]{\ensuremath{\color{blue}\hookrightarrow\space}}}
  - \usepackage{datetime}
  - \settimeformat{ampmtime}
  - \usepackage{lastpage}
---

## 8.9 Exercises

1. In a similar manner to the function `add`, define a recursive multiplication function `mult :: Nat -> Nat -> Nat` for the recursive type of natural numbers:

    Hint: make use of add in your definition.

2. Although not included in appendix B, the standard prelude defines

    `data Ordering = LT | EQ | GT`

    together with a function

    `compare :: Ord a => a -> a -> Ordering`

    that decides if one value in an ordered type is less than (`LT`), equal to (`EQ`), or greater than (`GT`) another value. Using this function, redefine the function `occurs :: Ord a => a -> Tree a -> Bool` for search trees. Why is this new definition more efficient than the original version?

3. Consider the following type of binary trees:

    `data Tree a = Leaf a | Node (Tree a) (Tree a)`

    Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differs by at most one, with leaves themselves being trivially balanced. Define a function `balanced :: Tree a -> Bool` that decides if a binary tree is balanced or not.

    Hint: first define a function that returns the number of leaves in a tree.

4. Define a function `balance :: [a] -> Tree a` that converts a non-empty list into a balanced tree.

   Hint: first define a function that splits a list into two halves whose length differs by at most one.

5. Given the type declaration

    `data Expr = Val Int | Add Expr Expr`

    define a higher-order function

    `folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a`

    such that `folde f g` replaces each `Val` constructor in an expression by the function `f`, and each `Add` constructor by the function `g`.

6. Using `folde`, define a function `eval :: Expr -> Int` that evaluates an expression to an integer value, and a function `size :: Expr -> Int` that calculates the number of values in an expression.

7. Complete the following instance declarations:

    `instance Eq a => Eq (Maybe a) where ...`

    `instance Eq a => Eq [a] where ...`

8. Extend the tautology checker to support the use of logical disjunction ($\lor$) and equivalence ($\iff$) in propositions.

9. Extend the abstract machine to support the use of multiplication.
