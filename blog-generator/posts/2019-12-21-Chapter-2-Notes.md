---
title: Chapter 2
author: Connor Baker
author-website: https://github.com/connorbaker
date: 2019-12-21
subject: Programming in Haskell, 2nd ed.
keywords: [Chapter 2, Notes]
subtitle: Programming in Haskell, 2nd ed.
---

## 2.3 Standard Prelude

Haskell has a large standard library. A portion of the standard library, called the Prelude, is included by default in all Haskell packages. Prelude includes many useful and commonly used functions, so it serves as a core part of the standard library. It includes functions such as `+` and `*` as well as several functions used on lists (like `head`, `tail`, `take`, and `drop`).

<!--more-->

## 2.4 Function Application

> The notation for function application is a bit different than is common in mathematics. These differences are highlighted by the table below:
>
> Mathematics | Haskell |
> :-: | :-: |
> $f(x)$ | `f x` |
> $f(x,y)$ | `f x y` |
> $f(g(x))$ | `f (g x)` |
> $f(x,g(y))$ | `f x (g y)` |
> $f(x)g(y)$ | `f x * g y` |
>
> Excerpt from: Graham Hutton. "Programming in Haskell" (2nd ed.).

## 2.5 Haskell Scripts

### Naming requirements

> When defining a new function, the names of the function and its arguments must begin with a lower-case letter, but can then be followed by zero or more letters (both lower- and upper-case), digits, underscores, and forward single quotes.
>
> Excerpt from: Graham Hutton. "Programming in Haskell" (2nd ed.).

### The layout rule

> Within a script, each definition at the same level must begin in precisely the same column. This layout rulemakes it possible to determine the grouping of definitions from their indentation.
>
> Excerpt from: Graham Hutton. "Programming in Haskell" (2nd ed.).
