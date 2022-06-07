module Chapter9 where

import Numeric.Natural (Natural)

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Natural -> Natural -> Bool
valid Sub x y = x > y
valid Div x y = mod x y == 0
valid _ _ _ = True -- Add and Mul closed on Nats

apply :: Op -> Natural -> Natural -> Natural
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

data Expr = Val Natural | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
      where
        brak (Val n) = show n
        brak e = "(" ++ show e ++ ")"

values :: Expr -> [Natural]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Natural]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Natural] -> Natural -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

splits :: [a] -> [([a], [a])]
splits =
    foldr
        (\x -> concatMap (\(lxs, rxs) -> [(x : lxs, rxs), (lxs, x : rxs)]))
        [([], [])]