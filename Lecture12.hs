data Op = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr

apply :: Op → Int → Int → Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

choices :: [a] ->[[a]]
choices = 

valid :: Op → Int → Int → Bool
--return positive integer is valid
valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid _ _ _ = True
--eval::
eval (Val n) = [n]
eval (App op e1 e2) = [ apply op x y | x <- eval e1, y <- eval e2, valid op x y]

values :: Expr → [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = or [values e == l | l <- choices ns] && eval e == [n]
--go on all the choices on the list, check if at least match it and 

solutions ns n = [ e | c <- choices]

exprs :: [Int] → [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) ← split ns
           , l ← exprs ls
           , r ← exprs rs
           , e ← combine l r] --call exprs recursively

