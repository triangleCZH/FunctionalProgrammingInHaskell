import Prelude hiding (dropWhile)
import Parsing
import Data.Char
double x = x + x

sum' [] = 0
sum' (x:xs) = x + sum' xs

--selection sort
f [] = []
f (x:xs) = f sm ++ [x] ++ f lg
  where sm = [n | n <- xs, n <= x]
        lg = [n | n <- xs, n > x]

drop' n [] = []
drop' 1 (x:xs) = xs
drop' n (x:xs) = drop' (n - 1) xs

cal a b = a / b

division xs = 2 `div` (length xs)

msort xs
    | xs == [] = []
    | length xs == 1 = xs
    | otherwise = let (a, b) = splitAt (length xs `div` 2) xs
      in merge (msort a) (msort b)
        where
          merge xs [] = xs
          merge [] xs = xs
          merge a1@(x:xs) a2@(y:ys)
            | x > y =  [y] ++ merge a1 ys
            | otherwise = [x] ++ merge a2 xs

concat' xss = [ x | xs <- xss, x <- xs ]

pair xs = zip xs $ tail xs

sorted xs = all (\(x,y) -> x <= y) [(x,y) | (x , y) <- pair xs]

count x' xs = foldr (\x xx -> if x' == x then xx + 1 else xx) 0 xs

scalar xs ys = [ (xs !! x) * (ys !! x) | x <- [0.. (length xs - 1)]]

eat ("a":xs) = xs 
eat x = x 


pyths x = [ (a, b, x) | a <- [1..x], b <- [1..x], a^2 + b^2 == x^2]

factor n = [ x | x <- [1..n-1], n `mod` x == 0]

perfect n = [ a | a <- [2..n], sum (factor a) == a]

rev :: [a] -> [a]
rev = foldr (\x xx -> xx ++ [x]) []

compo = sum . factor

dropWhile p [] = []
dropWhile p (x:xs) 
   | p x = dropWhile p xs
   | otherwise = x:xs


numTest a b = a + b 
eqTest a b = a == b
ordTest a b = a == b || a > b

type Pos a = (a, a)
data Bullet = Shoot | Fail deriving Show
shoot Shoot = 1
shoot Fail = 0

help _ = Shoot

data NAT = ZERO | SUCC (NAT)

nat2int ZERO = 0
nat2int (SUCC n) = 1 + nat2int n

data Tree a = Leaf a | Node (Tree a) a (Tree a)

complete (Leaf a) = True
complete x@(Node l v r) = (2 ^ (depth x) - 1) == (count x)
    where depth (Leaf a) = 1
          depth (Node l v r) = 1 + depth l
          count (Leaf a) = 1
          count (Node l v r) = 1 + count l + count r

n = a `div` length b
 where a = 10
       b = [1..5]
{-}
man n p = manyN n p +++ return []

manyN 1 p = do x <- p 
               return x
manyN n p = do x <- p
               xx <- man (n-1) p
               return (x ++ xx)-}

man 0 _ = return []
man n p = do v <- p 
             vs <- man (n - 1) p
             return (v:vs)

manh c = do xx <- man (c + 1) (sat (\x -> x == ' ' || x == '*'))
            char '\n'
            return (count '*' xx)

parseGrid (a, b) = do xs <- man (b + 1) (manh a)
                      return $ sum xs

{-}
expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             return (e + t) 
          +++ return t

term :: Parser Int
term = do f <- factorr
          do char '*'
             t <- term
             return (t * f) 
          +++ return f

factorr :: Parser Int
factorr = 
         do d <- digit
            return (digitToInt d) 
         +++ do char '('
                e <- expr
                char ')'
                return e-}

data Op = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid _ _ _ = True

eval (Val n) = [n]
eval (App op e1 e2) = [ apply op x y | x <- eval e1, y <- eval e2, valid op x y]

interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

subs [] = [[]]
subs (x:xs) = (subs xs) ++ map (x:) (subs xs)

choices xs = concat (map perms (subs xs))

values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution e ns n = or [ values e == l | l <- choices ns] && eval e == [n]

doublel xs = concat [ [a, a] | a <- xs]

getL = do x <- getLine
          return x 

test x = case x of 
      [] -> 1
      (a:_) | even a-> 2 
      _ -> 3