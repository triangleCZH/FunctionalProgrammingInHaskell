module Lecture6 where

import Prelude hiding (takeWhile, dropWhile, Nothing, Just, Maybe)

-- Final functions for Higher-order functions:

-- takeWhile even [2,4,6,7,9,7,6]
-- [2,4,6]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []      = []
takeWhile p (x:xs) 
  | p x        = x : takeWhile p xs
  | otherwise  = []

-- dropWhile even [2,4,6,7,9,7,6]
-- [7,9,7,6]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []        = []
dropWhile p l@(x:xs)  =
  if (p x) then dropWhile p xs else l



-- Datatypes and Classes examples

type Pos = (Int,Int)

origin :: Pos
origin = (0,0)

type Pair a = (a,a)

mult :: Pair Int -> Int
mult (x,y) = x * y

data Boolean = F | T -- using F and T to avoid clashes 

data Shape = Circle Float | Rect Float Float deriving Show

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv m 0 = Nothing
safediv m n = Just (m `div` n)

p = case safediv 5 1 of
            Nothing -> 0
            Just x -> x + 1

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero          = 0
nat2int (Succ n)      = 1 + nat2int n

int2nat :: Int -> Nat
int2nat n | n == 0    = Zero
          | otherwise = Succ (int2nat (n - 1))

add' :: Nat -> Nat -> Nat
add' a b = int2nat (nat2int a + nat2int b)

add'' :: Nat -> Nat -> Nat
add'' Zero n = n
add'' (Succ m) n = Succ (add' m n)

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

-- 10 * (5 + 7)
e1 = Mult (Val 10) (Add (Val 5) (Val 7))

e2 = Add (Val 10) (Mult (Val 5) (Val 7))

size :: Expr -> Int
size (Val _)     = 0
size (Add e1 e2) = 1 + max (size e1) (size e2)
size (Mult e1 e2) = 1 + max (size e1) (size e2)

eval :: Expr -> Int
eval e = error "TODO!"




{-

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []     = []
takeWhile p (x:xs)
   | p x  = x : takeWhile p xs
   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
   | p x = dropWhile p xs
   | otherwise = x : xs

-}