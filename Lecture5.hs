module Lecture5 where

import Prelude hiding (concat, map, filter, foldr)

import Data.Char


concat :: [[a]] -> [a]
concat xxs = [ x | xs <- xxs, x <- xs ]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

prime  :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x]

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x']

-- Higher-order-functions

twice :: (a -> a) -> (a -> a)
twice f = \x -> f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [ f x | x <- xs]

mapR :: (a -> b) -> [a] -> [b]
mapR f []     = [] 
mapR f (x:xs) = f x : mapR f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op v []     = v
foldr op v (x:xs) = x `op` (foldr op v xs)

mySum :: [Int] -> Int
mySum = foldr (+) 0

myProduct :: [Int] -> Int
myProduct = foldr (*) 1

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- length and reverse as folds

len = foldr (\_ y -> 1 + y) 0

rev = foldr (\x y -> y ++ [x]) []
