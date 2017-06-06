--module tuto where
import Test.QuickCheck
import Data.Char
import Prelude hiding (and, reverse, replicate, filter)

absolute :: Int -> Int
absolute x = if x < 0 then -x else x

prop_absolute a b = absolute a *absolute b == absolute(a*b)

data Tree = Tip Int | Branch Tree Tree
  deriving (Show)
a = Branch (Tip 1) (Tip 2)

--list comprehension
greater :: String -> Bool
greater line = and [ (ord x) >= (ord '5') | x <- line, x `elem` "0123456789"]

--recursion
greater' :: String -> Bool
greater' [] = True
greater' (x:xs) = if x `elem` "01234" then False else greater' xs

--higher order function
greater'' :: String -> Bool
greater'' line = foldr (\x xs -> if x `elem` "01234" then False else xs) True line

--quickCheck
prop_greater :: String -> Bool
prop_greater line = (greater line) == (greater' line)

--list comprehesions
swap :: [a] -> [a]
swap [] = []
swap line = concat [ x2:[x1] | let z = zip line [1..], (x1, y1) <- z, (x2, y2) <- z, odd y1, even y2, y2 - y1 == 1]

--recursion
swap' :: [a] -> [a]
swap' [] = []
swap' (x:[]) = [x]
swap' (x:y:xs) = y:x: (swap' xs)  

--recursion
triangle :: Int -> Int
triangle 0 = 0
triangle n = n + triangle (n - 1)

--higher order function
triangle' :: Int -> Int
triangle' n = foldr (+) 0 [1..n]

--list comprehension
triangle'' :: Int -> Int
triangle'' n = sum [ x | x <- [1..n]]

--define following use recursion
and :: [Bool] -> Bool
and [] = True
and (x : xs) = if x == False then False else and xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : (replicate (n - 1) x)

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) = if f x then x : (filter f xs) else filter f xs

--insertion sort
insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n all@(x:xs) = if n <= x then n:all else x:(insert n xs)

--insertion sort
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)