module Tutorial2 where

-- Needed to hide existing library functions, so that
-- we can define our own versions of the functions.

import Prelude hiding (concat, and, (!!), replicate, elem)

-- Exercise 1:
-- Many of the library have a very general type. For example:
--
-- concat :: Foldable t => t [a] -> [a]
--
-- For this tutorial we are going to implement a simpler version
-- that works only on lists. So for functions such as concat
-- above you can ignore "Foldable t" and replace "t" by a list.
-- Thus the type of concat that we will implement in this
-- tutorial is:
--
-- concat :: [[a]] -> [a]

-- concat function: takes a list of lists and
-- concatenates all lists

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- and function: takes a list of booleans and
-- computes the "and" of all elements. For example:
--
-- and [x,y,z] = x && y && z

and :: [Bool] -> Bool
and []      = True
and (x:xs)  = x && and xs

-- replicate function: takes an integer "n" and a value
-- and creates a list with n elements of the same input
-- value

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n-1) x

-- list lookup operator: takes a list and an integer "n"
-- and lookups the nth element on the list. Note that
-- list index start from 0.

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
[]     !! n = error "out of bounds"
(x:xs) !! n = xs !! (n-1)

-- elem function: takes a value and a list and checks
-- whether the value is an element of the list or not.

elem :: Eq a => a -> [a] -> Bool
elem x []      = False
elem x (y:ys)  = (x == y) || elem x ys

-- Exercise 2: Fibonacci sequence:

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Exercise 3: Double list: doubles every element in the
-- list

doubleList :: [Int] -> [Int]
doubleList []     = []
doubleList (x:xs) = 2*x : doubleList xs

-- Exercise 4: Given two lists, the zipSum function
-- sums the elements on the lists "elementwise". Example:
--
-- > zipSum [3,4,7] [3,2,1]
-- [6,6,8]

zipSum :: [Int] -> [Int] -> [Int]
zipSum [] xs         = []
zipSum xs []         = []
zipSum (x:xs) (y:ys) = (x + y) : zipSum xs ys

-- Exercise 5: merge function, which merges two
-- sorted lists into a single sorted list

merge :: [Int] -> [Int] -> [Int]
merge [] xs         = xs
merge xs []         = xs
merge (x:xs) (y:ys) =
  if x < y
    then x:merge xs (y:ys)
    else y:merge (x:xs) ys

-- Exercise 6: merge sort algorithm

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
  where
    (l, r) = splitAt p xs
    p = length xs `div` 2


