module Lecture4 where

import Prelude hiding (product, length, reverse, zip, drop, concat)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- computes the product of a list of numbers


-- product [1,2,3] = 1 * 2 * 3 = 6

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

-- computes the length of a list

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

-- reverse: reverses the order of the elements in a list

-- reverse [1,2,3] = [3,2,1]

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

-- reverse xs      = error "TODO"



{-
zip: takes 2 lists as inputs and returns
a list of pairs with the corresponding 
elements in the same position of each list. 
For example:

> zip [1..5] "Hello"
[(1,'H'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]

-} 

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs)  (y:ys)  = (x,y) : zip xs ys
zip _  _            = []

-- removes first n elements of a list

-- drop 2 [1,2,3,4,5] = [3,4,5]

drop :: Int -> [a] -> [a]
drop 0 xs      = xs
drop n []      = []
drop n (x:xs)  = drop (n-1) xs

-- appends two lists
--  [1,2,3] +++ [4..10]
-- [1,2,3,4,5,6,7,8,9,10]
--
--

(+++) :: [a] -> [a] -> [a]
[] +++ ys     = ys
(x:xs) +++ ys = x : xs +++ ys 

-- List comprehensions

-- concat [[1,2,3],[4,5]]

concat :: [[a]] -> [a]
concat xxs = [x | xs <- xxs, x <- xs]

-- Hint, use: n `mod` x == 0 

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ] 

prime  :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n =  [x | x <- [2..n], prime x]

allPrimes :: [Int]
allPrimes = [x | x <- [2..], prime x]

count :: Char -> String -> Int
count x [] = 0
count x (a:as) = (if x == a then 1 else 0) + count x as


--exercises
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : (take' (n-1) xs)

drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs  

elem' x [] = False
elem' n (x:xs) = if n == x then True else elem' n xs

quicksort [] = []
quicksort (x:xs) = let small = quicksort [n |  n <- xs, n <= x]
                       big = quicksort [n | n <- xs, n > x]
                   in small ++ [x] ++ big


--qsort :: Ord a ⇒ [a] → [a]
qsort [] = []
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
     smaller = [a | a <- xs, a <= x]
     larger = [b | b <- xs, b > x]

replicate' num times
    | times <= 0 = []
    | otherwise = num : replicate' num (times-1)



and' [] = True
and' (True:xs) = and' xs
and' (False:xs) = False


(!!!) [] _ = error "Empty list"
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1) 

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


--define a merge which can merge two sorted lists together
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge a@(x:xs) b@(y:ys) = if x < y then x : (merge xs b) else y : (merge a ys)

--define a mergesort
mergesort :: Ord a => [a] -> [a]
mergesort x 
  | length x <= 1 = x
  | otherwise = let (a,b) = splitAt (length x `div` 2) x in merge (mergesort a) (mergesort b)

--check if a list is sorted or not
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted xs = and [ x <= y | (x, y) <- (\x -> zip x (tail x)) xs]

-- this is the further explanantion of the lambda function of the previous function
pair' :: [a] -> [(a, a)]
pair' xs = zip xs (tail xs) 

--the following three shows how list comprehension can be used
position' :: Eq a => a -> [a] -> [Int]
position' x xs = [i | (xx, i) <- zip xs [1..], xx == x]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, n) | x <- [1..n], y <- [1..n], x^2 + y^2 == n^2]


perfect' xs = [a | a <- [2..xs], sum (factors a) == 2 * a]

--after class exercise
scalar :: [Int] -> [Int] -> Int
scalar [] _ = 0
scalar _ [] = 0
scalar xs ys = sum [x * y | (x,y) <- zip xs ys]