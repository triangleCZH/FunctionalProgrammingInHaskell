module A1_3035232640 where
import Test.QuickCheck
import Data.Char

--Q1
combinations :: Int -> Int -> Int
--base case
combinations 0 _ = 1
combinations 1 k = k
--k == n is a special case needs special care
combinations k n = if k == n then 1 else combinations k (n - 1) + combinations (k - 1) (n - 1)

--Q2
position :: Eq a => a -> [a] -> Int
--the where syntex is used to number the list with index
position a xs = if length list == 0 then -1 else head list
    where list = [ i | (x, i) <- zip xs [0..], x == a]

--Q3
intToColumn :: Int -> [Char]
--base case
intToColumn 0 = ""
--recursion to get the chars one by one
intToColumn n = intToColumn ((n - 1)  `div` 26) ++ [chr ((n - 1) `mod` 26 + ord 'A')]

--Q4
columnToInt :: [Char] -> Int
--base case
columnToInt [] = 0
--use the same concept as calculation of decimal values
columnToInt xs = sum [ a * 26 ^ i | (x, i)  <- (zip xs (reverse [0..(length xs - 1)])), let a = ord x - (ord 'A') + 1]

--Q5
propColumnNumber :: Int -> Property
--success if the convert -> counter-convert gives back the same value
propColumnNumber x = (x >= 0) ==> x == columnToInt (intToColumn x)

--Q6
lookupPhoneNumber :: [String] -> String -> [String]
--return all the values
lookupPhoneNumber xs [] = [ filter (`elem` ['0'..'9']) x | x <- xs]
--1:the length of filtered string should be at least as long as the prefix
--2:every single letters should match
lookupPhoneNumber xs x = [ substitution | strs <- xs, let substitution = (filter (`elem` ['0'..'9']) strs), length substitution >= (length x), (and) [ x1 == x2 | (x1, x2) <- zip substitution x]]

--Q7
permutations :: [a] -> [[a]]
--base case
permutations [] = [[]]
permutations (x:xs) = inserting x (permutations xs)
    where 
      inserting ele originals = [ x | xx <- [ map (\x -> combining (splitAt x original) ele) [0..(length original)] | original <- originals ], x <- xx]
        where 
            combining (x,y) z = x ++ (z :y)
{-
1)what permutations does is to insert an element into all the combinations of the n-1 elements
2)what inserting does is: firstly, let every list original in the [[]] originals to do: insert the new element in every position of original, which
forms [[]] xx, pick up every x in xx, reform all the x into [[]]
3)what combining does is that, given a curried pair and one element, it connects them together, this is for simplicity when coping with splitAt output form.
-}

  