module Tutorial0 where

import Test.QuickCheck

last1 :: [Int] -> Int
last1 xs = xs !! (length xs - 1)

last2 :: [Int] -> Int
last2 xs = head (reverse xs)

init1 :: [Int] -> [Int]
init1 xs = take (length xs - 1) xs

init2 :: [Int] -> [Int]
init2 xs = reverse (tail (reverse xs))

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
