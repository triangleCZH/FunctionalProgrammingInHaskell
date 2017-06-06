{-

COMP3258: Functional Programming

Assignment 2

Deadline: 23:59, March 21, 2017 (HKT)

-}

import Data.Char (ord, chr, isLetter)
{-

DO NOT ADD OTHER IMPORTS!

Unless specified, you should not use functions from other libraries apart from
the standard library Prelude

-}

-- | Problem 1

enumIntFromTo :: Int -> Int -> [Int]
enumIntFromTo a b = if a > b then [] else a : (enumIntFromTo (a + 1) b)


enumIntFromThenTo :: Int -> Int -> Int -> [Int]
enumIntFromThenTo a b c = 
    if (a > b && a < c) || (a < b && a > c) then []
    else a : (enumIntFromThenTo b (2 * b - a) c)



-- | Problem 2
-- You can assume that messages only use uppercase letters (A-Z) and white spaces.

caesar :: Int -> String -> String
caesar num [] = []
caesar num (first : tail) = 
    if first == ' ' then first : (caesar num tail)
    else (chr ((ord first - ord 'A' + num) `mod` 26 + ord 'A')) : (caesar num tail)

-- | Problem 3
unCaesar :: Int -> String -> String
unCaesar num [] = []
unCaesar num (first : tail) = 
    if first == ' ' then first : (unCaesar num tail)
    else (chr ((ord first - ord 'A' - num) `mod` 26 + ord 'A')) : (unCaesar num tail)

-- | Problem 4
-- it will handle the case of non-upper-case letters for both key and plain
vigenere :: String -> String -> String
vigenere key plain = vigenering (process key) (process plain)
  where
    process line = map (\x -> if ord x <= ord 'z' && ord x >= ord 'a' then chr (ord x - 32) else x) line
    vigenering key [] = []
    vigenering [] plain = plain
    vigenering all@(k : key) (first : plain) =  
      if isLetter first then (chr ((ord k + ord first - 2 * ord 'A') `mod` 26 + ord 'A')) : (vigenering (key ++ [k]) plain)
      else first : (vigenering all plain)

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)


-- | Problem 5

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node l v r) = if a > v then Node l v (insert a r) else Node (insert a l) v r 

-- | Problem 6

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node l v r) = v : (preorder l) ++ (preorder r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = (inorder l) ++ (v : (inorder r))

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node l v r) = postorder l ++ (postorder r) ++ [v] 



-- | Problem 7
sort :: Ord a => [a] -> [a]
sort [] = []
sort branches = inorder (foldr insert Leaf branches)   
{-- an opition to do this problem
sort :: Ord a => [a] -> [a]
sort [] = []
sort branches = inorder (help branches Leaf)
  where 
    help [] w = w
    help (b:branches) Leaf = help (branches) (insert b Leaf)
    help (b:branches) (Node l v r) = help (branches) (insert b (Node l v r))
-} 



data ListZipper a =
  ListZipper [a]
             a
             [a]
  deriving (Eq, Show)


-- | Problem 8

toList :: ListZipper a -> [a]
toList (ListZipper l1 v l2) = reverse l1 ++ (v : l2)

-- | Problem 9

moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (ListZipper [] _ _) = Nothing
moveLeft (ListZipper (f:l1) v l2) = Just (ListZipper l1 f (v:l2))

moveRight :: ListZipper a -> Maybe (ListZipper a)
moveRight (ListZipper _ _ []) = Nothing
moveRight (ListZipper l1 v (f:l2)) = Just (ListZipper (v:l1) f l2)
