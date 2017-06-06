import Data.Char

{-
Q1:

List comprehensions allow building new lists, typically from old lists
using a set of generators and guards.

A generator is an expression that states how to generate values for a
certain variable. For example:

x <- [1..5]

states how to generate values for the variable x.

A dependent comprehension is a comprehension where a generator depends
on values produced by a previous generator.

An example of a dependent generator is:

concat xss = [x | xs <- xss, x <- xs]

Here, the last generator (x <- xs) depends on the value produced by
the previous generator (xs).
-}


-- Q2:

-- 2a.

isFaceCard :: Char -> Bool
isFaceCard x = x == 'A' || x == 'K' || x == 'Q' || x == 'J'

isCard :: Char -> Bool
isCard x = isFaceCard x || (isDigit x && x /= '1')

facecard :: String -> Bool
facecard str = and [ isFaceCard c | c <- str, isCard c ]

-- 2b.

facecard2 :: String -> Bool
facecard2 [] = True
facecard2 (c:str) | isCard c  = isFaceCard c && facecard2 str
          | otherwise = facecard2 str

-- 2c.

facecard3 :: String -> Bool
facecard3 str = foldr (&&) True (map isFaceCard (filter isCard str))

-- Q3:

-- 3a.

matches :: String -> String -> String
matches xs ys = [ x | (x,y) <- zip xs ys, x==y ]

-- 3b.

matches2 :: String -> String -> String
matches2 [] ys = []
matches2 xs [] = []
matches2 (x:xs) (y:ys) | x==y      = x : matches2 xs ys
                       | otherwise = matches2 xs ys
