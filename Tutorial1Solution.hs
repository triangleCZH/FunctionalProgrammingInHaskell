module Tutorial1Solution where

import Test.QuickCheck

-- curried functions --
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

myaddPair :: (Int, Int) -> Int
myaddPair (x, y) = x + y

myadd :: Int -> Int -> Int
myadd x y = x + y

myadd1 :: Int -> Int
myadd1 = myadd 1

take5 :: [Int] -> [Int]
take5 = take 5

curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f = \x -> \y -> f (x,y)

uncurry1 :: (a -> b -> c) -> (a, b) -> c
uncurry1 f = \(x,y) -> f x y

prop_curry_add :: Int -> Int -> Bool
prop_curry_add x y = curry1 (uncurry1 myadd) x y == myadd x y

-- parametric polymorphism --
identity x = x

-- define functions --
-- conditional expressions --
signum1 :: Int -> Int
signum1 n = if n < 0 then -1 else
    if n == 0 then 0 else 1
-- guarded equations --
signum2 n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1
-- pattern matching --
hd :: [a] -> a
hd []        = error "cannot take the head of an empty list!"
hd (x:xs)    = x

first :: (a,b) -> a
first (x,y) = x

isZero :: Int -> Bool
isZero 0 = True
isZero n = False

-- lambda expressions --
add = \x -> (\y -> x + y)
myconst x = \_ -> x
odds n = map (\x -> x*2 + 1) [0..n-1]

-- Exercise --
isleapyear :: Int -> Bool
isleapyear j =
    if j `mod` 4 /= 0 then False
        else if j `mod` 100 /= 0 then True
            else if j `mod` 400 == 0 then True else False

isleap :: Int -> Bool
isleap j
    | 0==j`mod`100 = 0 == j`mod`400
    | otherwise    = 0 == j`mod`4

prop_leap :: Int -> Property
prop_leap j = (j > 0) ==> isleap j == isleapyear j

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- Exercise --
safetail1 :: [a] -> [a]
safetail1 xs = if (null xs) then [] else tail xs

safetail2 xs
    | null xs = []
    | otherwise = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs

safetail4 = \xs -> if null xs then [] else tail xs

-- More on Quickcheck --
absolute :: Int -> Int
absolute x = if (x < 0) then -x else x

prop_absolute :: Int -> Int -> Property
prop_absolute a b = let a' = toInteger a
                        b' = toInteger b
                        maxInt = toInteger (maxBound :: Int)
                        minInt = toInteger (minBound :: Int)
                    in
                        (a' * b' < maxInt && a' * b' > minInt)
                        ==> (absolute a) * (absolute b) == absolute (a * b)

prop_absolute' :: Int -> Int -> Property
prop_absolute' a b = (a' * b' < maxInt && a' * b' > minInt)
                        ==> (absolute a) * (absolute b) == absolute (a * b)
                    where
                        a' = toInteger a
                        b' = toInteger b
                        maxInt = toInteger (maxBound :: Int)
                        minInt = toInteger (minBound :: Int)


{-
 Note:
    1) You may wonder whether there is a difference between "where clauses" and "let expressions".
Indeed, there is a small difference: let is an expression, so you may use wherever you can use
expressions (just like "if" in Tutorial 0).
Whereas "where clauses" are syntax constructs.
You can read more here: https://wiki.haskell.org/Let_vs._Where

    2) Property is needed due to the use of this (==>) implication operator.
It is a type within QuickCheck whose content may complex, so it is not easy to print it in general
(that is why "prop_absolute 1 2" will tell you it cannot print it).
If you want to combine Properties, e.g. a ==> b && b ==> a, you need to use combinators, such as .&&. :
https://hackage.haskell.org/package/QuickCheck-2.6/docs/Test-QuickCheck-Property.html#g:7

    3) Instead of the familiar message that all tests passed, you may have gotten a message
that QuickCheck gave up. It is because it randomly generated quite a few values that didn't satisfied
the premise, so it rather gave up than continue executing infinitely.
Anyway, a better way in this situation is to write a custom generator that would generate you values
in that range, but it is beyond what we will cover in this course. For your information:
http://stackoverflow.com/a/18502905

Also, if you want to see what values QuickCheck tried (or change different parameters),
you can use different execution modes:
https://hackage.haskell.org/package/QuickCheck-2.4.0.1/docs/Test-QuickCheck.html

e.g. verboseCheck


