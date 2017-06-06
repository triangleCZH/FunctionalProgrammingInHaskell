module Tutorial3 where

-- Exercise 1: pythagoreans
pyths :: Integer -> [(Integer, Integer, Integer)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Exercise 2: perfect numbers
perfects :: Integer -> [Integer]
perfects n = [x | x <- [2..n], x == sum (factors x)]

factors :: Integer -> [Integer]
factors x = [y | y <- [1..x-1], x `mod` y == 0]

-- Exercise 3: from a list comprehension to normal function definition.
filtmap f p xs = map f (filter p xs)

-- Exercise 4: redefine zipWith using zip and map
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) $ zip xs ys

-- Exercise 5: scalar product
scalaprod :: [Integer] -> [Integer] -> Integer
scalaprod xs ys = sum $ zipWith (*) xs ys


-- Exercise 6: redefine library functions
-- length, reverse, map, filter, unzip
length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) [] -- or the line below
--reverse' = foldl (flip (:)) []


map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] -- or the line below
--map' f = foldr (\x xs -> (f x) : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

filter'' f = foldl (\xs x -> if f x then xs++[x] else xs) []

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x,y) (xs, ys) -> (x:xs, y:ys)) ([], [])


