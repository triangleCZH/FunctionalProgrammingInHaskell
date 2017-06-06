module A1 where

import Test.QuickCheck


combinations :: Int -> Int -> Int
combinations k n
  | k == 0 || k == n = 1
  | otherwise = combinations k (n - 1) + combinations (k - 1) (n - 1)


position :: Eq a => a -> [a] -> Int
position = go 0
  where
    go :: Eq a => Int -> a -> [a] -> Int
    go _ _ [] = -1
    go i v (x:xs) = if v == x then i else go (i + 1) v xs


intToColumn :: Int -> String
intToColumn 0 = []
intToColumn n =
  intToColumn (m `div` 26) ++ [alphabet !! (m `mod` 26)]
    where
      m = n - 1
      alphabet = ['A'..'Z']


columnToInt :: String -> Int
columnToInt [] = 0
columnToInt (x:xs) =
  first * (26 ^ length xs) + columnToInt xs
    where
      alphabet = ['A'..'Z']
      first = 1 + position x alphabet


propColumnNumber :: Int -> Property
propColumnNumber x = (x >= 0) ==> columnToInt (intToColumn x) == x


lookupPhoneNumber :: [String] -> String -> [String]
lookupPhoneNumber book prefix =
  if null prefix
    then newBook
    else filter (startWith prefix) newBook
  where
    startWith p s = length p <= length s && and (zipWith (==) p s)
    format = filter (\c -> c /= '-' && c/= ' ')
    newBook = map format book


permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) =
  [insert x i p | i <- indexes, p <- permutations_xs]
--  concatMap (\i -> map (insert x i) permutations_xs) indexes
    where
      permutations_xs = permutations xs
      indexes = [0..length xs]
      insert v i list = let (l, r) = splitAt i list in l ++ [v] ++ r

