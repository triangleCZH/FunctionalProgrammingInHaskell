sumOdds :: [Int] -> Int
sumOdds xs = sum (filter odd (filter (> 0) xs))
--sumOdds' xs = sum . (filter odd) . (filter (> 0)) $ xs

--declaring datatypes
data Tree a = Leaf
             | Node (Tree a) a (Tree a)
             deriving (Eq, Ord)

balanced :: Tree a -> Bool
balanced Leaf = True
balaced (Node left v right) = 
    let size1 = sizeTree left
        size2 = sizeTree right
    in abs (size1 - size2) <= 1

sizeTree :: Tree a -> Int
sizeTree Leaf = 1
sizeTree (Node left v right) = 
    let size1 = sizeTree left
        size2 = sizeTree right
    in size1 + size2

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node l v r) = 
    let l' = mapTree f l
        r' = mapTree f r
    in Node l' (f v) r'

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f v Leaf = v
foldTree f v (Node l n r) = 
    let res1 = foldTree f v r
        res2 = f n res1
    in foldTree f res2 l

sumTree = foldTree (+) 0

flatten = foldTree (:) []