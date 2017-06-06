============================== Question 1===========================================================

Definition of length:
length [] = 0
length (x:xs) = 1 ++ length xs

Prove:

length (xs ++ ys) = length xs + length ys

Proof: 

Induction on xs

Base case: xs = []

length (xs ++ ys)
= {Definition of length}
length ys

length xs + length ys
= {Definition of length}
0 + length ys
= {Simplification}
length ys

Inductive case: xs = z:zs

length (z : zs ++ ys)
= {Definition of ++ }
length (z : (zs　++ ys))
= {Definition of length}
1 + length (zs ++ ys)
= {Induction hypothesis}
1 + length zs + length ys
= {Definition of length}
length (z : zs) + length ys
= {xs = z : zs}
length xs + length ys

============================== Question 2===========================================================

Definition of reverse:
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

Prove:

length (reverse l) = length l

Proof:

Induction on l

Base case: l = []

length (reverse l)
= {Definition of reverse}
length []
= {Definition of length}
0

Inductive case: l = x:xs

length (reverse l)
= {l = x:xs}
length (reverse (x:xs))
= {Definition of reverse}
length (reverse xs ++ [x])
= {Definition of length}
length (reverse xs) + length [x]
= {Induction hypothesis}
length xs + length [x]
= {Definition of length}
length (x:xs)
= {l = x:xs}
length l

============================== Question 3===========================================================

Prove:

reverse (xs ++ ys) = reverse ys ++ reverse xs

Proof:

Simultaneous induction on xs and ys

Base case: xs = []

reverse (xs ++ ys)
= {xs = []}
reverse ([] ++ ys)
= {Definition of []}
reverse ys
= {Definition of []}
reverse ys ++ []
= {Definition of reverse}
reverse ys ++ reverse []
= {xs = []}
reverse ys ++ reverse xs

Induction case: xs = a:as

reverse (xs ++ ys)
= {xs = a:as}
reverse ((a:as) ++ ys)
= {Definition of ++}
reverse (a:(as ++ ys))
= {Definition of reverse}
reverse (as ++ ys) ++ [a]
= {Induction hypothesis}
reverse ys ++ reverse as ++ [a]
= {Definition of ++}
reverse ys ++ (reverse as ++ [a])
= {Definition of reverse}
reverse ys ++ reverse (a:as)
= {xs = a:as}
reverse ys ++ reverse xs

============================== Question 4===========================================================

Definition of foldr:
foldr f acc []     = acc 
foldr f acc (x:xs) = f x (foldr f acc xs) 

Prove:

For function g and h, value w, if f and v satisfy
	v         = h w
    f x (h y) = h (g x y)
then h . foldr g w = foldr f v


Proof:

it is equivalent to prove, for any legal argument l,
we must have h . foldr g w l = foldr f v l

Induction on l

Base case: l = []

h . foldr g w l
= {l = []}
h . foldr g w []
= {Definition of foldr}
h w
= {v = h w}
v
= {Definition of foldr}
foldr f v []
= {l = []}
foldr f v l

Inductive case: l = a:as

h . foldr g w l
= {l = a:as}
h . foldr g w (a:as)
= {Definition of foldr}
h . (g a (foldr g w as))
= {f x (h y) = h (g x y)}
f a (h (foldr g w as))
= {Definition of composition}
f a (h . foldr g w as)
= {Induction hypothesis}
f a (foldr f v as)
= {Definition of foldr}
foldr f v (a:as)
= {l = a:as}
foldr f v l

In conclusion, we obtain:

> h . foldr g w = foldr f v

============================== Question 5===========================================================

Definition of filter:
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  

Prove:


filter p . filter q = filter (and p q)
    where and p q x = p x && q x

Proof:
it is equivalent to prove, for any legal argument l,
we must have filter p . filter q l = filter (and p q) l

Induction on l

Base case: l = []
filter p . filter q l
= {l = []}
filter p . filter q []
= {Definition of filter}
filter p []
= {Definition of filter}
[]
= {Definition of filter}
filter (and p q) []
= {l = []}
filter (and p q) l

Inductive case : l = x:xs where q x = False
filter p . filter q l
= {l = x:xs}
filter p . filter q (x:xs)
= {Definition of .}
filter p (filter q (x:xs))
= {Definition of filter, q x = False}
filter p (filter q xs)
= {Definition of .}
filter p . filter q xs
= {Induction hypothesis}
filter (and p q) xs
= {Definition of filter, and p q x = False}
filter (and p q) (x:xs)
= {l = x:xs}
filter (and p q) l

Indcutive case: l = x:xs where p x = False, q x = True
filter p . filter q l
= {l = x:xs}
filter p . filter q (x:xs)
= {Definition of .}
filter p (filter q (x:xs))
= {Definition of filter, q x = True}
filter p (x : filter p xs)
= {Definition of filter, p x = False}
filter p (filter q xs)
= {Definition of .}
filter p . filter q xs
= {Induction hypothesis}
filter (and p q) xs
= {Definition of filter, and p q x = False}
filter (and p q) x:xs
= {l = x:xs}
filter (and p q) l

Inductive case: l = x:xs where p x && q x = True
filter p . filter q l
= {l = x:xs}
filter p . filter q (x:xs)
= {Definition of .}
filter p (filter q (x:xs))
= {Definition of filterm q x = True}
filter p . (x : filter p xs)
= {Definition of filter, p x = True}
x : (filter p (filter q xs))
= {Definition of .}
x : (filter p . filter q xs)
= {Induction hypothesis}
x : filter (and p q) xs
= {Definition of filter, and p q x = p x && q x = True}
filter (and p q) (x:xs)
= {l = x:xs}
filter (and p q) l

In conclusion, we obtain:

> filter p . filter q = filter (and p q)


