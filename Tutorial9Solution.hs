
================================== Review ==============================================================

Prove:

map f (map g xs) = map (f . g) xs

Proof:

Induction on xs

Base case: xs = []

map f (map g xs)
= {xs = []}
map f (map g [])
= {Definition of map}

[]
= {Definition of map}
map (f . g) []
= {xs = []}
map (f . g) xs

Inductive case: xs = y:ys

map f (map g xs)
= {xs = y:ys}
map f (map g (y:ys))
= {Definition of map}
map f (g y : map g ys)
= {Definition of map}
f (g y) : map f (map g ys)
= {Induction hypothesis}
f (g y) : map (f . g) ys
= {Definition of map and .}
map (f . g) (y:ys)
= {xs = (y:ys)}
map (f . g) xs

Qed.

====================================================================================================



============================== Question 1===========================================================

Prove

all (== x) (replicate n x) = True

Proof:

Induction on n

Base case: n = 0

all (== x) (replicate n x)
= {n = 0}
all (== x) (replicate 0 x)
= {Definition of replicate}
all (== x) []
= {Definition of all}
True

Inductive case: n = m + 1

all (== x) (replicate n x)
= {n = m + 1}
all (== x) (replicate (m + 1) x)
= {Definition of replicate}
all (== x) (x : replicate m x)
= {Definition of all}
(x == x) && all (== x) (replicate m x)
= {Simplification}
True && all (== x) (replicate m x)
= {Induction hypothesis}
True && True
= {Simplification}
True

Qed.

====================================================================================================



============================ Question 2 ============================================================

Prove

xs ++ [] = xs

Proof:

Induction on xs

Base case: xs = []

xs ++ []
= {xs = []}
[] ++ []
= {Definition of ++}
[]
= {xs = []}
xs

Inductive case: xs = y:ys

xs ++ []
= {xs = y:ys}
(y:ys) ++ []
= {Definition of ++}
y : (ys ++ [])
= {Induction hypothesis}
y : ys
= {xs = (y:ys)}
xs

Qed.


Prove

xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Proof:

Induction on xs

Base case: xs = []

xs ++ (ys ++ zs)
= {xs = []}
[] ++ (ys + zs)
= {Definition of ++}
ys + zs
= {Definition of ++}
([] ++ ys) ++ zs
= {xs = []}
(xs ++ ys) ++ zs

Inductive case: xs = a:as

xs ++ (ys ++ zs)
= {xs = a:as}
(a:as) ++ (ys ++ zs)
= {Definition of ++}
a : (as ++ (ys ++ zs))
= {Induction hypothesis}
a : ((as ++ ys) ++ zs)
= {Definition of ++}
(a : (as ++ ys)) ++ zs
= {Definition of ++}
((a : as) ++ ys) ++ zs
= {xs = (a:as)}
(xs ++ ys) ++ zs

Qed.

====================================================================================================



================ Question 3 ========================================================================

Prove

take n xs ++ drop n xs = xs

Proof:

Simultaneous induction on n and xs

Base case: n = 0

take n xs ++ drop n xs
= {n = 0}
take 0 xs ++ drop 0 xs
= {Definition of take and drop}
[] ++ xs
= {Definition of ++}
xs

Base case: n = m + 1, xs = []

take n xs ++ drop n xs
= {n = m + 1, xs = []}
take (m + 1) [] ++ drop (m + 1) []
= {Definition of take and drop}
[] ++ []
= {Definition of ++}
[]
= {xs = []}
xs

Inductive case: n = m + 1, xs = y:ys

take n xs ++ drop n xs
= {n = m + 1, xs = y:ys}
take (m + 1) (y:ys) ++ drop (m + 1) (y:ys)
= {Definition of take and drop}
(y : take m ys) ++ (drop m ys)
= {Definition of ++}
y : (take m ys ++ drop m ys)
= {Induction hypothesis}
y : ys
= {xs = y:ys}
= xs

====================================================================================================


========= Question 4 ===============================================================================

> data Tree = Leaf Int | Node Tree Tree

First we define two functions

> leaves (Leaf _)   = 1
> leaves (Node l r) = leaves l + leaves r
>
> nodes (Leaf _)    = 0
> nodes (Node l r) = 1 + nodes l + nodes r

Prove

nodes t + 1 = leaves t

Proof:

Induction on t

Base case: t = Leaf n

nodes t + 1
= {t = Leaf n}
nodes (Leaf n) + 1
= {Definition of nodes}
0 + 1
= {Simplification}
1
= {Definition of leaves}
leaves (Leaf n)
= {t = Leaf n}
levaes t

Inductive case: t = Node l r

nodes t + 1
= {t = Node l t}
nodes (Node l r) + 1
= {Definition of notes}
(1 + nodes l) + nodes r + 1
= {Simplification}
(nodes l + 1) + (nodes r + 1)
= {Induction hypothesis}
leaves l + leaves r
= {Definition of leaves}
leaves (Node l r)
= {t = Node l r}
leaves t


====================================================================================================


======= Question 5 =================================================================================

> data Expr = Val Int | Add Expr Expr
> data Op = PUSH Int | ADD

We have the equation

comp' e c = comp e ++ c


Base case: e = Val n

comp' e c
= {e = Val n}
comp' (Val n) c
= {Definiton of comp'}
comp (Val n) ++ c
= {Definition of comp}
[PUSH n] ++ c
= {Definition of ++}
PUSH n : c


Inductive case: e = Add x y

comp' e c
= {e = Add x y}
comp' (Add x y) c
= {Definition of comp'}
comp (Add x y) ++ c
= {Definition of comp}
(comp x ++ comp y ++ [ADD]) ++ c
= {Associativity of ++}
comp x ++ (comp y ++ ([ADD] ++ c))
= {Definition of ++}
comp x ++ (comp y ++ (ADD : c))
= {Induction hypothesis}
comp x ++ (comp' y (ADD : c))
= {Induction hypothesis}
comp'x (comp'y (ADD : c))

In conclusion, we obtain:

> comp' (Val n) c   = PUSH n : c
> comp' (Add x y) c = comp' x (comp' y (ADD : c))
