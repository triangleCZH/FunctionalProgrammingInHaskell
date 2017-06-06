Law: in the Indcutive case, you assume that the Hypothesis is true for a small case, and therefore try to prove 
that this also works for a larger case 
--1
Prove:
all (==x) (replicate n x) = True

Base case: n = 0
all (==x) (replicate n x)
= {n = 0}
all (==x) (replicate 0 x)
= {by definition of replicate}
all (==x) []
= {by definition of all}
True

Indcutive case: n = m + 1
all (==x) (replicate n x)
= {n = m + 1}
all (==x) (replicate (m + 1) x)
= {by definition of replicate}
all (==x) (x : replicate m x)
= {by definition of all}
(x == x) && all (==x) (replicate m x)
= {by Induction Hypothesis}
True && True
= {Simplification}
True

--2
Prove: 
definition
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

1. xs ++ [] = xs
2. xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

1.
Indcution on xs
Base case: xs = []
xs ++ []
= {xs = []}
[] ++[]
= {by definition of ++}
[]

Indcutive case: xs = y : ys
(y:ys) ++ []
= {by definition of ++}
y : (ys ++ [])
= {Induction Hypothesis}
y : ys
= {Simplification}
xs


2.
