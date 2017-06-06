import System.IO
import System.CPUTime
import Numeric

-- import Combinatorial

-- Solution found by student: ((1 + 7) * 3 + 50) * 10 + 25

data Op                       =  Add | Sub | Mul | Div

subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)
 
perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices                       :: [a] -> [[a]]
choices xs                    =  concat (map perms (subs xs))

apply                         :: Op -> Int -> Int -> Int
apply Add x y                 =  x + y
apply Sub x y                 =  x - y
apply Mul x y                 =  x * y
apply Div x y                 =  x `div` y

-- Checks whether applying an operation to two natural
-- numbers, returns a natural number.


-- datatype of expressions:

data Expr                     =  Val Int | App Op Expr Expr

values                        :: Expr -> [Int]
values (Val n)                =  [n]
values (App _ l r)            =  values l ++ values r

-- returns the value of an expression:
--   - if the value is invalid return []
--   - if the value is valid, return singleton list with value

-- Hint: use apply :: Op -> Int -> Int -> Int

-- valid :: Op -> Int -> Int -> Bool

eval :: Expr -> [Int]
eval (Val n)         = [n] 
eval (App op e1 e2)  =
  [apply op x y | x <- eval e1, y <- eval e2, valid op x y]

-- apply op (head (eval e1)) (head (eval e2)) 

-- Formalising the problem

{- Some examples: 

*Main> solution (App Mul (Val 3) (Val 4)) [3,4] 12
True
*Main> solution (App Add (Val 6) (Val 6)) [3,4] 12
False
*Main> solution (App Add (Val 3) (Val 4)) [3,4] 12
False

-}

solution          :: Expr -> [Int] -> Int -> Bool
solution e ns n   = or [ values e == l | l <- choices ns] && eval e == [n]   





-- Brute force solution

-- returns a list of all possible ways of splitting a list
-- into 2 non-empty parts

{-

> split [1,2,3,4]

[([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

-}

split                         :: [a] -> [([a],[a])]
split xs = [ (take n xs, drop n xs) | n <- [1..length xs - 1]]

  
ops                           :: [Op]
ops                           =  [Add,Sub,Mul,Div]

solutions                     :: [Int] -> Int -> [Expr]
solutions ns n                =
  [ e | c <- choices ns
      , e <- exprs c  
      , eval e == [n]]

-- Combining generation and evaluation

type Result                   =  (Expr,Int)

exprs                         :: [Int] -> [Expr]
exprs []                      =  []
exprs [n]                     =  [Val n]
exprs ns                      =  [e | (ls,rs) <- split ns
                                    , l       <- exprs ls
                                    , r       <- exprs rs
                                    , e       <- combine l r]

results                       :: [Int] -> [Result]
results []                    =  []
results [n]                   =  [(Val n,n) | n > 0]
results ns                    =  [e | (ls,rs) <- split ns
                                    , l       <- results ls
                                    , r       <- results rs
                                    , e       <- combine' l r]

combine      :: Expr -> Expr -> [Expr]
combine l r  = [ App o l r | o <- ops]

combine'                      :: Result -> Result -> [Result]
combine' (l,x) (r,y)          =
   [(App op l r, apply op x y) | op <- ops, valid op x y]

solutions'                    :: [Int] -> Int -> [Expr]
solutions' ns n               =  [e | ns'   <- choices ns
                                    , (e,m) <- results ns'
                                    , m == n]


-- valid function checks whether the operation applied
-- to the 2 numbers is valid

valid  :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Mul x y = True
valid Sub x y = x > y
valid Div x y = x `mod` y == 0





















-- Exploiting numeric properties

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y 
valid' Mul x y = x <= y && (x /= 1) && (y /= 1) 
valid' Div x y = x > y && x `mod` y == 0 && (y /= 1)

results'                      :: [Int] -> [Result]
results' []                   =  []
results' [n]                  =  [(Val n,n) | n > 0]
results' ns                   =  [res | (ls,rs) <- split ns
                                      , lx      <- results' ls
                                      , ry      <- results' rs
                                      , res     <- combine'' lx ry]

combine''                     :: Result -> Result -> [Result]
combine'' (l,x) (r,y)         =  [(App o l r, apply o x y) | o <- ops
                                                           , valid' o x y]

solutions''                   :: [Int] -> Int -> [Expr]
solutions'' ns n              =  [e | ns'   <- choices ns
                                    , (e,m) <- results' ns'
                                    , m == n]

-- Interactive version for testing


instance Show Op where
   show Add                   =  "+"
   show Sub                   =  "-"
   show Mul                   =  "*"
   show Div                   =  "/"

instance Show Expr where
   show (Val n)               =  show n
   show (App o l r)           =  bracket l ++ show o ++ bracket r
                                 where
                                    bracket (Val n) = show n
                                    bracket e       = "(" ++ show e ++ ")"

showtime                      :: Integer -> String
showtime t                    =  showFFloat (Just 3)
                                    (fromIntegral t / (10^12)) " seconds"

display                       :: [Expr] -> IO ()
display es                    =  do t0 <- getCPUTime
                                    if null es then
                                       do t1 <- getCPUTime
                                          putStr "\nThere are no solutions, verified in "
                                          putStr (showtime (t1 - t0))
                                     else
                                       do t1 <- getCPUTime
                                          putStr "\nOne possible solution is "
                                          putStr (show (head es))
                                          putStr ", found in "
                                          putStr (showtime (t1 - t0))
                                          putStr "\n\nPress return to continue searching..."
                                          getLine
                                          putStr "\n"
                                          t2 <- getCPUTime
                                          if null (tail es) then
                                             putStr "There are no more solutions"
                                           else
                                             do sequence [print e | e <- tail es]
                                                putStr "\nThere were "
                                                putStr (show (length es))
                                                putStr " solutions in total, found in "
                                                t3 <- getCPUTime
                                                putStr (showtime ((t1 - t0) + (t3 - t2)))
                                    putStr ".\n\n"

main                          :: IO ()
main                          =  do hSetBuffering stdout NoBuffering
			            putStrLn "\nCOUNTDOWN NUMBERS GAME SOLVER"
                                    putStrLn "-----------------------------\n"
                                    putStr "Enter the given numbers : "
                                    ns <- readLn
                                    putStr "Enter the target number : "
                                    n  <- readLn
                                    display (solutions'' ns n)