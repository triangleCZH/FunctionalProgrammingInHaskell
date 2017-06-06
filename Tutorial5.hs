module Tutorial5 where

import           Data.Char (isAlpha)
import           Parsing


-- Part I

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

expr1 :: Expr
expr1 = Add (Val 1) (Mul (Val 2) (Val 3))

type Unary a = Int -> a
type Binary a = a -> a -> a

fold :: Unary a -> Binary a -> Binary a -> Expr -> a
fold v _ _ (Val n)   = v n
fold v a m (Add x y) = a (fold v a m x) (fold v a m y)
fold v a m (Mul x y) = m (fold v a m x) (fold v a m y)

eval' :: Expr -> Int
eval' = fold id (+) (*)

binary :: String -> String -> String -> String
binary op x y = "(" ++ x ++ " " ++ op ++ " " ++ y ++ ")"

printTree :: Expr -> String
printTree (Val n)   = show n
printTree (Add x y) = binary "+" (printTree x) (printTree y)
printTree (Mul x y) = binary "*" (printTree x) (printTree y)

printTree' :: Expr -> String
printTree' = fold show (binary "+") (binary "*")

-- Exercise 1
collect :: Expr -> [Int]
collect = fold (\x -> [x]) (++) (++)

-- Exercise 2
evalPrint :: Expr -> (Int, String)
evalPrint = fold fVar fAdd fMul
  where
    fVar = \n -> (n, show n)
    fAdd = \(e1, p1) (e2, p2) -> (e1 + e2, binary "+" p1 p2)
    fMul = \(e1, p1) (e2, p2) -> (e1 * e2, binary "*" p1 p2)


-- Part II

-- Exercise 3



-- Exercise 4
firstAlpha :: Parser Char
firstAlpha = do _ <- many (sat (not . isAlpha))
                item

-- Exercise 5
manyN :: Int -> Parser a -> Parser [a]
manyN 0 _ = return []
manyN n parser = do x  <- parser
                    xs <- manyN (n - 1) parser
                    return (x:xs)

-- Part III
--jump and quare root search
pFactor :: Parser Expr
pFactor = pPara +++ pInt --the one put at the second place is the base case
  where
    pPara = do
      _ <- token $ char '('
      e <- pExpr
      _ <- token $ char ')'
      return e
    pInt = do
      x <- integer
      return $ Val x


pTerm :: Parser Expr
pTerm = pFactorTerm +++ pFactor
  where
    pFactorTerm = do
      f <- pFactor
      _ <- token $ char '*'
      t <- pTerm
      return $ Mul f t

pExpr :: Parser Expr
pExpr = pTermExpr +++ pTerm
  where
    pTermExpr = do
      t <- pTerm
      _ <- token $ char '+'
      e <- pExpr
      return $ Add t e


interpret :: String -> Int
interpret = eval . fst . head . parse pExpr


