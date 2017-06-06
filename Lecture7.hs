import Prelude
import Parsing
import Data.Char
data Expr = Val Int
           | Add Expr Expr
           | Mult Expr Expr

--10 * (5 + 7)

e1 = Mult (Val 10) (Add (Val 5) (Val 7))
--10 + 5 * 7
e2 = Add (Val 10) (Mult (Val 5) (Val 7))

{-}
expr :: Parser Expr
expr = do t ← term
          do char '+'
             e ← expr
             return $ Add t e +++ return $ Val t

term :: Parser Expr
term = do f ← factor
          do char '*'
             t ← term
             return $ Mult t f +++ return $ Val f

factor :: Parser Expr
factor = do d ← digit
            return $ Val (digitToInt d)
            +++ do char '('
                   e ← expr
                   char ')'
                   return $ Val e-}

expr = do t <-term
          do char '+'
             e <- expr 
             return (t + e) +++ return t
term = do f <- factor 
          do char '*'
             t <- term 
             return (f * t) +++ return f

factor = do d <- digit
            return (digitToInt d) +++ returning
              where returning = do char '('
                                   e <- expr
                                   char ')'
                                   return e 
