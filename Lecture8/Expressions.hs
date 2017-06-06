-- Expression parser example from section 8.8 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2007.

-- Parser for simple arithmetic expressions

import Parsing

-- "1 + 1"

-- Full grammar:
-- expr → term ('+' expr  ε)
--
-- term → factor ('*' term  ε)
-- 
-- factor → digit  '(' expr ')‘
-- 
-- digit  → '0'  '1'  …  '9' 

-- Parsing Expressions:

-- Examples: 1, 1 + 1
-- eval "2 * 3 + 4 * 5"

-- "3+2"

-- expr → term ('+' expr  ε)
expr :: Parser Int
expr = do t <- term
          (do char '+'
              e <- expr
              return (t + e)) +++ return t

-- term → factor ('*' term  ε)
term :: Parser Int
term =  do f <- factor
           (do char '*'
               t <- term
               return (f*t)) +++ return f

-- factor → '(' expr ')‘ | digit 
-- Hint: use natural for parsing digits

factor  :: Parser Int
factor = (do char '('
             e <- expr
             char ')'
             return e) +++ natural  

eval :: String -> Int
eval xs =  case (parse expr xs) of
               [(n,[])]  -> n
               [(_,out)] -> error ("unused input " ++ out)
               []        -> error "invalid input"
