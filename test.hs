-- Functional parsing library from chapter 8 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2007.
-- Modified by Bruno Oliveira
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 +++

-- Basic parsers
data Parser a =  P (String -> [(a,String)])

-- The monad of parsers (for the do-notation)

instance Functor Parser where
   fmap f (P g) = P (\s -> fmap (\(x,s') -> (f x, s')) (g s))

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

instance Alternative Parser where
   empty = failure
   (<|>) = (+++)

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f  =  P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)])



parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp  

item ::Parser Char --it sepecifies a parser for type Char, so item 'a' for example
item = P(\inp -> case inp of
                 []->[]
                 (x:xs) -> [(x,xs)]) 
--this follows the structure of a parser, P (), where inside, take a input and return [(,)]

failure ::Parser a  --failure "adsfad" returns []
failure = P (\inp -> [])
--inside () is a lambda function, where input any type, return empty list



(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of     --if parse p inp fails
                    []-> parse q inp        --then return the trial to parse q inp
                    [(v,out)] -> [(v, out)])--else just return result of parse p inp

{-does not work? why?-}
--sequencing
p :: Parser (Char,Char)
p = do x <- item -- this line does: using item to parse the first char of inp, give the value of the char to x  by using '<-'
       item -- the layout should be the same
       y <- item
       return (x, y)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item --p is such as p inp = inp == 'a', or isDigit
           if p x then
             return x
           else
             failure

char :: Char -> Parser Char
char x = sat (x ==)

digit :: Parser Char
digit = sat isDigit

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v
              
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


pp :: Parser String
pp =do char '[' --eat the [
       d <- digit --eat the first digit
       ds <- many (do char ',' --use a new block to eat every possible ,digit pairs, it gives out a list
                      digit)
       char ']' --eat the ]
       return (d:ds)


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

