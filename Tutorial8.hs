module Tutorial8 where

import           Control.Monad
import           Parsing


p2 :: Parser (Char, Char)
p2 = item >>= \x ->
     item >>= \y ->
     return (x, y)


p3 :: Parser (Char, Char)
p3 = P (\inp -> case parse item inp of
                  []         -> []
                  [(v, out)] -> parse (f v) out)
     where
       f = \x -> P (\inp -> case parse item inp of
                              []        -> []
                              [(v, out)] -> parse ((\y -> return (x, y)) v) out)


data Expr = Mul Expr Expr | Div Expr Expr | Val Int deriving (Eq, Show)


eval :: Expr -> Maybe Int
eval (Val x) = return x
eval (Mul x y) = do
  a <- eval x
  b <- eval y
  return (a * b)
eval (Div x y) = do
  a <- eval x
  b <- eval y
  if b == 0 then Nothing else return (a `div` b)


data Exception a = Raise String | Return a deriving (Eq, Show)


instance Monad Exception where
  return = Return
  m >>= k = case m of Raise e  -> Raise e
                      Return a -> k a

instance Applicative Exception where
   pure = return
   (<*>) = ap

instance Functor Exception where
   fmap = liftM


eval2 :: Expr -> Exception Int
eval2 (Val x) = return x
eval2 (Mul x y) = do
  a <- eval2 x
  b <- eval2 y
  return (a * b)
eval2 (Div x y) = do
  a <- eval2 x
  b <- eval2 y
  if b == 0 then Raise (show a ++ " div 0") else return (a `div` b)
