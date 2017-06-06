import Data.Char
import Data.List
import Parsing
import System.IO

-- | Problem 1
lift :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift op (Just a) (Just b) = Just (op a b)
lift op _ _ = Nothing

data Expr= Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Val Int
  | Var String
  deriving (Eq, Show)

type Env = [(String, Int)]

-- | Problem 2
eval :: Env -> Expr -> Maybe Int 
eval lib (Add e1 e2) = lift (+) (eval lib e1) (eval lib e2)
eval lib (Sub e1 e2) = lift (-) (eval lib e1) (eval lib e2)
eval lib (Mul e1 e2) = lift (*) (eval lib e1) (eval lib e2)
eval lib (Div e1 e2) = if eval lib e2 == (Just 0) then Nothing else lift div (eval lib e1) (eval lib e2)
eval lib (Mod e1 e2) = if eval lib e2 == (Just 0) then Nothing else lift mod (eval lib e1) (eval lib e2)
eval lib (Var e1) = lookup e1 lib
eval lib (Val e1) = Just e1

-- | Problem 3
identifier'  :: Parser String
identifier'  =  token ident'

ident' :: Parser String
ident' =  do x  <- letter
             xs <- many alphanum
             return (x:xs)

pExpr :: Parser Expr
pExpr = pOpTerm +++ pTerm

pTerm :: Parser Expr
pTerm = pOpFactor +++ pFactor

pOpTerm :: Parser Expr
pOpTerm = pOp1 +++ pOp2 +++ pTerm --could remove this pTerm
  where 
    pOp1 = do 
      x <- pTerm
      _ <- token $ char '+'
      y <- pOpTerm
      return (Add x y)
    pOp2 = do 
      x <- pTerm
      _ <- token $ char '-'
      y <- pOpTerm
      return (Sub x y) 


--must be an expression, or single ingeter, or a variable with name identifier
pFactor :: Parser Expr
pFactor = pPara +++ pInt +++ pId
  where
    pPara = do
      _ <- token $ char '('
      e <- pExpr 
      _ <- token $ char ')'
      return e 
    pInt = do
      x <- integer
      return (Val x)
    pId = do
      y <- identifier'
      return (Var y)


pOpFactor  :: Parser Expr
pOpFactor = pOp +++ pFactor --could remove this pFactor
  where
    pOp = do
      x <- pFactor
      t <- item
      y <- pOpFactor
      if t == '*' then return (Mul x y)
      else if t == '/' then return (Div x y)
      else if t == '%' then return (Mod x y)
      else failure

-- | Problem 4
runParser :: Parser a -> String -> Maybe a
runParser p line = if (snd $ head $ parse p line) == "" then Just (fst $ head $ parse p line) else Nothing

-- | Problem 5 to Problem 8
type Table = [[Maybe Int]]

--ref functions
intToColumn :: Int -> [Char]
--base case
intToColumn 0 = ""
--recursion to get the chars one by one
intToColumn n = intToColumn ((n - 1)  `div` 26) ++ [chr ((n - 1) `mod` 26 + ord 'A')]

columnToInt :: [Char] -> Int
--base case
columnToInt [] = 0
--use the same concept as calculation of decimal values
columnToInt xs = sum [ a * 26 ^ i | (x, i)  <- (zip xs (reverse [0..(length xs - 1)])), let a = ord x - (ord 'A') + 1]

--the answer function
printTable :: Table -> IO ()
printTable table = do printLine table (length (head table) - 1) (length table - 1)
                      putStr "\n" 



--main part
--The startting funtcion
main :: IO ()
main = askR

--Input request: rows
askR :: IO ()
askR =                   do putStr "Number of rows: "
                            xx <- getLine
                            if length xx == 0 then askR
                            else if and (map (\x -> x <= '9' && x >= '0') xx) then (askC (read xx :: Int)) else askR

--Input request: columns
askC :: Int -> IO ()
askC r =                 do putStr "Number of columns: "
                            xx <- getLine
                            if length xx == 0 then (askC r)
                            else if and (map (\x -> x <= '9' && x >= '0') xx) then (doThings (buildTable  r (read xx :: Int)) []) else (askC r) --the empty list is the reference unused 

doThings :: Table -> Env -> IO ()
doThings table ref = 
           do putStr "\n> "
              xx <- getLine
              --table part
              if xx == "table" then do printTable table
                                       doThings table ref
              --list out the variables
              else if xx == "vars" then do printRef (sort ref)
                                           doThings table ref
              --quit the main function
              else if xx == "quit" then putStr "\n"
              --del part (since there is not a '=', it can only be a deletion)
              else if not ('=' `elem` xx) 
                   then do let safeHead = parse (delparsin table) xx
                           if safeHead == [] then sayError table ref
                           else do let (y, z) = head safeHead
                                   --variable deletion
                                   if lookup y ref /= Nothing then do putStrLn ("Deleted " ++ y)
                                                                      doThings table (delVar ref y)
                                   --table deletion
                                   else if libForTrial table y == [] || z /= "" then sayError table ref
                                   else do let (c, r) = head (libForTrial table y)
                                           putStrLn ("Deleted " ++ intToColumn c ++　show　r)
                                           doThings (revalue table c r Nothing) ref
              --give values to variables
              else if isLower (xx !! 0) 
                   then do let safeHead = parse (refparsin table ref) xx
                           if safeHead == [] then do sayError table ref
                           else do let ((x, y), z)  = head safeHead
                                   if  y == Nothing ||  z /= ""  then sayError table ref
                                   --if it's new, insert it
                                   else if lookup x ref == Nothing
                                   then do putStrLn (x ++ " = " ++ show (filtered y))
                                           doThings table ((x, filtered y):ref)
                                   --if it's already in the reference, update it
                                   else do putStrLn (x ++ " = " ++ show (filtered y))
                                           doThings table (updateVar ref x (filtered y)) --if need to update value, haven't yet
              -- handles tail cases, and give values to the table
              else do let safeHead = parse (parsin table ref) xx
                      if safeHead == [] then sayError table ref
                      else do let ((x, y), z)  = head safeHead
                              if libForTrial table x == [] || y == Nothing || z /= "" then sayError table ref
                              else do let (c, r) = head (libForTrial table x)
                                      putStrLn (intToColumn c ++ show r ++ " = " ++ show (filtered y)) 
                                      doThings (revalue table c r y) ref



--parsers : they don't handle main errors, coz I find it hard to do extra things in a pure parser
--parser for interpreting reference
refparsin :: Table -> Env -> Parser (String, Maybe Int)
refparsin table ref= goodFormat +++ badFormat
                       where goodFormat = do _ <- string "let "
                                             name <- identifier
                                             equa <- token $ char '='
                                             value <- pExpr
                                             return (name, (eval (librForJust table ++ ref) value))
                             badFormat =  do name <- identifier
                                             equa <- token $ char '='
                                             value <- pExpr
                                             return (name, (eval (librForJust table ++ ref) value))

--parser for interpreting delete something like A1 or a
delparsin :: Table -> Parser [Char]
delparsin table = do x <- string "del "
                     y <- identifier'
                     return y

--This basic parser uses identifier', which handles every case starting with a letter
parsin :: Table -> Env -> Parser (String, Maybe Int)
parsin table ref = 
                 do name <- identifier'
                    equa <- token $ char '='
                    value <- pExpr
                    return (name, (eval (librForJust table ++ ref) value))


--functions manipulating values
--update a varaible if it's already a reference
updateVar :: Env -> String -> Int -> Env
updateVar ref x y = [  if a == x then (x, y) else (a, b) | (a, b) <- ref ]

--delete a variable
delVar :: Env -> String -> Env
delVar ref x = foldr (\(a,b) xx -> if a == x then xx else (a, b):xx) [] ref

--can update, or disable (Nothing) a table value
revalue :: Table -> Int -> Int -> Maybe Int -> Table
revalue table c r value = [ if y == r then replaceNth (c - 1) (table !! (y - 1)) value else table !! (y - 1) | y <- [1..length table]]     
                                where replaceNth nth (x:xs) val 
                                         | nth == 0 = val:xs
                                         | otherwise = x : (replaceNth (nth - 1) xs val) 


--tool function
--function to construct a new table
buildTable :: Int -> Int -> Table
buildTable r c = [ [Nothing  | y <- [1..c] ] | x  <- [1..r]]


--the analyze functions to figure out at least how many spaces are needed for each column
analyze :: Table -> [Int]
analyze table =  map (counts) [ foldr (\x xs -> if x == Nothing || x < xs then xs else x) (Nothing) [ table !! yy !! xx | yy <- [0..(length table - 1)]] | xx <- [0..(length (head table) - 1)]]
                   where counts Nothing = 0
                         counts (Just a) = length $ show a

--based on analyze, also consider like AA is longer than nothing
analyzing :: Table -> [Int]
analyzing table = [ if analysis !! (x - 1) < length (intToColumn x) then length (intToColumn x) else analysis !! (x - 1) | x <- [1..length analysis] ]
                        where analysis = analyze table

--easy tool to delete Maybe                        
filtered :: Maybe a -> a
filtered (Just a) = a

-- build into a liabrary or a map, so that can look into values, exclusive for Nothing
librForJust :: Table -> Env
librForJust table = concat [ [ ((intToColumn (x + 1)) ++ show (y + 1),  filtered (table !! y !! x)) | x <- [0..length (head table) - 1] , table !! y !! x /= Nothing] |  y <- [0..(length table - 1)] ]

--to find out if A1 is inside the scope
libForTrial :: Table -> String -> [(Int,Int)]
libForTrial  table name = concat [ if (intToColumn x ++ show y == name) then [(x, y)] else [] | x <- [1..(length (head table))], y <- [1..(length table)]] 


--print stuff
--to sayError for one case and continue
sayError :: Table -> Env -> IO ()
sayError table ref = do putStrLn "Error"
                        doThings table ref

--print out the reference
printRef :: Env -> IO ()
printRef [] = putStrLn ""
printRef ((x,y):xs) = do putStrLn (x ++ " = " ++ show y)
                         printRef xs

--the recursive helping function
printLine :: Table -> Int -> Int -> IO ()
printLine table (-1) (-1) = putStr (replicate (length (show (length table))) ' ' ++ "|") -- (x, y)
printLine table x (-1) = do printLine table (x - 1) (-1)
                            putStr $ (replicate ((analyzing table) !! x - length (intToColumn (x + 1)))  ' '  ++ (intToColumn (x + 1)) ++ "|")
printLine table (-1) y = do printLine table (length (head table) - 1) (y - 1)
                            putStr "\n"
                            putStr ( replicate (length (show (length table)) - length (show (y + 1))) ' ' ++ show (y + 1) ++ "|")
printLine table x y = do printLine table (x - 1) y 
                         putStr $ (replicate ((analyzing table) !! x - length (filtered (table !! y !! x)))  ' ' ++ filtered (table !! y !! x)++ "|")
                           where filtered (Just a) = show a 
                                 filtered Nothing = ""

--test cases
--test cases for refparsin
qq = parse (refparsin ttt [("a", 1),("b",2)]) "c = a * b"
q = parse (refparsin (buildTable 4 4) []) "a = 1" 

--test cases for parsin
ll = parse (parsin ttt []) "A1=    A1 * A3 + C3 % A1" -- support no space or multiple spaces!
l1 = parse (parsin ttt []) "afdfewg"
l2 = parse (parsin ttt []) "A1 = 1 hi"
l3 = parse (parsin ttt []) "A1 = i am good"

--test cases for building tables
ttt :: Table
ttt = [[Just 10000000, Nothing, Nothing],[Nothing, Nothing, Just 31010101],[Just 1000, Nothing, Just 33]]
t :: Table
t = [[Just 1000]]
tt :: Table
tt = [[Just 1000, Just 100],[Nothing, Just 1]]
aaa :: Table
aaa = [[Just 100, Nothing, Nothing],[Nothing, Just 1234, Just 31010],[Just 1000, Nothing, Just 33],[Just 1000, Nothing, Just 1111],[Just 100, Nothing, Nothing],[Nothing, Nothing, Just 31010],[Just 1000, Nothing, Just 33],[Just 1000, Nothing, Just 1111],[Just 100, Nothing, Nothing],[Nothing, Nothing, Just 31010],[Just 1000, Nothing, Just 33],[Just 1000, Nothing, Just 1111],[Just 100, Nothing, Nothing],[Nothing, Nothing, Just 31010],[Just 1000, Nothing, Just 33],[Just 1000, Nothing, Just 1111]]

--don't really understand but a "let " is automatically added to the front the line, so delete it first
