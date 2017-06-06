import Data.Char
palindrome = do x <- getLine
                case (pali x == reverse (pali x)) of 
                  True -> putStrLn "It's a palindrome"
                  False -> putStrLn "Nope!"
                  where pali = map toLower .filter isLetter

--game part
nim :: IO ()
nim = do putStrLn ("")
         printTable [5,4..1]
         play 1 [5,4..1]

play id lst =  do putStr ("Player " ++ (show id) ++"\nEnter a row number: ") 
                  x <- getLine
                  putStr "Stat to move: "
                  y <- getLine
                  putStrLn　""
                  if isNum x && isNum y  -- how can I use let here, because when I try let x1 = read x :: Int, it does not work 			       	    
                  then do if (toNum x) <= length lst && (toNum y) <= lst !! ((toNum x) - 1)
                          then do printTable (update lst (toNum x-1) (toNum y))
                                  checkWin id (update lst (toNum x-1) (toNum y))
                          else do putStrLn "wrong bro, try again"
                                  play id lst
                  else do putStrLn "wtf that's not num!!"
                          play id lst

update lst ith minus = [ if ith == i then (lst !! i - minus) else (lst !! i) | i <- [0..(length lst - 1)]]

toNum :: String -> Int
toNum x = read x :: Int
isNum :: [Char] -> Bool
isNum str = foldr (\x xx -> isDigit x && xx) True str

printTable lst = putStr (concat [ (show (i + 1) ++ ": " ++ replicate (lst !! i) '*' ++ "\n") | i <- [0..(length lst - 1)] ] ++ "\n")
checkWin plyr lst = if and (map (== 0) lst)
                    then putStrLn ("Player " ++ show plyr ++ " wins!")
                    else do putStrLn "no one wins yet"
                            play (3-plyr) lst


type Uid = String
type Name = String
type Age = Int
data Person = Student {name :: Name, age :: Age}　--record
            | NonStudent {name :: Name, age :: Age}
            deriving (Eq, Show)

makeStudent :: IO Person --this means I will use IO, and will return a person, but does not mean I will do IO with person
makeStudent = do putStr "your name: "
                 x <- getLine
                 putStr "your age: "
                 y <- getLine
                 return Student {name = x, age = (read y :: Int)}
                 
makeNonStudent :: IO Person
makeNonStudent = 
              do putStr "your name: "
                 x <- getLine
                 putStr "your age: "
                 y <- getLine
                 return NonStudent {name = x, age = (read y :: Int)}

price :: [Person] -> Float
price persons = read (show $ foldr calcu 0 persons) :: Float
                  where calcu Student {name = x, age = y} acc
                          | y <= 18 = 0 + acc
                          | otherwise = 160 + acc
                        calcu NonStudent {name = x, age = y} acc
                          | y <= 60 = 200 + acc
                          | otherwise = 200 - y + acc

go :: IO()
go = start []

start lst = do putStr "Student? [Y/N]: "
               ans <- getLine
               if ans == "Y"
               then do person <- makeStudent
                       start (person: lst)
               else if ans == "N"
               then do person <- makeNonStudent
                       start (person: lst)
               else if ans == "-"
               then do putStrLn $ show lst
                       putStrLn ("Total price is " ++ show (price lst))
               else start lst



tt =  [Student {name = "1", age = 18}, NonStudent {name="2", age = 75}]