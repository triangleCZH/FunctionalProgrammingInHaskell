module Tutorial7 where

import           Data.Char (isLetter, toLower)
import           Text.Read (readMaybe)


-- Q1

palindrome :: IO ()
palindrome = do
  rawLine <- getLine
  let line = map toLower $ filter isLetter rawLine
  putStrLn $ if line == reverse line then "It’s a palindrome!" else "Nope"


-- Q2

type Board = [Int]


nim :: IO ()
nim = loop [5, 4..1] 1
  where
    loop :: Board -> Int -> IO ()
    loop board player = do
      printBoard board
      if isEmpty board
        then putStrLn $ "Player " ++ show (theOther player) ++ " wins!"
        else do
          putStrLn $ "Player " ++ show player
          (row, num) <- askForInput board
          let newBoard = updateBoard board row num
          putStrLn ""
          loop newBoard (theOther player)


printBoard :: Board -> IO ()
printBoard board = putStrLn $ concat $ zipWith showRow board [1..]
  where
    showRow n r = show r ++ ":" ++ concat (replicate n " *") ++ "\n"


isEmpty :: Board -> Bool
isEmpty = all (== 0)


askForInput :: Board -> IO (Int, Int)
askForInput board = do
  row <- askInt "Enter a row number: " (\x -> x >= 1 && x <= 5 && board !! (x - 1) > 0)
  num <- askInt "Stars to remove: " (\x -> x >= 1 && x <= board !! (row - 1))
  return (row, num)


askInt :: String -> (Int -> Bool) -> IO Int
askInt msg cond = do
  putStr msg
  line <- getLine
  case readMaybe line of
    (Just x) | cond x -> return x
    _        -> askInt msg cond


updateBoard :: Board -> Int -> Int -> Board
updateBoard board row num = let (l, r:rs) = splitAt (row - 1) board in l ++ (r - num : rs)


theOther :: Int -> Int
theOther p = if p == 1 then 2 else 1


-- Q3

type Uid = String

type Name = String

type Age = Int

data Person = Student {name :: Name, age :: Age} | NonStudent {name :: Name, age :: Age} deriving (Eq, Show)


make :: (Name -> Age -> Person) -> IO Person
make f = do
  n <- askString "Input name: "
  a <- askInt "Input age: " (const True)
  return $ f n a


makeStudent :: IO Person
makeStudent = make Student


makeNonStudent :: IO Person
makeNonStudent = make NonStudent


askString :: String -> IO String
askString msg = do
  putStr msg
  getLine


price :: [Person] -> Float
price = sum . map calPrice
  where
    calPrice :: Person -> Float
    calPrice (Student _ a) | a <= 18 = 0
                           | otherwise = 200 * 0.8
    calPrice (NonStudent _ a) | a <= 60 = 200
                              | otherwise = fromIntegral (max (200 - a) 0)

go :: IO ()
go = loop []
  where
    loop :: [Person] -> IO ()
    loop p = do
      yn <- askString "Student? [Y/N]: "
      case yn of
        "Y" -> do
          s <- makeStudent
          loop (s:p)
        "N" -> do
          n <- makeNonStudent
          loop (n:p)
        _ -> do
          print $ reverse p
          putStr "Total price is "
          print $ price p

--this will automatically get "let " in front, this is a bug 
--for sublime, the ghci will not do this
trial = do x <- getLine
           putStr x