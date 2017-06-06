act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x, y)

getLine ::IO String
getLine = do x <- getChar
             if x == '\n' then return "" 
             else 
               do xs <- getLine
                  return (x:xs) 

--if no action then return emptym with (), it will not have any side effect
putStr :: String -> IO ()
putStr [] = 
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn s = 
  do putSr s
     putChar '\n'