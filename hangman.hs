import System.IO

hangman :: IO ()
hangman =
    do putStrLn "Think of a word: "
       word <- sgetLine
       putStrLn "Try to guess it: "
       play word

getCh ::IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c 

-- secretely get a line an echo '-' for each character
sgetLine :: IO String
sgetLine = do c <- getCh
              if c == '\n' then 
              	do putChar c
              	   return []
              else
              	do putChar '-'
              	   cs <- sgetLine
                   return (c:cs)


match :: String -> String -> String
match xs ys = [ if x `elem` ys then x else '-' | x <- xs]

play ::String -> IO ()
play word = do putStr "? " --$ match word guess
               guess <- getLine
               if gess == word then
               	   putStrLn "You got it!"
               else 
                   do putStrLn (match word guess)	

