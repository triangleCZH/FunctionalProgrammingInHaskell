module Hangman where

import System.IO

{-
One player secretly types in a word.

The other player tries to deduce the word, by entering a sequence of guesses. 

For each guess, the computer indicates which letters in the secret word occur in the guess.

The game ends when the guess is correct.
-}

hangman :: IO ()
hangman  =
   do putStrLn "Think of a word: "
      word <- sgetLine
      putStrLn "Try to guess it:"
      play word

getCh :: IO Char  
getCh  = do hSetEcho stdin False               
            c <- getChar              
            hSetEcho stdin True              
            return c

-- secretely get a line an echo '-' for each character!

sgetLine :: IO String
sgetLine  = do c <- getCh
               if c == '\n' then
                  return []
               else do putChar '-'
                       cs <- sgetLine
                       return (c:cs)  

-- match "Hello" "e" ~> "-e---"

-- elem "e" "Hello"

match :: String -> String -> String
match xs ys =
  [if elem x ys then x else '-' | x <- xs] 

play     :: String -> IO ()
play word = do guess <- getLine
               putStr $ match word guess
               return ()