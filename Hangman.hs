{-# LANGUAGE BlockArguments #-}

import Control.Concurrent (getChanContents)
import Control.Monad.Trans.RWS.Lazy (put)
import System.IO (hSetEcho)
import GHC.IO.StdHandles (stdin)


hangman = do
  putStrLn "Think a word:"
  word <- sgetLine
  putStrLn "Try to guess it : "
  play word

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n'then 
        do
            putChar x
            return []
    else 
        do
            putChar '-'
            xs <- sgetLine
            return (x : xs)

-- getCh : IO Char
getCh = do 
    hSetEcho stdin False --相当于隐藏输入的单词
    x <- getChar
    hSetEcho stdin True
    return x

play word = do 
    putStr "?"
    guess <- getLine
    if guess == word then
        putStrLn "You got it!!"
    else 
        do 
        putStrLn (match word guess)
        play word
    where
        match xs ys = [if elem x ys then x else '-' | x <- xs]
