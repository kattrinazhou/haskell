{-# LANGUAGE BlockArguments #-}
import Control.Monad
import GHC.CmmToAsm.AArch64.Instr (x0)
import Prelude hiding (putStrLn, putStr)
import GHC.Core.TyCon.Env (nameEnvElts)
import GHC.Builtin.Types.Prim (doubleX2PrimTy)

act = do
    x <- getChar
    y <- getChar
    z <- getChar
    return (x,y,z)

putStr:: String -> IO()
putStr [] = return ()
putStr (x:xs)= do
            putChar x
            putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
            putStr xs
            putChar '\n'
strlen :: IO()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has"
            putStr (show (length xs))
            putStrLn "  characters"


speak = do
    putStrLn "enter your name"
    name <- getLine
    putStrLn $ "hello , " ++ name ++ "!"

main :: IO Int
main = do
    x <- getLine
    y <- getLine
    return ((read x:: Int) + (read y :: Int))






