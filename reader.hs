{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
import Control.Monad

newtype Reader r a = MKReader {runReader :: r -> a}
instance Functor (Reader r) where
    fmap = liftM
instance Applicative (Reader r)where
    pure = return
    (<*>) = ap

{- Monad instance -}
instance Monad (Reader r) where
    return x = MKReader {runReader = const x}
    m >>= f =  MKReader {runReader = \r ->
        let res = runReader m r in 
            runReader (f res) r}
ask = MKReader { runReader = id}

local f m = MKReader {runReader = \r -> runReader m (f r)}

add5 ::Reader Int Int
add5 = ask >>= \i -> return (i+5)
mul10::Reader Int Int
mul10 = ask >>= \i -> return (i * 10)

addThenMul :: Reader Int Int
addThenMul = do
    x <- add5
    y <- mul10
    return (x+y)

readerMain :: Int -> Int
readerMain x = runReader addThenMul x

