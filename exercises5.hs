{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{- Week 6 exercise sheet -}
import Prelude hiding (mapM, sequence, Either, Left, Right, log)
import Control.Monad (ap, liftM)
import GHC.Plugins (msHsFilePath)
import Control.Arrow (ArrowChoice(right))

{- Part 1: Implementing Monadic Utility Functions -}

forever :: Monad m => m a -> m b
forever m = m >> forever m

void :: Monad m => m a -> m ()
void m = m >> return()

join :: Monad m => m (m a) -> m a
join m = m >>= id

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = do
    x<- m
    xs <- sequence ms
    return (x:xs)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ _ = undefined

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ _ _ = undefined

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ _ _ = undefined

{- Part 2: Implementing the Either Monad -}

data Either a b = Left a | Right b

instance Functor (Either a) where
    fmap = liftM

instance Applicative (Either a) where
    pure = return
    (<*>) = ap

instance Monad (Either a) where
    return  = Right
    (Left x) >>= _ = Left x 
    (Right y) >>= f = f y
    m  >>= f = undefined

{- Part 3: Implementing the Writer Monad -}

newtype Writer w a = MkWriter { runWriter :: (a, w) }

instance Monoid w => Functor (Writer w) where
    fmap = liftM

instance Monoid w => Applicative (Writer w) where
    pure = return
    (<*>) = ap

instance Monoid w => Monad (Writer w) where
    return x = MkWriter { runWriter = (x, mempty)}
    m >>= f  = MkWriter {
        runWriter = 
            let (res , s) = runWriter m in
            let (res2, s2) = runWriter (f res) in
                (res2, s <> s2)
            }

writer :: Monoid w => (a, w) -> Writer w a
writer _ = undefined

tell :: Monoid w => w -> Writer w ()
tell _ = undefined

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen _ = undefined

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor _ _ = undefined

{- Example use of the Writer monad -}
type Log = [String]

log :: String -> Writer Log ()
log x = tell [x]

add5 :: Int -> Writer Log Int
add5 x = log msg >> return res
    where res = x + 5
          msg = "Added 5 to " ++ (show x)

mul10 :: Int -> Writer Log Int
mul10 x = log msg >> return res
    where res = x * 10
          msg = "Multiplied " ++ (show x) ++ " by 10"

add :: Int -> Int -> Writer Log Int
add x y = log msg >> return (x + y)
    where msg = "Added " ++ (show x) ++ " to " ++ (show y)

addThenMul :: Int -> Writer Log Int
addThenMul x = do
    added <- add5 x
    multiplied <- mul10 x
    add added multiplied

-- Note you can test your writer implementation by writing
-- runWriter (addThenMul 5)
-- in GHCI.

