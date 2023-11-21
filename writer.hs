{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
import Prelude hiding (mapM, sequence, Either, Left, Right, log)
import Control.Monad (ap, liftM)

newtype Writer w a = MkWriter { runWriter :: (a, w) }

instance Monoid w => Functor (Writer w) where
    fmap = liftM

instance Monoid w => Applicative (Writer w) where
    pure = return
    (<*>) = ap

instance Monoid w => Monad (Writer w) where
    return x = MkWriter { runWriter = (x, mempty)}
    m >>= f  = 
        MkWriter {
            runWriter =
                let (res, s) = runWriter m in
                let (res2, s2) = runWriter (f res) in
                (res2, s <> s2)
        }


writer :: Monoid w => (a, w) -> Writer w a
writer (x, w) = MkWriter { runWriter = (x, w) }

tell :: Monoid w => w -> Writer w ()
tell w = MkWriter { runWriter = ((), w) }

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen m = MkWriter { runWriter = ((res, w), w) }
    where (res, w) = runWriter m

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f m = MkWriter { runWriter = (res, f w) }
    where (res, w) = runWriter m

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
