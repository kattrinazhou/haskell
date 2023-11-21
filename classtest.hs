
{-- question 1
Consider the following Haskell source code that models dinosaurs, which may have wings and/or
spikey bodies and some integer number of legs

Q1a (5 marks)
Critique whether or not this is an appropriate way to model dinosaurs in Haskell. Briefly present both
sides of the argument and come to a conclusion
--}
-- yes
{--Question 1b--}
{--test using Main.show [ pterodactyl , stegosaurus , diplodocus ]--}

data Dinosaur = Dinosaur Bool Bool Int deriving Eq
pterodactyl = Dinosaur True True 2
stegosaurus = Dinosaur False True 4
diplodocus = Dinosaur False False 4

getlegnum :: Dinosaur -> Int
getlegnum (Dinosaur _ _ x) = x

representlegs :: Dinosaur -> [Char]
representlegs name =
    init (formspikey (isspikey name) (replicate (getlegnum name) "b"))

iswinged :: Dinosaur -> Bool
iswinged (Dinosaur bool _ _) = bool

isspikey :: Dinosaur -> Bool
isspikey (Dinosaur _ bool _) = bool


formspikey :: Bool -> [[Char]] -> [Char]
formspikey _ [] = []
formspikey bool (x:xs) =
    if bool == True then x ++ '^' : formspikey bool xs
    else x ++ '-' : formspikey bool xs


show :: Dinosaur -> [Char]
instance Show Dinosaur where
show name=
    if iswinged name
        then ">" ++ dino_str ++ "<"
    else dino_str
    where dino_str = representlegs name


{--Question 2--}


-- 2a
-- sumFromOneTo 0 = 0
sumFromOneTo n
    | n < 1  =  0  {-base case-}
    | otherwise = n + sumFromOneTo (n-1) {-recursive case-}
-- 2b
sumWithAccFromOneTo :: Int -> Int -> Int
sumWithAccFromOneTo n accumulate
    | n < 1  =  accumulate  {-base case-}
    | otherwise = sumWithAccFromOneTo (n - 1) (accumulate + n) {-recursive case-}
{--Because of this code, the second parameter is used to record the current cumulative sum, but the first one is recalculated every time it is called, so it reduces resource waste and reduces the number of recursive calls--}
-- 2c
sumWithFoldFromOneTo :: Int -> Int
sumWithFoldFromOneTo n = foldr (+) 0 [1..n]
-- 2d
prop_sum n = sumFromOneTo n == sumWithFoldFromOneTo n

{--Question 3--}

superpower :: (Ord t, Floating t) => t -> t -> t
superpower a 2 = a ** 2
superpower a n
    | n < 2 = a {-base case-}
    | otherwise = superpower (a ** n) (n-1)



superpower' :: Int -> Int -> Int
superpower' a 2 = a ^ 2
superpower' a n
    | n < 2 = a {-base case-}
    | otherwise = superpower' (a ^ n) (n-1)


-- superpower' a n = foldl (**) a [1..n]

superpower'' a n = a ** product [1..n]