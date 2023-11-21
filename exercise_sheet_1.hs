{- Solutions for Exercise Sheet 1 -}

{- Functions -}
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max x $ max y z

-- Note, this can also be written f2 . f1
f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
f f1 f2 = \i -> f2 (f1 i)

-- Note, this is the same idea as the above function
g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
g f1 f2 x = f2 $ f1 x

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

force :: Float -> Float -> Float -> Float
force m1 m2 d = (g * m1 * m2) / (d ^ 2)
    where g = 6.67 * (10 ^^ (-11))

{- List Comprehensions -}
divByThree :: [Int]
divByThree = [ x | x <- [1..30], x `mod` 3 == 0 ]

triangles :: Int -> [Int]
triangles n = [ (x * (x - 1)) `div` 2 | x <- [1..n] ]

primes :: Int -> [Int]
primes n = [ x | x <- [1..n], isPrime x ]
    where
        divisors x = [ y | y <- [2..(x - 1)], x `mod` y == 0 ]
        isPrime x = length (divisors x) == 0

flatten :: [[a]] -> [a]
flatten xs = [ y | x <- xs, y <- x ]

{- Further Exercises -}
isbnCheck :: [Int] -> String
isbnCheck digits = if digit == 10 then "X" else (show digit)
    where
        coefficients = reverse [2..10]
        pairs = zipWith (*) coefficients digits
        summed = sum pairs
        digit = 11 - (summed `mod` 11)
