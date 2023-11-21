

doubleMe x = x + x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
addOne :: Int -> Int
addOne = (+1)

addOneTo :: Int -> Int
addOneTo = (+) 1

max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = if (max2 x y) > z then (max2 x y) else z

f :: (Int -> String) -> (String -> Bool) -> (Int -> Bool)
fun1 :: Int -> String
fun2 :: String -> Bool
fun1 x = "apple"
fun2 y = False
f fun1 fun2 = \x -> fun2 (fun1 x)

g :: (Int -> Bool) -> (Bool -> String) -> Int -> String
tobool :: Int -> Bool
toString :: Bool -> String
tobool x = if x < 5 then False else True
toString x = if x == True then "5 more" else "5 less"
g tobool toString x = toString (tobool x)

twice :: (Int -> Int) -> Int -> Int
func1 :: Int -> Int
func1 x = x + 1
twice func1 x = func1 (func1 x)

mod3 :: [Int]
mod3 = [x | x <- [1..30], x `mod` 3 == 0]

triangles :: Int -> [Int]
triangles n = [x * (x - 1) `div` 2 | x <- [1..n]]


-- isPrime :: Int -> Bool
-- isPrime n
--   | n <= 1    = False
--   | n <= 3    = True
--   | otherwise = all (\x -> n `mod` x /= 0) [2..sqrtN]
--   where sqrtN = floor (sqrt (fromIntegral n))

-- primes :: Int -> [Int]
-- primes n = filter isPrime [2..n]

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n <= 3    = True
  | n `mod` 2 == 0 || n `mod` 3 == 0 = False
  | otherwise = go 5
  where
    go k
      | k * k > n      = True
      | n `mod` k == 0 = False
      | otherwise      = go (k + 2)

primes :: Int -> [Int]
primes n = filter isPrime [2..n]


