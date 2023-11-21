
listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

fac :: Int -> Int
fac 0 = 1
fac n = 
    if n < 0 then
         error "bad argument"
    else n*(n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

betterFib :: Int -> Int
betterFib 0 = 0 
betterFib goal = 
    if goal < 2 then 1
    else betterFib' 0 1 2
    where 
        betterFib' prev1 prev2 idx = 
            if idx == goal then prev1 + prev2 
            else 
                betterFib' prev2 (prev1 + prev2)(idx +1)
data Tree = Leaf | Node Int Tree Tree

treeSum :: Tree -> Int
-- ^ recursively traverse binary
--   tree and compute sum
treeSum Leaf = 0
treeSum (Node x left right) = x + treeSum left + treeSum right


treeDepth :: Tree -> Int
-- ^ recursively traverse binary
--   tree and compute sum
treeDepth Leaf = 0
treeDepth (Node x left right) = 1 + max(treeDepth left) (treeDepth right)
