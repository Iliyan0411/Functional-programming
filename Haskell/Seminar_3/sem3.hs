main :: IO()
main = do
    -- print (isTriangular [[1,2,3], [0,5,7], [7,0,3]])
    -- print (divisors 15)
    -- print (primesInRange 5 20)
    -- print (prodSumDiv [6,7,8,9] 2)
    -- print (isSorted [1,2,3,4,5,5])
    -- print (merge [1,4,5,8] [2,3,10,11])
    -- print (insert [1,2,4,5] 3)
    -- print (insertionSort [5,3,2,4,7,8,9,6,54,4])
    -- print(f (* 5))
    -- print(myCompose'' (\x -> x+1) (\x -> x ^ 2) 2)
    print 4


-- zad1
isTriangular :: [[Int]] -> Bool
isTriangular mx = helper mx 0
    where
        helper [] _ = True
        helper mx count = 
            count == length (filter (== 0) (take count (head mx))) && helper (tail mx) (count+1)

-- zad2
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..(n-1)], n `mod` d == 0]

-- zad3
prime :: Integer -> Bool
prime n = helper (n-1)
    where 
        helper d
            | d <= 1 = True
            | n `mod` d == 0 = False
            | otherwise = helper (d-1)

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a..b], prime x]

-- zad4
prodSumDiv :: [Integer] -> Integer -> Integer 
prodSumDiv xs k = helper xs
    where 
        helper [] = 1
        helper xs = if sum (divisors (head xs)) `mod` k == 0
                    then head xs * helper (tail xs)
                    else helper (tail xs)

-- zad5
isSorted :: [Int] -> Bool
isSorted = helper
    where
        helper [] = True
        helper [x] = True 
        helper xs = head xs <= xs!!1 && helper (tail xs) 

-- zad6
merge :: [Int] -> [Int] -> [Int]
merge xs ys = qsort (xs ++ ys)
    where
        qsort [] = []
        qsort (x:xs) = smaller ++ [x] ++ qsort larger
            where
                smaller = [a | a <- xs, a <= x]
                larger = [b | b <- xs, b > x] 

-- zad7
insert :: [Int ] -> Int -> [Int ]
insert xs elem = helper xs
    where
        helper [] = [elem]
        helper (x:xs) = if elem <= x
                        then elem : x : xs
                        else x : helper xs

-- zad8
insertionSort :: [Int ] -> [Int ]
insertionSort xs = helper xs []
    where
        helper xs result = if null xs
                           then result
                           else helper (tail xs) (insert result (head xs))


-- Lambda functions
f :: (Int -> Int) -> Int
f func = func 2

myCompose :: (a -> b) -> (c -> a) -> (c -> b)
myCompose f g x = f (g x)

myCompose' :: (a -> b) -> (c -> a) -> (c -> b)
myCompose' f g = \x -> f(g x)

myCompose'' :: (a -> b) -> (c -> a) -> (c -> b)
myCompose'' f g = f . g
