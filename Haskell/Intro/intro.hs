-- div = quotient
-- mod = remainder


add :: Int -> Int
add x = x + 1 

f :: Int -> Int -> Int
f x y = x + y

myMin :: Int -> Int -> Int
myMin x y = if x <= y then x else y

myMin' :: Int -> Int -> Int
myMin' x y
    | x <= y = x
    | otherwise = y

contain :: Int -> Int -> Int -> Bool
contain x a b 
    | a <= x && x <= b = True
    | otherwise = False

myfunc :: Int -> Int -> Double
myfunc x y = fromIntegral(square x + square y) / 2
    where
        square x = x * x

fib :: Int -> Integer
fib n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fibIter :: Int -> Integer
fibIter 0 = 0
fibIter 1 = 1
fibIter n = helper 0 0 1
    where
        helper i prev curr = 
            if i >= n then prev else helper (i + 1) curr (prev + curr)

myNOD :: Int -> Int -> Int
myNOD a b
    | a == b = a
    | a > b = myNOD (a-b) b
    | otherwise = myNOD a (b - a)


mymaxadvisor :: Int -> Int
mymaxadvisor x = helper (x - 1)
    where
        helper d
            | x `mod` d == 0 = d
            | otherwise      = helper (d - 1)

oddSum :: Int -> Int -> Int
oddSum a b = helper a
   where
       helper a
        | a > b = 0
        | even a = helper (a + 1)
        | otherwise = a + helper (a + 2)

prime :: Int -> Bool
prime n = helper (n-1)
    where 
        helper d
            | d <= 1 = True
            | n `mod` d == 0 = False
            | otherwise = helper (d-1)

digitsNum :: Int -> Int
digitsNum n = if n < 10 then 1 else 1 + digitsNum (n `div` 10)

palindrom :: Int -> Bool
palindrom n = helper n 0 (digitsNum n - 1)
    where
        helper temp res deg
            | deg >= 0 = helper (temp `div` 10) (res + (temp `mod` 10) * 10^deg) (deg - 1)
            | deg < 0 && n == res = True
            | otherwise = False

palindromSum :: Int -> Int -> Int
palindromSum a b = helper a
    where
        helper a
            | a > b = 0
            | palindrom a = a + helper (a+1)
            | otherwise = helper (a+1)


numDivisors :: Int -> Int
numDivisors n = helper (n-1) 0
    where
        helper d count
            | d <= 1 = count
            | n `mod` d == 0 = helper (d-1) (count+1)
            | otherwise = helper (d-1) count


--main==============
main :: IO()
main = do
    print (numDivisors 21)
    
