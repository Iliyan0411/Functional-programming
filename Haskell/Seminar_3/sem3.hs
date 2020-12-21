main :: IO()
main = do
    print(isTriangular [[1,2,3], [0,5,7], [7,0,3]])
    print(divisors 15)
    print(primesInRange 5 20)





-- zad1
isTriangular :: [[Int]] -> Bool
isTriangular mx = helper mx 0
    where
        helper [] _ = True
        helper mx count = 
            count == length (filter (\x -> x==0) (take count (head mx))) && helper (tail mx) (count+1)

-- zad2
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

-- zad3
prime :: Integer -> Bool
prime n = helper (n-1)
    where 
        helper d
            | d <= 1 = True
            | n `mod` d == 0 = False
            | otherwise = helper (d-1)

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a..b], (prime x)]