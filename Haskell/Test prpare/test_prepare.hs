-- zad1
reverseOrdSuff :: Int -> Int
reverseOrdSuff num = helper num (newNumLen num)
    where
        newNumLen n
            | n < 10 = 1
            | n `mod` 10 < (n `mod` 100) `div` 10 = 1 + newNumLen (n `div` 10)
            | otherwise = 0

        helper num deg = if deg < 0
                         then 0
                         else num `mod` 10 * 10 ^ deg + helper (num `div` 10) (deg - 1)

-- zad2
sumUnique :: [[Int]] -> Int
sumUnique = helper
    where
        helper [] = 0 
        helper xss
            | null (head xss) = helper (tail xss)
            | length[elem | elem <- head xss, elem==head (head xss)] > 1 = helper([el | el <- head xss, el/=head (head xss)] : tail xss)
            | otherwise = head (head xss) + helper (tail (head xss) : tail xss)






main :: IO()
main = do
    -- zad1
    print (reverseOrdSuff 37563)
    print (reverseOrdSuff  32763)
    print (reverseOrdSuff 32567)
    print (reverseOrdSuff 32666)

    -- zad2
    print "------------"
    print (sumUnique [[1,2,3,2],[-4,-4],[5]])
    print (sumUnique [[2,2,2],[3,3,3],[4,4,4]])
    print (sumUnique [[1,2,3],[4,5,6],[7,8,9]])