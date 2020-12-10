-- zad1
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n =
    if null xs
    then []
    else (head xs + n : incrementAllBy (tail xs) n)

-- zad2
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = [n * x | x <- xs]

-- zad3
isImage :: [Int] -> [Int] -> Bool
isImage as bs = helper (tail as) (tail bs)
    where
        k = head as - head bs
        helper :: [Int] -> [Int] -> Bool
        helper as bs
         | null as = True
         | head as - head bs == k = helper (tail as) (tail bs)
         | otherwise = False

-- zad4
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs a = [x | x <- xs, x >= a]

-- zad5
isAscending :: Integer -> Bool
isAscending n = helper (n `div` 10) [(n `mod` 10)]
    where
        helper :: Integer -> [Integer] -> Bool
        helper n xs
            | n == 0 = True
            | n `mod` 10 <= head xs = helper (n `div` 10) (n `mod` 10 : xs)
            | otherwise = False




main :: IO()
main = do
    print(incrementAllBy [1,2,3,4,5] 3)
    print(multiplyAllBy [1,2,3,4,5] 2)
    print(isImage [1,2,3,4] [3,4,5,6])
    print(filterSmallerThan [1,2,3,6,8,5,4,2,3,6] 4)
    print(isAscending 123455)
