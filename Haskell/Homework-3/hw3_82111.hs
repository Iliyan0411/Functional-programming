import Data.Char

----- zad1 -----
countRepeated :: String -> Int      -- countRepeated изчислява колко пъти се повтаря първият символ на низа без прекъсване
countRepeated [] = 0                -- Пример: "aaabkjfff" -> 3, "aabaccd" -> 2
countRepeated [x] = 1 
countRepeated xs = if head xs == xs!!1
                   then 1 + countRepeated (tail xs)
                   else 1


encode :: String -> String
encode = helper
    where
        helper xs
            | null xs = ""
            | k <= 2 = head xs : helper (tail xs)
            | otherwise = show k ++ (head xs : helper (drop k xs))
                where
                    k = countRepeated xs
-- =================================================================


----- zad2 -----
countDigits :: Int -> Int                                           -- countDigits връща колко цифри има в дадено число
countDigits n = if n < 10 then 1 else 1 + countDigits (n `div` 10)

takeNumber :: String -> Int             -- takeNumber връща числото, което е префикс на подадения низ
takeNumber str = toInt (helper str)     -- Пример: "12abccg" -> 12
    where
        toInt :: [Int] -> Int               -- toInt превръща списък от цифри в число
        toInt xs = toIntHelper (reverse xs) 0
            where
                toIntHelper xs deg = if null xs
                                     then 0
                                     else head xs * 10 ^ deg + toIntHelper (tail xs) (deg+1)
        
        helper :: String -> [Int]                       -- helper връща списък от цифрите на числото, което е префикс на подадения низ
        helper xs = if isAlpha (head xs) || null xs     -- Пример: "12abccg" -> [1,2]
                    then []
                    else (ord (head xs) - ord '0') : helper (tail xs)


decode :: String -> String 
decode = helper
    where
        helper xs
            | null xs = ""
            | isAlpha (head xs) = head xs : helper (tail xs)
            | otherwise = replicate k (xs !! max 0 (countDigits k)) ++ helper (drop (countDigits k + 1) xs)
                where
                    k = takeNumber xs
-- =============================================================================================================


main :: IO()
main = do
    print "====== Problem_1 ======"
    print (encode "Haskell") -- -> "Haskell"
    print (encode "aaabccdefff") -- -> "3abccde3f"
    print (encode "aaaaaaaaaaaabbb") -- -> "12a3b"
    print (encode "aabbb") -- -> "aa3b"

    print "----------------"

    print "====== Problem_2 ======"
    print (decode "12a3b") -- -> "aaaaaaaaaaaabbb"
    print (decode "a3b") -- -> "abbb"
    print (decode "aa3b") -- -> "aabbb"