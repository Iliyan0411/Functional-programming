countInfixes :: String -> String -> Int 
countInfixes [] _ = 0
countInfixes _ [] = 0
countInfixes s str
    | length s > length str = 0
    | isPrefix s str = 1 + countInfixes s (drop (length s) str)
    | otherwise = countInfixes s (tail str)
        where
            isPrefix [] _ = True
            isPrefix _ [] = False
            isPrefix s str = length s <= length str && s == take (length s) str



main :: IO()
main = do
    print (countInfixes "fish" "My favorite songs are Paradise city and I want to break free. Also my dad goes every day to fishing and don't catch anything.")