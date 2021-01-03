fst3 :: (a,b,c) -> a -- връща първия елемент на вектор с размер 3
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b -- връща втория елемент на вектор с размер 3
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c -- връща третия елемент на вектор с размер 3
thd3 (_,_,c) = c
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

----- Problem_1 -----
type Account = (Int, Int, Double)
type Person = (Int, String, String)

----- a)
getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance ([], []) _ = 0
getAverageBalance ([], [_]) _ = error "Invalid arguments"
getAverageBalance ([_], []) _ = error "Invalid arguments"
getAverageBalance dataBase pred = helper dataBase 0 0
    where
        -- helper-функция реализираща getAverageBalance итеративно
        helper :: ([Account],[Person]) -> Double -> Double ->Double
        helper dataBase result div
            | null (snd dataBase) = result / div
            | pred currPerson = helper (fst dataBase, tail (snd dataBase)) 
                                        (result + fst (balance (fst3 currPerson) (fst dataBase)))
                                        (div + snd (balance (fst3 currPerson) (fst dataBase)))
            | otherwise = helper (fst dataBase, tail (snd dataBase)) result div
                where
                    currPerson = head (snd dataBase) -- текущият човек, който разглеждаме от базата данни

                    -- връща двойка от сумата на парите във всички сметки на човека в дадената банка 
                    -- и съответно броят на сметките, които той притежава
                    balance :: Int -> [Account] -> (Double, Double)
                    balance _ [] = (0, 0)
                    balance personID accounts = helper accounts 0 0
                        where
                            -- helper-функция реализираща balance итеративно
                            helper :: [Account] -> Double -> Double -> (Double, Double)
                            helper accounts result counter
                                | null accounts = (result, counter)
                                | personID == snd3 (head accounts) = helper (tail accounts) 
                                                                            (result + thd3 (head accounts))
                                                                            (counter + 1)
                                | otherwise = helper (tail accounts) result counter
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

-- inputs
people1 :: [Person]
people1 = [(1,"Ivan","Sofia"),(2,"Georgi","Burgas"), (3,"Petar","Plovdiv"),(4,"Petya","Burgas")]

accounts1 :: [Account]
accounts1 = [(1,1,12.5),(2,1,123.2),(3,2,13.0),(4,2,50.2),(5,2,17.2), (6,3,18.3),(7,4,19.4)]


---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
main :: IO()
main = do
    print (getAverageBalance (accounts1,people1) (\ (_,_,city) -> city == "Burgas")) -- --> 24.950000000000003
    print (getAverageBalance (accounts1,people1) (\ (_,(n:_),_) -> n == 'P')) -- --> 18.85