fst3 :: (a,b,c) -> a -- връща първия елемент на вектор с размер 3
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b -- връща втория елемент на вектор с размер 3
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c -- връща третия елемент на вектор с размер 3
thd3 (_,_,c) = c
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
----- Problem_1 -----
type Account = (Int, Int, Double)
type Person = (Int, String, String)

----- a)
getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance ([], []) _ = 0
getAverageBalance ([], [_]) _ = 0
getAverageBalance ([_], []) _ = 0
getAverageBalance (accounts, persons) pred = helper persons 0 0
    where
        helper [] 0 _ = 0
        helper persons result div
            | null persons = result / div
            | pred (head persons) = helper (tail persons) 
                                           (result + fst(wantedAcc (head persons))) 
                                           (div + snd(wantedAcc (head persons)))
            | otherwise = helper (tail persons) result div
                where
                    -- връща двойка от сумата на всички сметки на дадения човек
                    -- и броя на сметките, които той притежава
                    wantedAcc person = (sum pAcc, fromIntegral (length pAcc))
                        where
                            -- връща списък от сметките, които даденият човек има в банката 
                            pAcc = [thd3 acc | acc <- accounts, snd3 acc == fst3 person]

----- b)
type Balance = Double

averageBalanceOfCities :: ([Account],[Person]) -> [String] -> Balance
averageBalanceOfCities ([], []) [_] = 0
averageBalanceOfCities ([_], []) [_] = 0
averageBalanceOfCities ([], [_]) [_] = 0
averageBalanceOfCities ([_], [_]) [] = 0
averageBalanceOfCities (accounts, persons) cities = average 
    where
        -- списък на всички хора от подадените като аргумент градове
        wantedPersons = [prs | prs <- persons, length (filter (== thd3 prs) cities) > 0]

        -- списък на парите във всяка от сметките на хората от wantedPersons
        allAcc = [thd3 acc | acc <- accounts, length (filter (\x -> fst3 x==snd3 acc) wantedPersons)>0]
        
        -- сумата на парите във всички сметки от allAcc радзелена на броя сметки в allAcc
        average = sum allAcc / fromIntegral (length allAcc)

----------------------------------------------------------------------------------------------------
-- inputs
people1 :: [Person]
people1 = [(1,"Ivan","Sofia"),(2,"Georgi","Burgas"), (3,"Petar","Plovdiv"),(4,"Petya","Burgas")]

accounts1 :: [Account]
accounts1 = [(1,1,12.5),(2,1,123.2),(3,2,13.0),(4,2,50.2),(5,2,17.2), (6,3,18.3),(7,4,19.4)]


t1 :: BTree
t1 = Node 16 (Node 0 Empty Empty)
    (Node 4 (Node 1 Empty Empty)
    (Node 0 Empty Empty))

t2 :: BTree
t2 = Node 4 (Node 0 Empty Empty)
    (Node 2 (Node 1 Empty Empty) 
    Empty) 
----------------------------------------------------------------------------------------------------

----- Problem_2 -----
data BTree = Empty | Node Int BTree BTree

countInteresting :: BTree -> Int
countInteresting Empty = 0
countInteresting (Node 1 Empty Empty) = 1
countInteresting (Node 2 Empty t2) = 1 + countInteresting t2
countInteresting (Node 2 t1 Empty) = 1 + countInteresting t1
countInteresting (Node 4 (Node m t11 t12) (Node n t21 t22)) = 1 +
                                                              countInteresting (Node m t11 t12) + 
                                                              countInteresting (Node n t21 t22)
countInteresting (Node _ t1 t2) = countInteresting t1 + countInteresting t2

---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
    ----- Problem_1 -----
    ----- a)
    print (getAverageBalance (accounts1,people1) (\ (_,_,city) -> city == "Burgas")) -- --> 24.950000000000003
    print (getAverageBalance (accounts1,people1) (\ (_,n:_,_) -> n == 'P')) -- --> 18.85

    print ("------------")

    ----- b)
    print (averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"])-- --> 67.85
    print (averageBalanceOfCities (accounts1,people1) ["Burgas","Gabrovo","Plovdiv"])-- --> 23.62

    print ("------------")

    ----- Problem_2 -----
    print (countInteresting t1) -- -> 2 (4=2^2, 1=2^0)
    print (countInteresting t2) -- -> 3 (4=2^2, 2=2^1, 1=2^0)