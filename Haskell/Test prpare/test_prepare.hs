import Data.Char ( ord )


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


-- zad3
type Product = (String, Double)
type StoreAvailability = [Product]

--- a)
closestToAverage :: StoreAvailability -> String
closestToAverage [_] = ""
closestToAverage products = helper (tail products) (head products)
    where
        average = sum[snd prod | prod <- products] / fromIntegral (length [snd prod | prod <- products])

        helper [] closest = fst closest
        helper products closest = 
            if abs(average - snd (head products)) < abs(average - snd closest)
            then helper (tail products) (head products)
            else helper (tail products) closest

--- b)
cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative [prod] = 0
cheaperAlternative (curr : products) = 
    if hasCheaper curr products
    then 1 + cheaperAlternative products
    else cheaperAlternative products
        where
            hasCheaper (_,_) [] = False 
            hasCheaper curr products = (fst curr == fst (head products) && snd curr /= snd (head products))
                                        || hasCheaper curr (tail products)


-- zad4
x :: (Double, Double, Double) -> Double
x (a,_,_) = a

y :: (Double, Double, Double) -> Double
y (_,b,_) = b

z :: (Double, Double, Double) -> Double
z (_,_,c) = c

minDistance :: [(Double,Double,Double)] -> Double
minDistance [] = 0
minDistance [(_,_,_)] = 0
minDistance (p : points) = helper (p : points) (d p (head points))
    where
        d p1 p2 =  (x p1 - x p2)**2 + (y p1 - y p2)**2 + (z p1 - z p2)**2

        helper [(_,_,_)] minDis = minDis
        helper (p : points) minDis = 
            if temp < minDis
            then helper points temp
            else helper points minDis
                where
                    temp = findMin [d p pnt | pnt <- points]

                    findMin [x] = x
                    findMin (d1:d2:distances) = 
                        if d1 < d2
                        then findMin (d1:distances)
                        else findMin (d2:distances)


-- zad 5
reduceStr :: String -> String
reduceStr [] = []
reduceStr str = helper str []
    where
        helper [c] res = reverse (c : res)
        helper (c : str) res =
            if abs (ord c - ord (head str)) == 32
                then helper (head res : tail str) (tail res)
                else helper str (c : res)


-- zad6
maximize :: (Ord a, Num a) => [a -> a] -> (a -> a)
maximize fs x = snd (maximum [(abs (f x), f x) | f <- fs])


-- zad7
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b 
    | a > b = True 
    | f (g a) == g (f a) = inverseFun f g (a+1) b
    | otherwise  = False 


-- zad8
data BTree = NullT | Node (Float,Float) BTree BTree

orderedTree :: BTree -> Bool 
orderedTree NullT = True 
orderedTree (Node (_,_) NullT NullT) = True 
orderedTree (Node (a, b) (Node (a1, b1) st1 st2) NullT) = a < a1 && b > b1 && orderedTree (Node (a1, b1) st1 st2)
orderedTree (Node (a, b) NullT (Node (a1, b1) st1 st2)) = a > a1 && b < b1 && orderedTree (Node (a1, b1) st1 st2)
orderedTree (Node (a, b) (Node (a1, b1) st11 st12) (Node (a2, b2) st21 st22)) = a < a1 && b > b1 &&
                                                                                a > a2 && b < b2 &&
                                                                                orderedTree (Node (a1, b1) st11 st12) &&
                                                                                orderedTree (Node (a2, b2) st21 st22)


-- zad9 
-- Да се напише функция, която пресмята средноаритметичната сумата от стойностите на върховете, които се намират на четни нива.

data Tree = Empty | Tnode Int Tree Tree

averageSumEvenLevels :: Tree -> Double
averageSumEvenLevels t = if b == 0
                         then 0
                         else a / b
    where
        a = fromIntegral (sumEvenLevels t 0)
        b = fromIntegral (countNodesOfEvenLvl t 0)

        sumEvenLevels Empty _ = 0
        sumEvenLevels (Tnode n t1 t2) k = if even k
                                          then n + sumEvenLevels t1 (k+1) + sumEvenLevels t2 (k+1)
                                          else sumEvenLevels t1 (k+1) + sumEvenLevels t2 (k+1)

        countNodesOfEvenLvl Empty _ = 0
        countNodesOfEvenLvl (Tnode n t1 t2) k = if even k
                                                then 1 + countNodesOfEvenLvl t1 (k+1) + countNodesOfEvenLvl t2 (k+1)
                                                else countNodesOfEvenLvl t1 (k+1) + countNodesOfEvenLvl t2 (k+1)


----------------------------------------
store1, store2 :: [Product]
store1=[("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]
store2=[("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]

points1 :: [(Double, Double, Double)]
points1 = [(0,0,0), (4,2,2), (2,1,6), (1,3,2)]

tree1, tree2 :: BTree
tree1 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                                        (Node (4.0,9.0) NullT NullT))
                        (Node (2.0,12.0) NullT
                                        (Node (1.0,15.0) NullT NullT))

tree2 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                        (Node (7.0,9.0) NullT NullT))       
                        (Node (2.0,12.0) NullT
                                        (Node (1.0,15.0) NullT NullT))

tree3 :: Tree
tree3 = Tnode 2 
            (Tnode 7
                (Tnode 2 Empty Empty)
                (Tnode 6
                    (Tnode 5 Empty Empty)
                    (Tnode 11 Empty Empty)))
            (Tnode 5
                Empty
                (Tnode 9
                    (Tnode 4 Empty Empty)
                    Empty))



main :: IO()
main = do
    -- zad1
    -- print (reverseOrdSuff 37563)
    -- print (reverseOrdSuff  32763)
    -- print (reverseOrdSuff 32567)
    -- print (reverseOrdSuff 32666)

    -- -- zad2
    -- print "------------"
    -- print (sumUnique [[1,2,3,2],[-4,-4],[5]])
    -- print (sumUnique [[2,2,2],[3,3,3],[4,4,4]])
    -- print (sumUnique [[1,2,3],[4,5,6],[7,8,9]])

    -- -- zad3
    -- print (closestToAverage store1) -- --> "cheese"
    -- print (cheaperAlternative store2) -- --> 1

    -- -- zad4
    -- print (minDistance points1)

    -- zad5
    -- print (reduceStr "dabAcCaCBAcCcaDD")
    -- print (reduceStr "abcCBagLl")

    -- zad 6
    print (maximize [\x -> x * x * x, (+ 1)] 0.5) -- --> 1.5
    print (maximize [\x -> x * x * x, (+ 1)] (-2)) -- --> 8

    -- zad7
    -- print (inverseFun (\x -> x+1) (\x -> x-1) 5 10) -- --> True
    -- print (inverseFun (\x -> x*x) (\x -> x^3) 0 1) -- --> True
    -- print (inverseFun (\x -> x+1) (\x -> x+2) 0 1) -- --> True
    -- print (inverseFun (\x -> 2*x) (\x -> x+2) 0 1) -- --> False

    -- zad8
    -- print (orderedTree tree1)
    -- print (orderedTree tree2)

    -- zad9
    -- print(averageSumEvenLevels tree3)

    