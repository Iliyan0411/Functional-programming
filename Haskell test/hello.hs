main = do
 print "My first Haskell program"
 name <- getLine
 print ("Hello, " ++ name)

f :: Num a => a -> a -> a
f x y = x * y
