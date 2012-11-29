myLength :: [a] -> Int
myLength xs = myLength' xs 0
    where
        myLength' [] y     = y
        myLength' (_:as) y = myLength' as y+1
