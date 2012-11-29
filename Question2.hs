myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:ys) = myButLast ys
