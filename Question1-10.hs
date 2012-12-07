import Data.List

myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:ys) = myButLast ys

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

myLength :: [a] -> Int
myLength xs = myLength' xs 0
  where
    myLength' [] y     = y
    myLength' (_:as) y = myLength' as y+1

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
  where
    reverse'' [] ys     = ys
    reverse'' (y:ys) zs = reverse'' ys (y:zs)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse' xs == xs

data NestedList a
    = Elem a
    | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: (Eq a) => [a] -> [a]
compress = map head . group

compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress' ys
    | otherwise = x : compress' ys
compress' xs = xs

pack :: (Eq a) => [a] -> [[a]]
pack = group

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = pack'' xs [[x]]
  where
    pack'' [] bs                = bs
    pack'' (a:bs) cs
        | a == (head . last) cs = pack'' bs $ init cs ++ [a : last cs]
        | otherwise             = pack'' bs $ cs ++ [[a]]

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\a -> (length a, head a)) (group xs)
