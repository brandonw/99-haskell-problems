import Data.List

data EncodedValue a = Single a
                    | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodedValue a]
encodeModified xs = map f $ group xs
  where
    f [x] = Single x
    f x   = Multiple (length x) (head x)

decodeModified :: [EncodedValue a] -> [a]
decodeModified = concatMap expand
  where
    expand (Single x)     = [x]
    expand (Multiple n x) = replicate n x

encodeDirect :: (Eq a) => [a] -> [EncodedValue a]
encodeDirect [] = []
encodeDirect xs = map g $ foldr f [] xs
  where
    f a []          = [(a, 1)]
    f a ((b, c):ys)
        | a == b    = (b, c+1):ys
        | otherwise = (a, 1):(b, c):ys
    g (x, 1) = Single x
    g (x, n) = Multiple n x

dupli :: [a] -> [a]
dupli [] = []
dupli xs = concatMap f xs
  where
    f a = [a, a]

repli :: [a] -> Int -> [a]
repli [] n = []
repli xs n = concatMap f xs
  where
    f = replicate n

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x : xs) n = let (a, b) = split xs (n - 1) in (x : a, b)

slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b - a + 1) $ drop (a - 1) xs

rotate :: [a] -> Int -> [a]
rotate [] _         = []
rotate xs 0         = xs
rotate l@(x : xs) n = if n > 0 then rotate (xs ++ [x]) (n - 1)
                               else rotate (last l : init l) (n + 1)

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (i, j) = splitAt n xs in (last i, init i ++ j)
