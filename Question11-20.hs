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
