import System.Random
import Control.Monad

insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x l@(y : ys) i
    | i <= 0    = error "index must be one or greater"
    | i == 1    = x : l
    | otherwise = y : insertAt x ys (i - 1)

range :: Int -> Int -> [Int]
range x y = range' x y []
  where
    range' x y zs
        | x == y = y : zs
        | x < y = range' x (y - 1) (y : zs)
        | x > y = range' x (y + 1) (y : zs)

rndSelect :: [a] -> Int -> [a]
rndSelect _ 0   = []
rndSelect [] _  = []
rndSelect xs n
    | n < 0     = []
    | otherwise = do
        g <- getStdGen
        return $ take n $ map (xs !!) $ randomRs (0, length xs - 1) g
