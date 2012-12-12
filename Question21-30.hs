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

rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0   = return []
rndSelect [] _  = return []
rndSelect xs n
    | n < 0     = return []
    | otherwise = do
        g <- getStdGen
        return $ take n $ map (xs !!) $ randomRs (0, length xs - 1) g

rndSelect' :: [a] -> Int -> IO [a]
rndSelect' _ 0  = return []
rndSelect' [] _ = return []
rndSelect' xs n
    | n < 0     = return []
    | otherwise =  rndSelect'' xs n []
      where
        rndSelect'' _ 0 rs  = return rs
        rndSelect'' xs' c rs = do
            r <- randomRIO (0, length xs' - 1)
            rndSelect'' (take r xs' ++ drop (r + 1) xs') (c - 1) $ (xs' !! r) : rs

diffSelect :: Int -> Int -> IO [Int]
diffSelect 0 _   = return []
diffSelect n max = rndSelect' [1..max] n

rndPermu :: [a] -> IO [a]
rndPermu []  = return []
rndPermu [x] = return [x]
rndPermu xs  = rndPermu' xs []
  where
    rndPermu' [] rs  = return rs
    rndPermu' xs' rs = do
        r <- randomRIO (0, length xs' - 1)
        rndPermu' (take r xs' ++ drop (r + 1) xs') $ xs' !! r : rs
