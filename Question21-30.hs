import System.Random
import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashTable.IO as H

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

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = []
combinations _ [] = []
combinations n ks
    | n < 0       = error "Number of elements cannot be < 0"
    | otherwise   = filter ((n ==) . length) $ subsequences ks

combinations' :: Int -> [a] -> [[a]]
combinations' 0 _  = []
combinations' 1 ns = map (:[]) ns
combinations' k ns = do (x, i) <- zip ns [0..length ns - 1]
                        map (x:) $ combinations' (k - 1) $
                                                 take i ns ++ drop (i + 1) ns

lsort :: [[a]] -> [[a]]
lsort = sortBy sort'
  where
    sort' xs ys
        | lx > ly   = GT
        | lx < ly   = LT
        | otherwise = EQ
          where
            lx = length xs
            ly = length ys

type HashTable k v = H.BasicHashTable k v

{-lfsort :: [[a]] -> [[a]]-}
{-lfsort xss = map fst $ sortBy sortT $ map freqTuple xss-}
  {-where-}
    {-sortT (_, t1') (_, t2')-}
        {-| t1' < t2' = LT-}
        {-| t1' > t2' = GT-}
        {-| otherwise = EQ-}

listLengthFreqs :: [[a]] -> IO (HashTable Int Int)
listLengthFreqs []     = H.new
listLengthFreqs (x:xs) = do ht <- listLengthFreqs xs
                            v  <- H.lookup ht l
                            H.insert ht l $ if isNothing v then 1
                                                           else 1 + fromJust v
                            return ht
  where
    l = length x
