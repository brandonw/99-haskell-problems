import Data.List (group)

compress :: (Eq a) => [a] -> [a]
compress xs = compress' xs []
  where
    compress' [] bs           = bs
    compress' (a : []) cs     = cs ++ [a]
    compress' (a : b : cs) ds = if a == b then compress' (a : cs) ds
                                          else compress' (b : cs) (ds ++ [a])

compress'' :: (Eq a) => [a] -> [a]
compress'' x = map head $ group x

compress''' :: (Eq a) => [a] -> [a]
compress''' = map head . group
