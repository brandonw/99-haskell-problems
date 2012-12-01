reverse :: [a] -> [a]
reverse xs = reverse' xs []
  where
    reverse' [] ys     = ys
    reverse' (y:ys) zs = reverse' ys (y:zs)
