import Data.List
 
shift n l = l ++ replicate n 0
 
pad n l = replicate n 0 ++ l
 
norm :: (Eq a, Num a) => [a] -> [a]
norm = dropWhile (== 0)
 
deg l = length (norm l) - 1
 
zipWith' op p q = zipWith op (pad (-d) p) (pad d q)
  where d = (length p) - (length q)
 
polydiv f g = aux (norm f) (norm g) []
  where aux f s q | ddif < 0 = (q, f)
                  | otherwise = aux f' s q'
           where ddif = (deg f) - (deg s)
                 k = (head f)
                 ks = map (* k) $ shift ddif s
                 q' = zipWith' (+) q $ shift ddif [k]
                 f' = norm $ tail $ zipWith' (-) f ks

polyadd f g | length f < length g = norm $ zipWith (+) (pad (length g - length f) f) g
            | length f > length g = norm $ zipWith (+) f (pad (length f - length  g) g)
            | otherwise           = norm $ zipWith (+) f g

polymul f g = if all (==0) f
                   then [0]
                   else polyadd monmul rest
                        where monmul = (map (*(head f)) g) ++ (replicate ((length f) - 1) 0)
                              rest = polymul (tail f) g
