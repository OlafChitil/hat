hamming :: [Int] -> [Int]
hamming ps =
    tail (hamming_ps)
    where
    hamming_ps  = 1 : foldl1 merge (map hamProd ps)
    hamProd p   = map (p*) hamming_ps

merge (x:xs) (y:ys) =
    case compare x y of
    LT -> x : merge xs (y:ys)
    EQ -> x : merge xs ys          
    GT -> y : merge (x:xs) ys

main = print (take 10 (hamming [2, 3, 5]))
