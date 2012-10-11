------------------------------------------------------------------
-- Searching in a grid of words for hidden words oriented in any of
-- the 8 possible directions.
-- Colin Runciman, May 1984 (this version, for tracing, March 2000)
------------------------------------------------------------------

main = mapM (putStr.find) hidden
    where
    find word = word ++ " " ++ concat dirs ++ "\n"
        where
        dirs = map snd (
            filter (any (contains word) . fst)
                [(r,"right "), (d,"down "), (dl,"downleft "), (ul,"upleft ")]
            ++
            filter (any (contains drow) . fst) 
                [(r,"left "), (d,"up "), (dl,"upright "), (ul,"downright ")] )
        drow = reverse word
    r  = grid
    d  = transpose grid
    dl = diagonals grid
    ul = diagonals (reverse grid)

transpose [r] = map (:[]) r
transpose (r:rs) = zipWith (:) r (transpose rs)

diagonals [r] = map (:[]) r
diagonals (r:rs) = zipinit r ([]:diagonals rs)

zipinit [] ys = ys
zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys

contains xs ys = any (prefix xs) (suffixes ys)

suffixes [] = []
suffixes xs = xs : suffixes (tail xs) 

prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

grid =
   [['Y', 'I', 'O', 'M', 'R', 'E', 'S', 'K', 'S', 'T'],
    ['A', 'E', 'H', 'Y', 'G', 'E', 'H', 'E', 'D', 'W'],
    ['Z', 'F', 'I', 'A', 'C', 'N', 'I', 'T', 'I', 'A'],
    ['N', 'T', 'O', 'C', 'O', 'M', 'V', 'O', 'O', 'R'],
    ['E', 'R', 'D', 'L', 'O', 'C', 'E', 'N', 'S', 'M'],
    ['Z', 'O', 'U', 'R', 'P', 'S', 'R', 'N', 'D', 'A'],
    ['O', 'Y', 'A', 'S', 'M', 'O', 'Y', 'E', 'D', 'L'],
    ['R', 'N', 'D', 'E', 'N', 'L', 'O', 'A', 'I', 'T'],
    ['F', 'I', 'W', 'I', 'N', 'T', 'E', 'R', 'R', 'C'],
    ['F', 'E', 'Z', 'E', 'E', 'R', 'F', 'T', 'F', 'I'],
    ['I', 'I', 'D', 'T', 'P', 'H', 'U', 'B', 'R', 'L'],
    ['C', 'N', 'O', 'H', 'S', 'G', 'E', 'I', 'O', 'N'],
    ['E', 'G', 'M', 'O', 'P', 'S', 'T', 'A', 'S', 'O'],
    ['T', 'G', 'F', 'F', 'C', 'I', 'S', 'H', 'T', 'H'],
    ['O', 'T', 'B', 'C', 'S', 'S', 'N', 'O', 'W', 'I']]

hidden =
  ["COSY", "SOFT", "WINTER", "SHIVER", "FROZEN", "SNOW",
   "WARM", "HEAT", "COLD",   "FREEZE", "FROST",  "ICE" ]
