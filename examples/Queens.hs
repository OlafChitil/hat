
-- The queens problem made famous by Wirth.

type Board = [Int]

main :: IO ()
main =
  if null solutions then putStrLn "no solution!"
  else putStr (showBoard (head solutions))
  where
  solutions = queens 4

queens :: Int -> [Board]
queens n = valid n n 

valid :: Int -> Int -> [Board]
valid 0 n = [[]]
valid m n = filter safe (extend n (valid (m-1) n)) 

extend :: Int -> [Board] -> [Board]
extend n bs = consEach [1..n] bs 

consEach :: [a] -> [[a]] -> [[a]]
consEach [] y = []
consEach (a:x) y = map (a:) y ++ consEach x y 

safe :: Board -> Bool
safe (a:b) = no_threat a b 1

no_threat :: Int -> Board -> Int -> Bool
no_threat a [] m = True
no_threat a (b:y) m =
  a /= b && a+m /= b && a-m /= b && no_threat a y (m+1) 

showBoard :: Board -> String 
showBoard b =
  unlines (concat (zipWith rank [1..] b))
  where
  rank r qcol =
    map line ["o o o", " \\|/ ", " === "]
    where
    line crown_slice =
      concat (zipWith square [1..] b)
      where
      square scol _ =
        if scol == qcol then crown_slice
        else if scol `rem` (2::Int) == r `rem` (2::Int) then "....."
        else "     "
