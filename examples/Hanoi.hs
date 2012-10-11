main = hanoi 3

hanoi :: Int -> IO ()
hanoi = h 'A' 'B' 'C'
        where
        h :: Char -> Char -> Char -> Int -> IO ()
        h a b c 0 = return ()
        h a b c n =
          do
            h a c b (n-1)
            putStr ("move disc from " ++ a : " to " ++ b : "\n")
	    h c b a (n-1)
