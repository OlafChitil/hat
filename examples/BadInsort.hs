-- Simple faulty insertion sort

sort :: Ord a => [a] -> [a]
-- FAULT (1): missing equation for [] argument
sort (x:xs)	=	insert x (sort xs)

insert :: Ord a => a -> [a] -> [a]
insert x []	=	[x]
insert x (y:ys)	=	if x <= y
 	 	-- FAULT (2): y missing from result
 	 	then x : ys
 	 	-- FAULT (3): recursive call is same
 	 	else y : insert x (y:ys)

main	=	putStrLn (sort "program")

