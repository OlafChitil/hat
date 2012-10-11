-- Reducing propositions to clausal form.
-- Colin Runciman, University of York.
-- Original version, 18/10/90.
-- This is Version 5 (see JFP Heap Profiling paper) translated for nhc.

module Main(main) where

main :: IO ()
main = do
         inp <- getContents
         putStr (concatMap clauses (lines inp))

data StackFrame = Ast Formula | Lex Char 
type Stack = [StackFrame]

data Formula =
  Tru |
  Sym Char |
  Not Formula |
  Dis Formula Formula |
  Con Formula Formula |
  Imp Formula Formula |
  Eqv Formula Formula 

type Clause = (String,String)

-- separate positive and negative literals, eliminating duplicates
clause :: Formula -> Clause
clause p = clause' p ([] , [])

clause' :: Formula -> Clause -> Clause
clause' (Dis p q)       x   = clause' p (clause' q x)
clause' (Sym s)       (c,a) = (insert s c , a)
clause' (Not (Sym s)) (c,a) = (c , insert s a)

-- the main pipeline from propositional formulae to printed clauses
clauses :: String -> String
clauses s = concat (map disp ( unicl ( split ( disin ( negin ( elim ( parse
              s )))))))

-- push disjunctions beneath conjunctions
disin :: Formula -> Formula
disin (Con p q) = Con (disin p) (disin q)
disin (Dis p q) = disin' (disin p) (disin q)
disin p = p

disin' :: Formula -> Formula -> Formula
disin' (Con p q) r = Con (disin' p r) (disin' q r)
disin' p (Con q r) = Con (disin' p q) (disin' p r)
disin' p q = Dis p q

-- format pair of lists of propositional symbols as clausal axiom
disp :: Clause -> String
disp (l,r) = interleave l spaces ++ "<=" ++ interleave spaces r ++ "\n"

-- eliminate connectives other than not, disjunction and conjunction
elim :: Formula -> Formula
elim (Not p) = Not (elim p)
elim (Dis p q) = Dis (elim p) (elim q)
elim (Con p q) = Con (elim p) (elim q)
elim (Imp p q) = Dis (Not (elim p)) (elim q)
elim (Eqv f f') = Con (elim (Imp f f')) (elim (Imp f' f))
elim p = p

-- insertion of an item (here a prop symbol) into an ordered list
insert :: Char -> String -> String
insert x []     = [x]
insert x yys@(y:ys) = if x < y then x:yys
                  else if x > y then y:insert x ys
                  else yys

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     _  = []

-- shift negation to innermost positions
negin :: Formula -> Formula
negin (Not (Not p)) = negin p
negin (Not (Con p q)) = Dis (negin (Not p)) (negin (Not q))
negin (Not (Dis p q)) = Con (negin (Not p)) (negin (Not q))
negin (Dis p q) = Dis (negin p) (negin q)
negin (Con p q) = Con (negin p) (negin q)
negin p = p

-- the priorities of symbols during parsing
opri :: Char -> Int
opri '(' = 0
opri '=' = 1
opri '>' = 2
opri '|' = 3
opri '&' = 4
opri '~' = 5

-- parsing a propositional formula
parse :: String -> Formula
-- parse t = f where [Ast f] = parse' t []
parse t = case parse' t [] of
          Ast f : [] -> f

parse' :: String -> Stack -> Stack
parse' [] s = redstar s
parse' (' ':t) s = parse' t s
parse' ('(':t) s = parse' t (Lex '(' : s)
parse' (')':t) s = parse' t (x:s')
                   where
                   (x : Lex '(' : s') = redstar s
parse' (c:t) s = if 'a'<= c && c <= 'z' then parse' t (Ast (Sym c) : s)
                 else if spri s > opri c then parse' (c:t) (red s)
                 else parse' t (Lex c : s)

-- reduction of the parse stack
red :: Stack -> Stack
red (Ast p : Lex '=' : Ast q : s) = Ast (Eqv q p) : s
red (Ast p : Lex '>' : Ast q : s) = Ast (Imp q p) : s
red (Ast p : Lex '|' : Ast q : s) = Ast (Dis q p) : s
red (Ast p : Lex '&' : Ast q : s) = Ast (Con q p) : s
red (Ast p : Lex '~' : s) = Ast (Not p) : s

-- iterative reduction of the parse stack
redstar :: Stack -> Stack
redstar = while ((/=) 0 . spri) red

spaces :: String
spaces = repeat ' '

-- split CNF formula into a list of conjuncts
split :: Formula -> [Formula]
split (Con p q) = split p ++ split q
split Tru = []
split p = [p]

-- priority of the parse stack
spri :: Stack -> Int
spri (Ast x : Lex c : s) = opri c
spri s = 0

-- does any symbol appear in both consequent and antecedant of clause
tautclause :: Clause -> Bool
tautclause (c,a) = intersect c a /= []

intersect :: Eq a => [a] -> [a] -> [a]
intersect = filter . (flip elem)

-- form unique clausal axioms excluding tautologies
unicl :: [Formula] -> [Clause]
unicl = filterset (not . tautclause) . map clause 

filterset :: Eq a => (a->Bool) -> [a] -> [a]
filterset = filterset' []

filterset' s p [] = []
filterset' s p (x:xs) = if not (x `elem` s) && p x then x:filterset' (x:s) p xs
                                               else filterset' s p xs

while :: (a->Bool) -> (a->a) -> a -> a
while p f x = if p x then while p f (f x) else x

