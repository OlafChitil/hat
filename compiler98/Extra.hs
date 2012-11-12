module Extra(module Extra, module HbcOnly, module Data.Maybe, trace) where

import HbcOnly
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import System.IO (hPutStr,stderr)
import System.Exit (exitFailure)
import SysDeps (trace)

mapListSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapListSnd f = map (mapSnd f)

foldls f z [] = z
foldls f z (x:xs) =
  let z' = f z x
  in seq z' (foldl f z' xs)

strace msg c = if length msg == 0
	       then  c
	       else trace msg c

warning s v = trace ("Warning: "++s) v
--warning s v = v

fstOf a b = a
sndOf a b = b

snub [] = []
snub (x:xs) = x:snub (filter (/=x) xs)

pair   x y   = (x,y)
triple x y z = (x,y,z)

-- #if !defined(__HASKELL98__)
-- isNothing Nothing = True
-- isNothing _       = False
-- #endif

dropJust (Just v) = v

isLeft (Left a) = True
isLeft _        = False

isRight (Right a) = True
isRight _        = False

dropLeft (Left a) = a

dropRight (Right a) = a

dropEither (Left x) = x
dropEither (Right x) = x

mapPair f g (x,y) = (f x,g y)
mapFst  f   (x,y) = (f x,  y)
mapSnd    g (x,y) = (  x,g y)

findLeft l = 
        f [] l
    where
        f a [] = Right (reverse a)
        f a (Left  e:r) = Left e
        f a (Right x:r) = f (x:a) r

eitherMap f [] = Right []
eitherMap f (x:xs) =
        case f x of
          Left err -> Left err
          Right x' -> case eitherMap f xs of
                        Left err -> Left err
                        Right xs' -> Right (x':xs') 


jRight :: Int -> [Char] -> [Char]
jRight n s = case length s of
                ns -> if ns > n then s
                      else space (n-ns) ++ s

jLeft :: Int -> [Char] -> [Char]
jLeft n s = case length s of
                ns -> if ns > n then s
                      else s ++ space (n-ns)

partitions f [] = []
partitions f (x:xs) =
    gB f (f x) [x] xs
  where
    gB f v a [] = [reverse a]
    gB f v a (x:xs) = if f x == v
                      then gB f v (x:a) xs
                      else reverse a : gB f (f x) [x] xs

----------

mix s [] = ""
mix s xs =  foldl1 (\x y-> x ++ s ++ y) xs

mixSpace = mix " "
mixComma = mix ","
mixLine  = mix "\n"

mixCommaAnd [x] = x
mixCommaAnd [x,y] = x ++ " and " ++ y
mixCommaAnd (x:xs) = x ++ ", " ++ mixCommaAnd xs

rep 0 c = []
rep n c = c:rep (n-1) c

-----------------

assoc :: Eq a => a -> [(a,b)] -> b
assoc a [] = error "assoc!"
assoc a ((k,v):kvs) = if a == k then v
                       else assoc a kvs

assocDef :: Eq a => [(a,b)] -> b -> a -> b
assocDef []          d a = d
assocDef ((k,v):kvs) d a = if a == k then v
                           else assocDef kvs d a

-------------------

-- abstract type for storing the position of a syntactic construct in a file,
-- that is, line and column number of both start and end positions.

data Pos = P !Int !Int
-- line * 10000 + column of start, line * 10000 + column of end
-- both lines and column start at 1
-- allow lines and coluns 0 to mark nonexisting position

type Line = Int
type Column = Int

-- used in STGcode to get encoded start position
-- STGcode should be changed so that this function can disappear
pos2Int :: Pos -> Int 
pos2Int (P s _) = s

toPos :: Line -> Column -> Line -> Column -> Pos
toPos l1 c1 l2 c2 =  P (l1*10000 + c1) (l2*10000 + c2) 

-- create a virtual position out of a real one
insertPos :: Pos -> Pos
insertPos (P s e) = P s 0

noPos :: Pos
noPos = P 0 0

mergePos :: Pos -> Pos -> Pos
-- combines positions by determining minimal one that covers both
-- positions may or may not overlap
-- does not assume that first pos really earlier 
-- nonexisting positions are ignored
mergePos (P s1 e1) (P s2 e2) =
  if e1 == 0 then P s2 e2
  else if e2 == 0 then P s1 e1
  else P (min s1 s2) (max e1 e2)

mergePoss :: [Pos] -> Pos
-- merge a list of positions
mergePoss = foldr mergePos noPos

fromPos :: Pos -> (Line,Column,Line,Column)
fromPos (P s e) =
 let l1 = s `div` 10000
     c1 = s - l1*10000
     l2 = e `div` 10000
     c2 = e - l2*10000
 in (l1,c1,l2,c2)

strPos :: Pos -> String
strPos p = 
  case fromPos p of
    (0,0,0,0) -> "nopos"
    (l1,c1,0,0)   -> show l1 ++ ':' : show c1
    (l1,c1,l2,c2) | l1==l2 && c1==c2
                  -> show l1 ++ ':' : show c1
    (l1,c1,l2,c2) -> show l1 ++ ':' : show c1 ++ '-' : show l2 ++ ':' : show c2

instance Show Pos where
  show p = strPos p

instance Eq Pos where
  P s1 e1 == P s2 e2 = (s1 == s2) && (e1 == e2)

instance Ord Pos where  
  -- for ordering error messages of parser
  -- and determining minimum of two positions
  -- nonexisting positions are avoided
  P s1 e1 > P s2 e2 = 
    s1 > s2 || (s1 == s2 && e1 > e2)
  min (P s1 e1) (P s2 e2) =
    if e1 == 0 
      then if e2 == 0 
             then if s1 <= s2 then P s1 e1 else P s2 e2
             else P s2 e2
      else if e2 == 0
             then P s1 e1
             else if (s1 < s2) || (s1 == s2 && e1 <= e2)
                    then P s1 e1
                    else P s2 e2 

--------------------


data SplitIntegral = SplitPos [Int]
                   | SplitZero
                   | SplitNeg [Int]

-- splitIntegral :: (Integral a) => a -> SplitIntegral
splitIntegral n =
  if n < 0
  then SplitNeg (split' (-n))
  else if n == 0 then SplitZero
  else SplitPos (split' n)
 where
  split' n = if n == 0 then []
             else fromInteger (toInteger (n `mod` 256)) : split' (n `div` 256)

--------------------
type Set a = [a]

emptySet = []

singletonSet a = [a]

listSet xs = (nub xs)

unionSet xs ys = unionSet' xs ys
               where unionSet' [] ys = ys
                     unionSet' (x:xs) ys | x `elem` ys = unionSet' xs ys
                                         | otherwise   = x:unionSet' xs ys

removeSet xs ys = filter (`notElem` ys) xs
---------------------
strChr' :: Char -> Char -> String 
strChr' del '\\' = "\\\\"
strChr' del '\n' = "\\n"
strChr' del '\t' = "\\t"
strChr' del c = if isPrint c 
                 then if c == del 
                      then "\\" ++ [c]
                      else [c]
                 else "\\o" ++ map (toEnum . (+(fromEnum '0')))
                                  (ctoo (fromEnum c))
                    where ctoo c = [(c `div` 64),(c `div` 8) `mod` 8,c `mod` 8]
                          
strChr :: Char -> String 
strChr c = "'" ++ strChr' '\'' c ++ "'"

strStr :: String -> String 
strStr s = "\"" ++ concatMap (strChr' '"') s ++ "\""

-----------------------
showErr :: (Pos,String,[String]) -> String
showErr (pos,token,strs) =
    strPos pos ++ (" Found " ++ token ++
    case nub strs of
           [] -> " but no token can be accepted here."
           [x] -> " but expected a " ++ x
	   xs  -> " but expected one of " ++ mix " " xs)

------------------------
isNhcOp :: Char -> Bool
isNhcOp '~' = True; isNhcOp '=' = True; isNhcOp '*' = True
isNhcOp '%' = True; isNhcOp '/' = True; isNhcOp ':' = True
isNhcOp '+' = True; isNhcOp '@' = True; isNhcOp '.' = True
isNhcOp '>' = True; isNhcOp '&' = True; isNhcOp '$' = True
isNhcOp '|' = True; isNhcOp '-' = True
isNhcOp '!' = True; isNhcOp '<' = True
isNhcOp '^' = True; isNhcOp '#' = True; isNhcOp '?' = True
isNhcOp '\\' = True
isNhcOp _ = False

------------------------
-- Given a list of filenames, return filename and its content of first file
-- that was read successfully (intention: other filenames may not exist)

readFirst :: [String] -> IO (String,String)

readFirst []     = do
  hPutStr stderr "Fail no filenames, probably no -I or -P" 
  exitFailure
readFirst [x]    = do 
  finput <- readFile x
  return (x,finput)
readFirst (x:xs) =
  catch (do finput <- readFile x
            return (x,finput))
        (\ y -> (y :: IOException) `seq` readFirst xs)

------------------------
