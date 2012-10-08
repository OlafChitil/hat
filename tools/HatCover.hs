-- HatCover: Colin Runciman, University of York, October 2004
-- Print Haskell sources highlighting the maximal expressions with
-- instances recorded in a given Hat trace.

module HatCover(hatCover, Interval, LC) where

import System.Environment (getProgName)
import LowLevel (openHatFile, NodeType(..), nodeSequence, FileNode(..), getSrcRef)
import Foreign.C.String (withCString)
import SrcRef (SrcRef(..), readSrcRef)

type SrcRef' = (String, Interval LC)
type Interval a = (a,a)
type LC = (Int,Int)

includes :: Ord a => Interval a -> Interval a -> Bool
includes (a,d) (b,c) = a <= b && c <= d

precedes :: Ord a => Interval a -> Interval a -> Bool
precedes (_,b) (c,_) = b < c

-- The record of which expressions have been covered is represented as a
-- tree of source intervals for each source-file.

type Cover = [(String, Tree (Interval LC))]
data Tree a = Leaf | Fork (Tree a) a (Tree a)

normalTree :: Tree a -> Bool
normalTree Leaf         = True
normalTree (Fork _ _ _) = True

fork :: Tree a -> a -> Tree a -> Tree a
fork back x forw | normalTree back && normalTree forw = Fork back x forw

normalList :: [a] -> Bool
normalList []    = True
normalList (_:_) = True

cons :: a -> [a] -> [a]
cons x xs | normalList xs = x : xs

add :: SrcRef' -> Cover -> Cover
add (f,i) [] = [(f, single i)]
add (f,i) ((g,t):etc) =
  if f==g then coverCons (g,insert i t) etc
  else cons (g,t) (add (f,i) etc)
  where
  coverCons st@(s,t) cvr | normalTree t = st : cvr

addAll :: [SrcRef'] -> Cover -> Cover
addAll []       cvr | normalList cvr = cvr
addAll (sr:srs) cvr | normalList cvr = addAll srs (add sr cvr)

single :: a -> Tree a
single x = Fork Leaf x Leaf

insert :: Ord a => Interval a -> Tree (Interval a) -> Tree (Interval a)
insert i Leaf = single i
insert i t@(Fork back j forw) =
  if      i `precedes` j then fork (insert i back) j forw
  else if j `precedes` i then fork back j (insert i forw)
  else if j `includes` i then t
  else {- i `includes` j -} fork (back `outside` i) i (forw `outside` i)
  
outside :: Ord a => Tree (Interval a) -> Interval a -> Tree (Interval a)
outside Leaf _ = Leaf
outside (Fork back ab forw) cd =
  if      ab `precedes` cd then fork back ab (forw `outside` cd)
  else if cd `precedes` ab then fork (back `outside` cd) ab forw
  else {- cd `includes` ab -} graft (back `outside` cd) (forw `outside` cd) 

graft :: Tree (Interval a) -> Tree (Interval a) -> Tree (Interval a)
graft Leaf t = t
graft (Fork back i forw) t = fork back i (graft forw t)

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Fork back i forw) = flatten back ++ [i] ++ flatten forw


hatCover :: FilePath -> [String] -> IO [(String, [Interval LC])]
hatCover hatfile moduleNames = do
    prog    <- getProgName
    withCString prog (withCString hatfile . openHatFile)
    nodes   <- nodeSequence
    let srs = [ convert sr |
                (fn,nt) <- nodes, isCover nt,
                let srn = getSrcRef fn, srn /= FileNode 0,
		let sr = readSrcRef srn, line sr /= 0,
		null moduleNames || filename sr `elem` moduleNames ]
    return [(a, flatten b) | (a,b) <- addAll srs []]


isCover :: NodeType -> Bool
isCover ExpApp         = True
isCover ExpValueApp    = True
isCover ExpChar        = True
isCover ExpInt         = True
isCover ExpInteger     = True
isCover ExpRat         = True
isCover ExpRational    = True
isCover ExpFloat       = True
isCover ExpDouble      = True
isCover ExpValueUse    = True
isCover ExpConstUse    = True
isCover ExpFieldUpdate = True
isCover ExpProjection  = True
isCover other          = False

convert :: SrcRef -> SrcRef'
convert (SrcRef fn ls cs le ce) = (fn, ((ls,cs),(le,ce)))


