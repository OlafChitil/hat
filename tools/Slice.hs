module Slice (Slice,union,makeSlice,sliceStartLine,sliceEndLine,sliceFile
             ,offsetSlice,highlightSlice,srcRef2Slice,RangeOrd(..)) where

import Data.Char           (isSpace)

import NodeExp        (NodeExp(..),children,getNode)
import LowLevel       (getSrcRef,nil)
import SrcRef         (SrcRef(..),readSrcRef)
import HighlightStyle (Highlight(..),Colour(..),highlight)
import Explore        (Location(..),Coord(..),getDefLocation)

import System.IO.Unsafe (unsafePerformIO)

newtype FilePos = FilePos (Int,Int) deriving Eq -- Line, Col
data PartSlice = Part {file :: FilePath
                      ,start :: FilePos
                      ,finish :: FilePos}
type Slice = [PartSlice]

instance Eq PartSlice where
  (==) (Part {file=f, start=s, finish=e}) (Part {file=f', start=s', finish=e'})
    = f == f' && s == s' && e == e'

instance Show PartSlice where
  show (Part {file=f, start=s, finish=e}) =
    "(" ++ show f ++ ":" ++ show s ++ "-" ++ show e ++ ")"

instance Show FilePos where
  show (FilePos (l,c)) = show l ++ ":" ++ show c

instance Ord FilePos where
  compare (FilePos (x, y)) (FilePos (x',y'))
    | x == x'   = compare y y'
    | otherwise = compare x x'

class Eq a => RangeOrd a where
  (<<+) :: a -> a -> Bool
  (+>>) :: a -> a -> Bool
  (+>>) x y = (<<+) y x
  (<+) :: a -> a -> Bool
  (+>) :: a -> a -> Bool
  (+>) x y = (<+) y x
  (<+>) :: a -> a -> Bool
  (>+<) :: a -> a -> Bool
  (>+<) x y = (<+>) y x

instance RangeOrd PartSlice where
  (<<+) (Part {file=f, start=s, finish=e}) (Part {file=f', start=s', finish=e'})
    | f == f'   = e <= s'
    | otherwise = f < f'
  (<+) (Part {file=f, start=s, finish=e}) (Part {file=f', start=s', finish=e'})
    = f == f' && s < s' && e > s' && e <= e'
  (<+>) (Part {file=f, start=s, finish=e}) (Part {file=f', start=s', finish=e'})
    = f == f' && s <= s' && e >= e'

union :: Slice -> Slice -> Slice
union x = compress . merge x

merge :: Slice -> Slice -> Slice
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <<+ y || x <+ y || x <+> y = x : merge xs (y:ys)
  | otherwise                    = y : merge (x:xs) ys

compress :: Slice -> Slice
compress [] = []
compress [x] = [x]
compress (x:x1:xs)
  | x <+ x1   = compress (join x x1 : xs)
  | x <+> x1  = compress (x : xs)
  | otherwise = x : compress (x1:xs)

join :: PartSlice -> PartSlice -> PartSlice
join (Part {file=f, start=s, finish=e}) (Part {file=f', start=s', finish=e'})
  = Part {file=f, start=s, finish=e'}

makeSlice :: Int -> NodeExp -> Maybe Slice
makeSlice depth exp =
  if depth >= 1 then
    let
      parts =   map (filter (not . isZeroLoc))
              $ map (srcRef2Slice . readSrcRef)
              $ filter (/= LowLevel.nil)
              $ map (getSrcRef . getNode)
              $ (nTimes depth (concatMap children)) [exp]
      nTimes 0 _ x = x
      nTimes n f x = nTimes (n-1) f (f x) -- (new ++ acc) where new = f x 
      isZeroLoc (Part {start=FilePos(0,0),finish=FilePos(0,0)}) = True
      isZeroLoc _ = False
    in
      if null parts
        then Nothing
        else Just $ foldr union (head parts) (tail parts)
  else
    let
      (f,Location{begin=Coord{row=rB,col=cB}
                 ,end=Coord{row=rE,col=cE}}) = getDefLocation $ getNode exp
    in
      Just [Part {file=f
                 ,start=FilePos(rB,cB)
                 ,finish=FilePos(rE,cE)}]

sliceStartLine :: Slice -> Int
sliceStartLine = (\(Part {start=(FilePos (s,_))}) -> s) . head

sliceEndLine :: Slice -> Int
sliceEndLine = (\(Part {finish=(FilePos (e,_))}) -> e) . last

sliceFile :: Slice -> FilePath
sliceFile = (\(Part {file=f}) -> f) . head

offsetSlice :: (Int,Int) -> Int -> Slice -> Slice
offsetSlice (l,c) so=
  map doOffset
  where
    doOffset (Part {file=f
                   ,start=(FilePos (sl,sc))
                   ,finish=(FilePos(el,ec))}) =
      Part {file=f
           ,start=FilePos(sl+l,if (sl+l) == 0 then sc + c + so else sc + c)
           ,finish=FilePos(el+l,if (el+l) == 0 then ec + c + so else ec + c)}

highlightSlice :: Slice -> String -> String
highlightSlice [] x = x
highlightSlice _ [] = []
highlightSlice ((Part {start=(FilePos (sl,sc)),finish=(FilePos (el,ec))}):others) x
  =    ls
    ++ cs
    ++ highlightedLines
    ++ rest
    where
      (ls,r) = grabLines sl x
      (cs,r1) = grabChars sc r
      (rls,r2) = grabLines (el-sl) r1
      (rcs,r3) = grabChars (if el-sl == 0 then ec-sc else ec) r2
      rest = highlightSlice (offsetSlice (-el,0) (-(ec + 2)) others) r3
      
      highlightedLines =    (unlines $ map highlightLine $ lines rls)
                         ++ highlightLine rcs
      highlightLine l =    takeWhile isSpace l
                        ++ (highlight [Background Red]
                                      $ dropWhile isSpace l)
      
      grabLines :: Int -> String -> (String,String)
      grabLines n = (\(x,y) -> (unlines x,unlines y)) . splitAt n . lines
      grabChars :: Int -> String -> (String,String)
      grabChars n = splitAt (n + 1)

srcRef2Slice :: SrcRef -> Slice
srcRef2Slice (SrcRef {filename=f
                     ,line=ls
                     ,column=cs
                     ,lineend=le
                     ,columnend=ce}) =
  [Part {file=f,start=(FilePos(ls,cs)),finish=(FilePos(le,ce))}]