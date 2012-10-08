module Explore  (Coord(..),noCoord,isNoCoord,isNextCoord,beforeCoord
                ,afterCoord,Row,Col
                ,Location(..),isVirtLocation,isNoLocation
                ,isImproperLocation,mkLocation,getLocation
                ,getDefLocation,getUseLocation,hasUseLocation
                ,redexParent) where
import SrcRef   (readSrcRef,defnSrcRef)
import LowLevel (FileNode(..),nil,getSrcRef,getDefnRef,getParentNode
                ,NodeType(..),nodeType)
import Ident    (getIdentAt)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)
import qualified SrcRef (SrcRef(..))

type Row = Int -- starts at 1, 0 for special purpose
type Col = Int -- starts at 1, 0 for special purpose

data Coord = Coord {row :: Row, col :: Col} deriving (Eq,Ord,Show)
  -- both always >= 0, start counting at 1 

noCoord :: Coord
noCoord = Coord 0 0

isNoCoord :: Coord -> Bool
isNoCoord c = row c == 0 || col c == 0

isNextCoord :: Coord -> Coord -> Bool
-- only for same row, because impossible to say if end of row reached
-- and hence wrap around to next row should be made
isNextCoord c1 c2 = row c1 == row c2 && col c1 + 1 == col c2

beforeCoord :: Coord -> Coord
-- not for noCoord
beforeCoord c = 
  if col c <= 1 then 
    if row c <= 1 then
      error "beforeCoord start"
    else
      Coord{row=row c-1,col=maxBound}
  else
    c{col=col c-1}

afterCoord :: Coord -> Coord
-- not for noCoord
afterCoord c =
  if col c == maxBound then
    Coord{row=row c+1,col=1}
  else
    c{col=col c+1}


-----------------------------------------------------------------------

data Location = Location {begin :: Coord, end :: Coord} deriving (Eq,Ord,Show)
  -- always begin <= end

isVirtLocation :: Location -> Bool
isVirtLocation l = isNoCoord (end l) && not (isNoCoord (begin l))

isNoLocation :: Location -> Bool
isNoLocation l = isNoCoord (begin l) && isNoCoord (end l)

isImproperLocation :: Location -> Bool
isImproperLocation = isNoCoord . begin

mkLocation :: Coord -> Coord -> Location
mkLocation b e = Location{begin=b,end=if e < b then noCoord else e} 

getLocation :: FileNode -> (FilePath,Location)
-- gets use location if it exists, 
-- otherwise use location of parent, otherwise def location
getLocation node =
  if hasUseLocation node then getUseLocation node 
    else let parent = redexParent node
         in if parent /= nil && hasUseLocation parent 
              then getUseLocation parent 
              else getDefLocation node

getDefLocation :: FileNode -> (FilePath,Location)
-- assumes given node is ValueUse, ConstUse or ValueApp (or App?)
getDefLocation node =
  (filename,Location{begin=Coord{row=rowBegin,col=colBegin}
                    ,end=Coord{row=rowEnd,col=colEnd}})
  where
  atomNode = getDefnRef node
  (SrcRef.SrcRef filename rowBegin colBegin rowEnd colEnd)
    | atomNode==nil = SrcRef.SrcRef "" 0 0 0 0
    | otherwise     = unsafePerformIO (liftM defnSrcRef (getIdentAt atomNode))

getUseLocation :: FileNode -> (FilePath,Location)
-- assumes given node has a location
getUseLocation node = 
  (filename,Location{begin=Coord{row=rowBegin,col=colBegin}
                    ,end=Coord{row=rowEnd,col=colEnd}})
  where
  srcRefNode = getSrcRef node
  (SrcRef.SrcRef filename rowBegin colBegin rowEnd colEnd)
    | srcRefNode==nil  = SrcRef.SrcRef "" 0 0 0 0
    | otherwise        = readSrcRef srcRefNode

hasUseLocation :: FileNode -> Bool
hasUseLocation node = fst (getUseLocation node) /= ""

redexParent :: FileNode -> FileNode
-- returned parent redex is application or constDef, 
-- never guard, case, if, update etc.
-- may however be nil
redexParent redex =
  let parent = getParentNode redex 
  in if parent == nil then
       parent
     else
       case nodeType parent of
         ExpGuard -> redexParent parent
         ExpCase -> redexParent parent
         ExpIf -> redexParent parent
         ExpFieldUpdate -> redexParent parent  -- occurs?
         ExpHidden -> redexParent parent
         _ -> parent
