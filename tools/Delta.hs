{-# LANGUAGE EmptyDataDecls #-}

-- Low-level routines specifically for hat-delta / hat-detect.
module Delta
  ( findMain, findMainCall, edtNextChild
  , doObserve, doTrail, doDelta, doDetect, doAnim, doExplore, doView
  , identifyBug, identifyCycle
  , questionText, showHeuristic
  , ParentSet, newParentSet
  , anySuspect
  , DetectCommand(..), toCommand
  , DeltaOption(..), HeuristicMode(..), HeuristicBool(..)
  ) where

import Data.Char          (toLower,ord)
import System.Process     (system)
import System.Environment (getArgs,getProgName)
import System.Exit        (exitWith,ExitCode(..))
import Control.Monad      (when)

import LowLevel (FileNode(..),nil,unevaluated,SimpleNodeType(..)
                ,simpleNodeType,NodeType(..),nodeType,getSrcRef)
import Foreign.Ptr        (Ptr)
import System.IO.Unsafe   (unsafePerformIO)
import Numeric  (showHex)
import CommonUI (Options(..),hatAnim,hatDelta,hatDetect,hatExplore
                ,hatTrail,hatObserve,hatView)
import Explore  (Location(..),getLocation, getDefLocation,redexParent
                ,Coord(..))
import HighlightStyle (Highlight(..),Colour(..),highlight,getTerminalSize)
import PrettyLibHighlight
                (text, simple)
import SExp     (prettySExp)
import ADT      (ADT(..))
import NodeExp  (NodeExp(..),fullEval,flatEval,getNode,nodeExp2SExp
                ,removeResultCycles,nodeExpForNode,removeNonResultCycles
                ,fullEvalText,flatEvalText,children,finalResult,children)
import SrcRef	(readSrcRef)
import Slice    (Slice(..),sliceStartLine,sliceEndLine,sliceFile,offsetSlice
                ,highlightSlice,makeSlice,srcRef2Slice,RangeOrd(..))

data DeltaOption = Version
                 | ShowHelp
                 | QuickCheckMode
                 | SliceDepth Int
                 | DepthLimit Int
                 | DisableADTCompression
                 | NoSliceValue Float
                 | Heuristic HeuristicMode
                 | TreeType String
                   deriving Eq

data HeuristicMode = ValueHeuristic Float
                   | Correct
                   | Incorrect
                   | Add HeuristicMode HeuristicMode
                   | Negate HeuristicMode
                   | Multiply HeuristicMode HeuristicMode
                   | Invert HeuristicMode
                   | If HeuristicBool HeuristicMode HeuristicMode
                     deriving (Eq,Show,Read)
data HeuristicBool = TrueBool
                   | FalseBool
                   | Not HeuristicBool
                   | And HeuristicBool HeuristicBool
                   | Or HeuristicBool HeuristicBool
                   | Eq HeuristicMode HeuristicMode
                   | Gt HeuristicMode HeuristicMode
                   | Lt HeuristicMode HeuristicMode deriving (Eq,Show,Read)

data PS
type ParentSet = Ptr PS

data DetectCommand = Yes
                   | No
--                   | Maybe
--                   | MaybeYes
--                   | MaybeNo
                   | Quit
                   | Help
                   | Observe
                   | Trail
                   | Detect
                   | Delta
                   | Anim
                   | Explore
                   | View
                   | Split
                   | ShowADT
                   | ShowADTHs
                   | Children
                   | Set String String
                   | Get String
                   | Undo
                   | Unknown

toCommand :: String -> DetectCommand
toCommand cmd =
  case map toLower $ dropWhile (== ':') $ takeWhile (/=' ') cmd of
    "yes"      -> Yes
    "y"        -> Yes
    "true"     -> Yes
    "t"        -> Yes
    "no"       -> No
    "n"        -> No
    "false"    -> No
    "f"        -> No
    "quit"     -> Quit
    "q"        -> Quit
    "help"     -> Help
    "h"        -> Help
    "?"        -> Help
    "u"        -> Undo
    "undo"     -> Undo
    "observe"  -> Observe
    "trail"    -> Trail
    "detect"   -> Detect
    "delta"    -> Delta
    "anim"     -> Anim
    "animate"  -> Anim
    "explore"  -> Explore
    "source"   -> View
    "split"    -> Split
    "show"     -> ShowADT
    "adt"      -> ShowADT
    "adth"     -> ShowADTHs
    "children" -> Children
    "set"      -> Set var val
                  where
                    setText = dropWhile (==' ') $ dropWhile (/=' ') cmd
                    var = map toLower $ takeWhile (/=' ') setText
                    val = dropWhile (==' ') $ dropWhile (/=' ') setText
    "get"      -> Get var
                  where var = dropWhile (==' ') $ dropWhile (/=' ') cmd
    _          -> Unknown

doAnim :: NodeExp -> FilePath -> IO ()
doAnim exp f =
  do errCode <- system $ hatAnim f (getNode exp)
     checkOK errCode "hat-anim"

doDelta :: NodeExp -> FilePath -> IO ()
doDelta exp f =
  do errCode <- system $ hatDelta f (getNode exp)
     checkOK errCode "hat-delta"

doDetect :: NodeExp -> FilePath -> IO ()
doDetect exp f =
  do errCode <- system $ hatDetect f (getNode exp)
     checkOK errCode "hat-detect"

doExplore :: NodeExp -> FilePath -> IO ()
doExplore exp f =
  do errCode <- system $ hatExplore f (getNode exp)
     checkOK errCode "hat-explore"

doObserve :: NodeExp -> FilePath -> IO ()
doObserve exp f =
  do errCode <- system $ hatObserve f (flatEvalText 200 exp)
     checkOK errCode "hat-observe"

doTrail :: NodeExp -> FilePath -> IO ()
doTrail exp f = do errCode <- system $ hatTrail f (getNode exp)
                   checkOK errCode "hat-trail"

doView :: NodeExp -> IO ()
doView exp = do errCode <-   system
                           $ hatView
                           $ readSrcRef
                           $ getSrcRef
                           $ getNode exp
                checkOK errCode "hat-view"

identifyBug :: [DeltaOption] -> NodeExp -> String
identifyBug opts exp
  =    highlight [Background Red] "Bug identified:"
    ++ "\n  "
    ++ (showLocation opts exp)

identifyCycle :: [DeltaOption] -> [ADT] -> String
identifyCycle opts l
  =    highlight [Background Red] "Bug identified in cycling functions:"
    ++ "\n  "
    ++ concatMap (  (++ "\n---\n")
                  . (showLocation opts)
                  . (\(Branch _ n _ _) -> n))
                 l

showLocation :: [DeltaOption] -> NodeExp -> String
showLocation opts exp =
     show file ++ ":"
  ++ show functionStartOffset ++ "-"
  ++ show functionEndOffset ++ ":\n"
  ++ (if not(SliceDepth 0 `elem` opts) && maybeSlice /= Nothing then
        (highlightSlice offsetSubFunctionSlice $
                        getFileRange functionStartOffset functionEndOffset file)
      else getFileRange functionStartOffset functionEndOffset file)
--  ++ "\nSlice: " ++ show subFunctionSlice
--  ++ "\nNode: " ++ (show $ getNode exp)
  where
    (file,(Location {begin=Coord{row=functionStartOffset}
                    ,end=Coord{row=functionEndOffset}})) =
      getDefLocation (getNode exp)
    subFunctionSlice = (\(Just x) -> x) $ maybeSlice
    offsetSubFunctionSlice = offsetSlice (-functionStartOffset,0) 0
                                         subFunctionSlice
    maybeSlice = makeSlice 1 exp

getFileRange :: Int -> Int -> FilePath -> String
getFileRange s e =   unlines
                   . map ("  "++) 
                   . drop (s - 1) 
                   . take e
                   . lines
                   . unsafePerformIO
                   . readFile

showHeuristic :: (NodeExp -> Float)
              -> NodeExp
              -> String
              -> String
showHeuristic f e s =
  s ++ show (f e)

indent :: Int -> String -> String
indent n = tail . concatMap (("\n"++) . ((take n $ repeat ' ')++)) . lines

padTo :: Int -> String -> String
padTo n s = pad n (take n s)
            where
              pad n s = take (n - (length s)) (repeat ' ') ++ s

questionText :: Int ->
                (NodeExp -> String) ->
                (NodeExp -> String) ->
                NodeExp ->
                String
questionText w f g exp =
     mainQText w f g exp
  ++ withinText f exp
  where
    withinText :: (NodeExp -> String) ->
                  NodeExp ->
                  String
    withinText f exp =
      if    isQuestionable parent
         && pF == eF
         && (   (pSt <= eSt && pEnd > eEnd)
             || (pSt < eSt && pEnd >= eEnd))
        then let pExp = (  removeNonResultCycles
                         $ removeResultCycles
                         $ nodeExpForNode parent)
             in    "\nWithin:\n"
                ++ f pExp
                ++ withinText f pExp
        else ""
      where
        parent = redexParent $ getNode exp
        (pF,(Location {begin=Coord{row=pSt}
                      ,end=Coord{row=pEnd}})) = getDefLocation parent
        (eF,(Location {begin=Coord{row=eSt}
                      ,end=Coord{row=eEnd}})) = getDefLocation (getNode exp)
    subFunctionSlice = (\(Just x) -> x) $ makeSlice 1 exp

isQuestionable :: FileNode -> Bool
isQuestionable x = x > FileNode 5 && 
                   (t == ExpApp || t == ExpConstUse || t == ExpConstDef)
                   where t = nodeType x

mainQText :: Int ->
           (NodeExp -> String) ->
           (NodeExp -> String) ->
           NodeExp ->
           String
mainQText w f g exp =
  if    numLines lhs == 1
     && numLines rhs == 1
     && (width lhs + width rhs) < (w - 4)
    then lhs ++ " = " ++ rhs
    else lhs ++ "\n=\n" ++ rhs
  where
    lhs = f exp
    rhs = g exp
    numLines :: String -> Int
    numLines = length . lines
    width :: String -> Int
    width = maximum . (map length) . lines . stripHidden
    stripHidden :: String -> String
    stripHidden [] = []
    stripHidden (x:xs) = if ord x == 27 then stripToM xs
                                        else x:stripHidden xs
                         where
                           stripToM [] = []
                           stripToM ('m':xs) = stripHidden xs
                           stripToM (x:xs) = stripToM xs

removeEscaping :: String -> String
removeEscaping [] = []
removeEscaping ('\ESC':xs) = removeEscaping $ tail $ dropWhile (/= 'm') xs
removeEscaping (x:xs) = x:(removeEscaping xs)

checkOK errcode s = when (errcode/=ExitSuccess)
                         (putStrLn ("ERROR: Unable to start "++s++"."))

file :: NodeExp -> FilePath
file = takeWhile (/= '.') . fst . getDefLocation . getNode

foreign import ccall "parentset.h" newParentSet    :: FileNode -> IO ParentSet
foreign import ccall "parentset.h" freeParentSet   :: ParentSet -> IO ()
foreign import ccall "parentset.h" extendParentSet :: ParentSet -> FileNode -> IO ()

foreign import ccall "detectutils.h" findMainUse   :: Bool -> IO FileNode
foreign import ccall "detectutils.h" nextChild     :: ParentSet -> IO FileNode
foreign import ccall "detectutils.h" anySuspect    :: FileNode -> IO Bool

findMain :: IO FileNode
findMain = findMainUse False

findMainCall :: IO FileNode
findMainCall = findMainUse True

-- For debugging:
-- foreign import ccall showParentSet   :: ParentSet -> IO ()

edtNextChild :: ParentSet -> IO FileNode
edtNextChild ps = do
 -- candidate ps
    c <- candidate ps
    b <- anySuspect c
    if c==LowLevel.nil || b then return c else edtNextChild ps
  where
  candidate ps = do
    c <- nextChild ps
 -- putStrLn ("edtNextChild: "++showHex (int c) "")
    if c==LowLevel.unevaluated	-- actually EOF
      then return LowLevel.nil
      else if c==LowLevel.nil
      then candidate ps
      else case simpleNodeType c of
        NodeConditional -> do extendParentSet ps c
                              --showParentSet ps
                              candidate ps
        NodeIdentifier  -> candidate ps
        NodeBasicValue  -> candidate ps
        NodeCAF         -> return c
        NodeApplication -> case nodeType c of
                             ExpApp      -> return c
                             ExpValueApp -> candidate ps
        NodeSugar       -> return c
        NodeSpecial     -> case nodeType c of
                             ExpProjection -> return c
                             _             -> do extendParentSet ps c
                                                 candidate ps
        _               -> error "unexpected node in edtNextChild"
