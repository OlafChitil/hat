-- module HatExplore where

import LowLevel (FileNode(..),openHatFile,nil,unevaluated,entered,interrupted
                ,NodeType(..),nodeType
                ,peekResult,peekSubExprs,peekExpArg,getParentNode
                ,hiddenChildren,getSrcRef,getDefnRef,getErrorLoc
                ,hatVersionNumber)
import qualified SrcRef (SrcRef(..))
import SrcRef (readSrcRef,defnSrcRef)
import Ident (Ident,getIdentAt)
import SExp (prettyEquation2)
import CommonUI (Options,initialOptions)
import Detect (findMain)
import CommonUI (safeReadInt,hatObserve,hatTrail,hatDetect,hatAnim)
import HighlightStyle
import System.IO (hSetBuffering,BufferMode(..),stdin,stdout,stderr,hPutStrLn)
import Foreign.C.String (withCString)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (system)
import System.Environment (getArgs,getProgName,getEnv)
import System.Exit (exitWith,ExitCode(..))
import Data.List (isSuffixOf,sortBy,partition,union)
import Control.Monad (liftM)
import Data.Char (isAlpha)
import Explore (Coord(..),noCoord,isNoCoord,isNextCoord,beforeCoord
               ,afterCoord,Row,Col
               ,Location(..),isVirtLocation,isNoLocation
               ,isImproperLocation,mkLocation,getLocation,hasUseLocation
               ,getDefLocation,getUseLocation
               ,redexParent)


main = do
  hatFile <- initHatFile
  startNode <- determineStartNode
  startInteraction startNode 
   

printNodes :: String -> [FileNode] -> IO ()
-- for testing only
printNodes str nodes =
  putStrLn (str ++ ':' : foldr myShow "" (map int nodes))
  where
  myShow i xs = ',' : showHex i xs 

showNode :: FileNode -> String
showNode = flip showHex "" . int  
  
initHatFile :: IO FilePath
initHatFile = do
  prog    <- System.Environment.getProgName
  hatFile <- determineHatFile
  withCString prog (\p-> withCString hatFile (openHatFile p))
  return hatFile

determineHatFile :: IO FilePath
determineHatFile = do
  args    <- System.Environment.getArgs
  case args of (f:_) -> return (rectify f)
               _     -> do hPutStrLn stderr
                             ("error: no trace file")
                           exitWith (ExitFailure 1)
  where
  rectify :: FilePath -> FilePath
  rectify f | ".hat" `isSuffixOf` f = f
            | otherwise = f ++ ".hat"

dropHS :: FilePath -> FilePath
dropHS f = case reverse f of
  's':'h':'.':name -> reverse name
  's':'h':'l':'.':name -> reverse name

getErrorRedex :: IO FileNode
-- determine the redex that caused a runtime error
-- if no runtime error then return nil
getErrorRedex = do
  errRedex <- getErrorLoc -- careful, might be nil, Hidden or trusted
  return (firstWithSrcRef errRedex)

firstWithSrcRef :: FileNode -> FileNode
-- return redex or first ancestor in traced code that has source reference
-- redex may be nil, Hidden, or in trusted code (then has hidden parent)
firstWithSrcRef redex =
  if redex /= nil && (getSrcRef redex == nil || isHidden (redexParent redex))
    then firstWithSrcRef (redexParent redex)
    else redex

determineStartNode :: IO FileNode
-- either Main.main or passed as first argument
determineStartNode = do
  maybeNode <- getArgumentNode 1
  case maybeNode of
    Just node -> return node
    Nothing -> do
      errRedex <- getErrorRedex
      if errRedex /= nil then
        return errRedex
       else do
        mainNode <- findMain
        if mainNode == LowLevel.nil then do
          putStrLn "Could not find \"Main.main\"!"
          exitWith (ExitFailure 1)
         else
          return mainNode
  
getArgumentNode :: Int -> IO (Maybe FileNode)
-- get node from nth program argument, if it exists
getArgumentNode argNo = do
  arguments <- System.Environment.getArgs
  return $ if length arguments <= argNo then
             Nothing
           else case safeReadInt 0 (arguments!!argNo) of
             0 -> Nothing
             i -> let node = (FileNode i)  
                  in if expIsRedex node then Just node else 
                    let parent = redexParent node
                    in if parent == nil then Nothing else Just parent
                  -- if given node is not a redex try its parent

--------------------------------------------------------------------

traverseReduct :: FileNode -> (Slice,[FileNode],[FileNode])
-- given node of redex determine
-- slice that covers part of reduct that *may* have been demanded (approxim.)
-- edt-children (demanded redexes (applications))
-- cafs (shared children)
traverseReduct redex = 
  if reduct `elem` [LowLevel.nil,unevaluated,entered,interrupted] then 
    (emptySlice,[],[]) 
  else go reduct emptySlice
  where
  (parent,reduct) = case nodeType redex of
    ExpConstUse -> 
      let constDef = peekExpArg redex 0
          constRes = peekResult constDef
      in if constRes `notElem` [nil,unevaluated,entered,interrupted]
           then case nodeType constRes of
                  ExpProjection ->
                    let projParent = getParentNode constRes 
                    in if projParent /= constDef 
                         then (getParentNode projParent,projParent)
                         -- pattern binding
                         else (constDef,constRes)
                  _ -> (constDef,constRes)
           else (constDef,constRes)
    _ -> (redex,peekResult redex)
  gos :: [FileNode] -> Slice -> (Slice,[FileNode],[FileNode])
  gos [] slice = (slice,[],[])
  gos (exp:exps) slice =
    let (slice1,redexes1,consts1) = gos exps slice
        (slice2,redexes2,consts2) = go exp slice1
    in (slice2,redexes2++redexes1,consts2++consts1)
  go :: FileNode -> Slice -> (Slice,[FileNode],[FileNode])
  go exp slice = if getParentNode exp /= parent then (slice,[],[]) 
    else case nodeType exp of
      ExpApp -> 
        if isRedex exp then
          let (newSlice,redexes,consts) = 
                gos (peekSubExprs exp) (addToSlice loc slice)
          in (newSlice,exp:redexes,consts)
        else (subtractFromSlice loc slice,[],[])
      ExpValueApp -> 
        gos (tail (peekSubExprs exp)) (addToSlice loc slice)
        -- first arg is atom for which there is no location
      ExpValueUse -> (addToSlice loc slice,[],[])
      ExpConstUse -> 
        if isRedex (peekExpArg exp 0) then  -- check constDef
          (addToSlice loc slice,[],[exp])
        else
          (subtractFromSlice loc slice,[],[])
      ExpConstDef -> error "traverseReduct: ExpConstDef"
      ExpGuard -> 
        let (slice1,redexes1,consts1) = traverseReduct exp
            (slice2,redexes2,consts2) = go (peekExpArg exp 0) slice1
        in (slice2,redexes1++redexes2,consts1++consts2)
        -- sadly trace contains no useful location for guard
        -- existing location is that of boolean expression
      ExpCase -> 
        let (slice1,redexes1,consts1) = traverseReduct exp
            slice2 = addToSlice (Location{begin=begin loc
                                         ,end=Coord{row=row (begin loc)
                                              ,col = col (begin loc)+3}})
                       (mergeSlices slice1 (subtractFromSlice loc slice))
            (slice3,redexes2,consts2) = go (peekExpArg exp 0) slice2
        in (slice3,redexes1++redexes2,consts1++consts2)
        -- subtract whole subexpression from slice before adding "case"
        -- and demanded parts
      ExpIf -> 
        let (slice1,redexes1,consts1) = traverseReduct exp
            slice2 = addToSlice (Location{begin=begin loc
                                         ,end=Coord{row=row (begin loc)
                                              ,col = col (begin loc)+1}})
                       (mergeSlices slice1 (subtractFromSlice loc slice))
            (slice3,redexes2,consts2) = go (peekExpArg exp 0) slice2
        in (slice3,redexes1++redexes2,consts1++consts2)
        -- subtract whole subexpression from slice before adding "if"
        -- and demanded parts
      ExpFieldUpdate -> 
        if isRedex exp then
          let (slice,redexes,consts) = gos (peekSubExprs exp) 
                (addToSlice loc slice)
          in (slice,exp:redexes,consts)
        else
          (subtractFromSlice loc slice,[],[])
      ExpProjection -> (addToSlice loc slice,[],[]) 
        -- argument must have different parent
      ExpHidden -> (emptySlice,hiddenChildren exp,[])
        -- (slice,[],[])
      ExpForward -> go (peekResult exp) slice -- just walk through
      ExpChar -> (addToSlice loc slice,[],[])
      ExpInt -> (addToSlice loc slice,[],[])
      ExpInteger -> (addToSlice loc slice,[],[])
      ExpRat -> (addToSlice loc slice,[],[])
      ExpRational -> (addToSlice loc slice,[],[])
      ExpFloat -> (addToSlice loc slice,[],[])
      ExpDouble -> (addToSlice loc slice,[],[])
      _ -> error "getReductSubExpNodes: unexpected node type"
    where
    (_,loc) = getUseLocation exp


isRedex :: FileNode -> Bool
-- assume there is a non-nil result 
isRedex exp = peekResult exp `notElem` [unevaluated,exp]
  -- partial application can have a result pointing to itself

--------------------------------------------------------------------

redexAllChildren :: FileNode -> [FileNode]
-- given a redex, i.e. application or constUse
-- yield all children, including trusted ones
-- children are applications or constUses
redexAllChildren redex = appRedexes ++ constRedexes
  where
  (_,appRedexes,constRedexes) = traverseReduct redex

expSort :: [FileNode] -> [FileNode]
expSort = sortBy cmp
  where
  cmp red1 red2 = compare loc1 loc2
    where
    (_,loc1) = getUseLocation red1
    (_,loc2) = getUseLocation red2


redexChildren :: FileNode -> [FileNode]
-- excludes trusted children (currently even those with children, TO DO)
-- currently this function is not used at all
redexChildren = filter suspectedRedex . redexAllChildren


suspectedRedex :: FileNode -> Bool
-- given application or constUse
suspectedRedex redex = 
  result `elem` [entered,interrupted] || 
    not (isHidden result && null (hiddenChildren result)) 
  where
  result = redexResult redex

redexResult :: FileNode -> FileNode
-- given application or constUse return the reduct (or entered, etc)
-- this is usually not the final result
redexResult redex = case nodeType redex of
  ExpApp -> peekResult redex
  ExpConstUse -> peekResult (peekExpArg redex 0)
  ExpConstDef -> peekResult redex
  -- the following are just for reliablity
  ExpGuard -> peekResult redex
  ExpCase -> peekResult redex
  ExpIf -> peekResult redex
  ExpFieldUpdate -> peekResult redex
  _ -> error ("redexResult: unexpected redex " ++ showNode redex)

redexParents :: FileNode -> [FileNode]
-- list of ancestors, first is the parent
redexParents redex =
  let parent = redexParent redex
  in if parent == nil then [] else parent : redexParents parent

expIsRedex :: FileNode -> Bool
expIsRedex node = case nodeType node of
  ExpApp -> isRedex node
  ExpConstUse -> isRedex (peekExpArg node 0)
  ExpConstDef -> isRedex node
  _ -> False

isHidden :: FileNode -> Bool
isHidden node = case nodeType node of
  ExpHidden -> True
  _         -> False

--------------------------------------------------------------------

updRedexFaultyDescendants :: State -> State
-- all descendants including redex itself that may be faulty
-- i.e. descendant tree without subtrees started by a correct redex
updRedexFaultyDescendants state =
  state{faultSetRoot = root
       ,faultSet = if root == nil then emptySet else go root id emptySet}
  where
  -- root is first wrong redex when going up parents
  root = case filter (`elemSet` (wrong state)) 
                (cur (redexes state):parents state) of
           (wrong:_) -> wrong
           [] -> nil
  -- continuation passing style, threading already visited nodes
  -- to avoid looping for mutually recursive constants
  go :: FileNode -> (Set FileNode -> Set FileNode) 
     -> Set FileNode -> Set FileNode
  go redex cont collected =
    if redexCorrect redex state || redex `elemSet` collected
      then cont collected
      else (foldr ($) cont (map go (redexAllChildren redex))) 
             (insertSet redex collected)


--------------------------------------------------------------------

redexDefUsedSlice :: State -> FileNode -> Slice
redexDefUsedSlice state redex =
  let (file,loc) = getDefLocation redex
  in if file /= filename (cur (modules state))
       then emptySlice
       else used
  where
  (used,_,_) = traverseReduct redex

redexDefSlice :: State -> FileNode -> Slice
redexDefSlice state redex =
  let (file,loc) = getDefLocation redex
  in if file == filename (cur (modules state))
       then mkSlice loc
       else emptySlice

redexUseLocation :: FileNode -> Location
-- assumes given node is redex, i.e. application or constUse
-- location covers full redex, including all subexpressions
redexUseLocation redex =
  if redex `elem` [nil,unevaluated,entered,interrupted] then
    error "redexUseLocation: invalid redex"
  else loc
  where
  (_,loc) = getUseLocation redex

redexUseSlice :: FileNode -> Slice
-- assumes given node is redex, i.e. application or constUse
-- slice includes the application and all subconstructs 
-- that are values or may be values
redexUseSlice redex = 
  if redex `elem` [nil,unevaluated,entered,interrupted] then
    error "redexUseSlice: invalid redex"
  else gos (peekSubExprs redex) (mkSlice loc)
  where
  parent = getParentNode redex
  (_,loc) = getUseLocation redex

  gos :: [FileNode] -> Slice -> Slice
  gos exps slice = foldr go slice exps
  go :: FileNode -> Slice -> Slice
  go exp slice = 
    if getParentNode exp /= parent then 
      slice
    else case nodeType exp of
      ExpApp -> subtractFromSlice loc slice
      ExpValueApp -> gos (tail (peekSubExprs exp)) (addToSlice loc slice)
      ExpValueUse -> addToSlice loc slice
      ExpConstUse -> subtractFromSlice loc slice
      ExpConstDef -> slice -- only if original redex was constUse
      ExpGuard -> subtractFromSlice loc slice
      ExpCase -> subtractFromSlice loc slice
      ExpIf -> subtractFromSlice loc slice
      ExpFieldUpdate -> subtractFromSlice loc slice
      ExpChar -> addToSlice loc slice
      ExpInt -> addToSlice loc slice
      ExpInteger -> addToSlice loc slice
      ExpRat -> addToSlice loc slice
      ExpRational -> addToSlice loc slice
      ExpFloat -> addToSlice loc slice
      ExpDouble -> addToSlice loc slice
      _ -> error "redexUseSlice: unexpected node type"
    where
    (_,loc) = getUseLocation exp

--------------------------------------------------------------------

borderSlice :: Text -> Slice
-- construct slice of all spaces at beginning and ends of lines
borderSlice text = Slice (go 1 text)
  where
  go :: Int -> Text -> [Location]
  go _ [] = []
  go curRow (line:rest) = 
    (if leftSpace > 0 then 
       (Location{begin=Coord{row=curRow,col=1}
                ,end=Coord{row=curRow,col=leftSpace}}:)
     else id)
    ((Location{begin=Coord{row=curRow,col=lastCol-rightSpace+1}
              ,end=Coord{row=curRow,col=maxBound}}:)
    (go (curRow+1) rest))
    where
    lastCol = length revLine
    leftSpace = length (takeWhile (==' ') line)
    rightSpace = length (takeWhile (==' ') revLine)
    revLine = case reverse line of
                '\r':rest -> rest
                all -> all

--------------------------------------------------------------------

startInteraction :: FileNode -> IO ()
startInteraction startRedex = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  (width,height) <- getTerminalSize
  putStr cls
  let allParents = redexParents startRedex
  current <- loadModule (fst (getLocation startRedex))
  curModules <- insertModules allParents (mkCursorSeq [] current [])
  checkModules (newRedex (State
    { modules = curModules
    , window = Coord{row=height,col=width}
    , firstRow = 1
    , parents = allParents
    , redexes = undefined
    , correct = emptySet
    , wrong = emptySet
    , neither = emptySet
    , faultSetRoot = nil
    , faultSet = emptySet
    , showFaultSet = False
    , showPart = False
    , message = "Press h for help."
    })
    startRedex)


fileTags :: State -> [Tag]
fileTags state = 
  let redexSlice = mkSlice (redexUseLocation (cur (redexes state)))
      siblings = cursorSeqToList (redexes state)
      (suspectedSiblings,trustedSiblings) = partition suspectedRedex siblings
      (correctSus,otherSus) = 
        partition (`elemSet` (correct state)) suspectedSiblings
      (wrongFaultySus,neitherSus) =
        partition (`elemSet` (wrong state)) otherSus
      (neitherTrust,otherTrust) =
        partition (`elemSet` (neither state)) trustedSiblings
      (wrongFaultyTrust,correctTrust) =
        partition (`elemSet` (wrong state)) trustedSiblings
      correctSiblings = correctTrust ++ correctSus
      neitherSiblings = neitherTrust ++ neitherSus
      (faultySiblings,wrongSiblings) = 
        partition (`redexFaulty` state) (wrongFaultyTrust ++ wrongFaultySus)
      correctSiblingsSlice = mergeSlicesMap redexUseSlice correctSiblings
      neitherSiblingsSlice = mergeSlicesMap redexUseSlice neitherSiblings
      wrongSiblingsSlice = mergeSlicesMap redexUseSlice wrongSiblings
      faultySiblingsSlice = mergeSlicesMap redexUseSlice faultySiblings
      makeSlice = if showPart state then redexDefUsedSlice else redexDefSlice
  in if filename (cur (modules state)) == 
        fst (getUseLocation (cur (redexes state)))
       then combineSlices 
               [(redexSlice,currentM)
               ,(correctSiblingsSlice,correctM)
               ,(wrongSiblingsSlice,wrongM)
               ,(neitherSiblingsSlice,markedM)
               ,(faultySiblingsSlice,faultyM)
               ,(border (cur (modules state)),emptyM)
               ,(if showFaultSet state
                   then manySlices (makeSlice state) (faultSet state)
                   else makeSlice state (cur (redexes state))
                ,definitionM)]
       else case getLocation (cur (redexes state)) of
              (file,loc) ->             
                combineSlices
                  ((if file == filename (cur (modules state))
                     then ((mkSlice loc,currentM):)
                     else id)
                  [(border (cur (modules state)),emptyM)
                  ,(if showFaultSet state
                      then manySlices (makeSlice state) (faultSet state)
                      else makeSlice state (cur (redexes state))
                   ,definitionM)])
    

redexMark :: FileNode -> State -> Mark
redexMark red state =
  (if showFaultSet state  && red == faultSetRoot state
     then (combMarks definitionM) else id) $
  (if red == cur (redexes state) then (combMarks currentM) else id) $
  (if red `elemSet` correct state 
     then correctM
   else if red `elemSet` wrong state 
     then if redexFaulty red state then faultyM else wrongM
   else if red `elemSet` neither state || suspectedRedex red
     then markedM
   else correctM)     

equationText :: State -> Int -> FileNode -> String
equationText state no eqn = 
  leftWidth 2 (show no) ++ ". " ++ mark2Highlights (redexMark eqn state) ++
    take (col (window state)-5) 
       (removeHighlights (prettyEquation2 initialOptions eqn))
    ++ highlightOff
  -- need to remove highlights in equation, because otherwise
  -- highlights not correctly nested and line cut off too early

leftWidth width = reverse . take width . (++ repeat ' ') . reverse

removeHighlights :: String -> String
removeHighlights xs = ys ++ 
  if null zs then [] else (removeHighlights . tail . dropWhile (/= 'm')) zs
  where
  (ys,zs) = span (/= '\ESC') xs

defaultMessage :: State -> String
defaultMessage state =
  "Call " ++ show (curIndex (redexes state)) ++ 
  "/" ++ show (lengthCursorSeq (redexes state)) ++ " | " ++
  (if showFaultSet state then "faulty slice" else "definition") ++ " | " ++
  (if showPart state then "executed part" 
                     else "complete")


force :: [a] -> [a]
-- force evaluation of whole list spine
force = reverse . reverse

cutOffText :: Int -> Text -> String
-- cut off text at given width, handling embedded escape sequences correctly
-- (not all, e.g. not goto)
-- also does not cut off escape sequences appearing just after width
-- incomplete lines are filled up with spaces
cutOffText mw = init . unlines . (map (cutOffLine mw))
  where
  cutOffLine :: Int -> String -> String
  cutOffLine w xs = case xs of
    "" -> replicate w ' '
    (c:cs) -> if c == '\ESC' 
                then let (escapeSeq,end:rest) = break isAlpha cs
                     in c : escapeSeq++[end]++cutOffLine w rest 
              else if w == 0 
                then highlightOff
              else c : cutOffLine (w-1) cs

display :: State -> IO ()
display state = do
  let lastRow = firstRow state + row (window state) - srcRow - 1
      tags = fileTags state
      markedText = markText (text (cur (modules state))) (firstRow state) 
                     lastRow tags
      eqns = reverse (cur (redexes state) : take (eqnNo-1) (parents state))
      firstEqnNo = length (parents state)-length eqns+2
  putStr (home ++ lineWrap False)
  writeFile "test.txt" (show tags)
  (putStr . force . cutOffText (col (window state))) $ 
    -- force minimises flickering of screen output
    highlight [Bold] ("==== Hat-Explore " ++ hatVersionNumber ++ " ==== "
       ++ (if null (message state) then defaultMessage state 
                                   else message state)
       ++ ' ':repeat '=')
    : zipWith (equationText state) [firstEqnNo..] eqns 
    ++ replicate (eqnNo - length eqns) ""
    ++ highlight [Bold] ("---- "++filename (cur (modules state))
      ++" ---- lines "++show (firstRow state)++" to "
      ++show lastRow++' ':repeat '-') 
    : markedText 
  putStr home

data SrcModule = SrcModule
  { filename :: String
  , text :: Text
  , len :: Row
  , border :: Slice
  }

eqnNo = 5 :: Int
srcRow = eqnNo+2 :: Int
srcRows state = row (window state) - srcRow

data State = State 
  { modules :: CursorSeq SrcModule  -- ordered alphabetically
  , window :: Coord
  , firstRow :: Row
  , parents :: [FileNode] -- ancestors of redexes
  , redexes :: CursorSeq FileNode 
      -- current redex + siblings ordered by location
  , correct :: Set FileNode -- trusted ones not listed
  , wrong :: Set FileNode
  , neither :: Set FileNode -- subset of trusted ones
  , faultSetRoot :: FileNode
  , faultSet :: Set FileNode
  , showFaultSet :: Bool
  , showPart :: Bool
  , message :: String
  }

newRedex :: State -> FileNode -> State
-- set given redex in state; assume parents are already set correctly
-- note: a parent might be a pseudo-parent of a constant
newRedex state new = 
  case parents state of
    (parent:_) -> let parChildren = expSort (redexAllChildren parent)
                      (left,rest) = break (== new) parChildren
                  in if null rest -- redex not amongst children of parent
                    then state{redexes = mkCursorSeq [] new []}
                    else state{redexes = mkCursorSeq left new (tail rest)}
    [] -> state{redexes = mkCursorSeq [] new []}

help :: IO ()
help = do
  putStr (cls ++ goto 1 1)
  putStr (highlight [Bold] "==== Help Text ===============================")
  putStr (goto 1 2)
  putStr helpText
  getChar
  return ()
  where
  helpText = " cursor down    follow current call\n cursor up      go back to caller of current call\n cursor left    go to call further left in current definition body\n cursor right   go to call further right in current definition body\n\n c              declare current equation to be correct (wrt. intentions)\n w              declare current equation to be wrong (wrt. intentions)\n n              undo declaration of correctness (neither correct nor wrong)\n a              amnesia - forget all declarations as correct or wrong\n\n f              toggle between showing fault set or just current definition\n p              toggle between showing used part of definition or full\n\n <              change to alphabetically preceeding module\n >              change to alphabetically succeeding module\n\n t              scroll source window to top of code\n u              scroll source window upwards\n d              scroll source window downwards\n b              scroll source window to bottom of code\n\n r              redraw everything after change of window size\n h or ?         display this help text\n\n q              quit\n\n\nMeaning of colours:\ngreen - correct, amber - wrong, blue - unkown, red - faulty.\n\n Press any key to continue. "

loop :: State -> IO ()
loop state1 = do
  let state2 = updRedexFaultyDescendants state1
  display state2
  let state = state2{message = ""}
  c <- getChar
  case c of 
    'q' -> putStr cls
    'u' -> let oldRow = firstRow state
           in loop state{firstRow = if oldRow>1 then oldRow-1 else oldRow}
    'd' -> let oldRow = firstRow state
           in loop state{firstRow = if oldRow<=len (cur (modules state)) 
                                      then oldRow+1 
                                      else oldRow}
    't' -> loop state{firstRow = 1}
    'b' -> loop state{firstRow = max 1 
                                   (len (cur (modules state))
                                    -(row (window state))+2)}
    'r' -> do   
             (width,height) <- getTerminalSize
             loop state{window=Coord{row=height,col=width}}
    'c' -> loop (declareCorrect state) 
    'w' -> loop (declareWrong state)
    'n' -> loop (declareNeither state)
    'a' -> loop state{correct=emptySet,wrong=emptySet,neither=emptySet}
    'f' -> loop state{showFaultSet=not (showFaultSet state)}
    'p' -> loop state{showPart = not (showPart state)}
    '<' -> if lengthCursorSeq (modules state) == 1
             then loop state{message = "There is only one module."}
             else loop state{modules = rotateLeft (modules state)}
    '>' -> if lengthCursorSeq (modules state) == 1
             then loop state{message = "There is only one module."}
             else loop state{modules = rotateRight (modules state)}
    'o' -> do
             system (hatObserve 
               (dropHS (filename (cur (modules state))))
               (head (words (removeHighlights  -- crude approximation
                 (prettyEquation2 initialOptions (cur (redexes state)))))))
             loop state{message = "Spawned hat-observe"}
    'l' -> do 
             system (hatTrail (dropHS (filename (cur (modules state)))) 
               (cur (redexes state)))
             loop state{message = "Spawned hat-trail"}
    'e' -> do 
             system (hatDetect
               (dropHS (filename (cur (modules state))))
               (cur (redexes state)))
             loop state{message = "Spawned hat-detect"}
    'i' -> do 
             system (hatAnim (dropHS (filename (cur (modules state)))) 
               (cur (redexes state)))
             loop state{message = "Spawned hat-anim"}
    'h' -> help >> loop state
    '?' -> help >> loop state
    '\ESC' -> do
      c <- getChar
      if c `elem` "[O" then do
        c <- getChar
        case c of
          'D' -> -- cursor left
                 if lengthCursorSeq (redexes state) == 1
                   then loop state{message = "The call has no siblings."}
                   else loop state{redexes = rotateLeft (redexes state)}
          'C' -> -- cursor right
                 if lengthCursorSeq (redexes state) == 1
                   then loop state{message = "The call has no siblings."}
                   else loop state{redexes = rotateRight (redexes state)}
          'A' -> -- cursor up
                 if not (null (parents state)) then checkModules
                   (newRedex state{parents = tail (parents state)} 
                      (head (parents state)))
                 else loop state{message = 
                      "This is a top-level constant whose caller is unknown."}
          'B' -> -- cursor down
                 -- TO DO: handle constDef differently
                 case expSort (redexAllChildren (cur (redexes state))) of
                   (child:_) -> 
                     checkModules (newRedex 
                       state{parents = cur (redexes state) : parents state} 
                       child)
                   [] -> loop state{message = "There are no child calls."}
       else loop state{message = "Unkown command. Press h for help."}
    _   -> loop state{message = "Unkown command. Press h for help."}

loadModule :: FilePath -> IO SrcModule
loadModule filename = do
  filecontent <- readFile filename
  let srcText = lines (expandTabs 0 filecontent)
  return (SrcModule 
           { filename = filename
           , text = srcText
           , len = length srcText
           , border = borderSlice srcText})

expandTabs :: Col -> String -> String
expandTabs _ "" = ""
expandTabs c (x:xs) = case x of
  '\t' -> replicate spaces ' ' ++  expandTabs (c+spaces) xs
  '\n' -> '\n' : expandTabs 0 xs
  '\r' -> expandTabs 0 xs
  '\v' -> '\n' : expandTabs 0 xs 
  _    -> x : (expandTabs $! (c+1)) xs 
  where
  spaces = 8 - c `mod` 8

checkModules :: State -> IO ()
-- called after redex was changed
-- ensure that module of redex use is loaded and becomes current Module
checkModules state = do
  newModules <- insertModule (cur (redexes state)) (modules state)
  loop (checkRow state{modules = newModules})

insertModules :: [FileNode] -> CursorSeq SrcModule -> IO (CursorSeq SrcModule)
insertModules [] modules = return modules
insertModules (n:ns) modules = do
  modules2 <- insertModule n modules
  insertModules ns modules2

insertModule :: FileNode -> CursorSeq SrcModule -> IO (CursorSeq SrcModule)
-- ensure that module of given node is loaded and becomes current one
insertModule node oldModules =
  let curFilename = fst (getLocation node) 
  in case selectCurrent ((== curFilename) . filename) oldModules of
       Just newModules -> return newModules
       Nothing -> do
         current <- loadModule curFilename
         return (insertNewCurrent 
                  ((< curFilename) . filename) current oldModules)

checkRow :: State -> State
-- insure that redex use (or def if no use) is within the displayed window
checkRow state = 
  if firstInter >= firstRow state &&
     lastInter < firstRow state + rows 
    then state
  else if modLen <= rows
    then state{firstRow = 1}
  else if lastInter - firstInter >= rows
    then state{firstRow = firstInter}
  else let start = firstInter - (rows - (lastInter-firstInter+1)) `div` 2
       in if start + rows -1 > modLen
            then state{firstRow = modLen - rows + 1}
          else if start < 1 
            then state{firstRow = 1}
          else state{firstRow = start}
  where
  rows = srcRows state
  modLen = len (cur (modules state))
  (firstInter,lastInter) = rowsOfInterest state

rowsOfInterest :: State -> (Row,Row)
rowsOfInterest state =
  if hasUseLocation (cur (redexes state)) 
    then (row (begin (head siblingLocations))
         ,row (end (last siblingLocations)))
    else (row (begin curLoc),row (end curLoc))
  where
  (_,curLoc) = getLocation (cur (redexes state))
  Slice siblingLocations =
    foldr mergeSlices emptySlice 
      (map redexUseSlice 
        (cursorSeqToList (redexes state)))

declareCorrect :: State -> State
declareCorrect state@State{correct=cor,wrong=wr,neither=nei} = 
  let red = cur (redexes state)
  in state{correct = if suspectedRedex red then insertSet red cor else cor
          ,wrong = removeSet red wr
          ,neither = removeSet red nei}

declareWrong :: State -> State
declareWrong state@State{correct=cor,wrong=wr,neither=nei} = 
  let red = cur (redexes state)
  in state{correct = removeSet red cor
          ,wrong = insertSet red wr
          ,neither = removeSet red nei}
 
declareNeither :: State -> State
declareNeither  state@State{correct=cor,wrong=wr,neither=nei} = 
  let red = cur (redexes state)
  in state{correct = removeSet red cor
          ,wrong = removeSet red wr
          ,neither = if suspectedRedex red then nei else insertSet red nei}

redexFaulty :: FileNode -> State -> Bool
-- cannot otherwise keep track of faulty redexes, because don't know
-- pseudoparents of a constant.
redexFaulty red state =
  red `elemSet` (wrong state) && 
    all (flip redexCorrect state) (redexAllChildren red)

redexCorrect :: FileNode -> State -> Bool
redexCorrect red state =
  red `elemSet` correct state || 
  not (suspectedRedex red || red `elemSet` neither state || 
       red `elemSet` wrong state)

-----------------------------

type Text = [String] -- list of tab and newline-free lines; 1 char = 1 col
type MarkedText = [String] -- may include escape sequences for highlighting


mark2Highlights :: Mark -> String
mark2Highlights Empty = highlightOn [Normal] -- default marking
mark2Highlights (Mark hi1 hi2 ) = 
  highlightOn (Normal : hi1 ++ hi2)
  -- the foreground colour is never changed
  -- this may be problematic if default foreground does not contrast with
  -- the various background colours chosen here
  -- however, some terminals (e.g. kde) interpret bold + foreground colour
  -- as a lighter foreground colour; so to obtain bold plus background colour
  -- also on these terminals, the foreground is never changed.


markText :: Text -> Row -> Row -> [Tag] -> MarkedText
-- markup input text from first row up to second row
-- result only contains the specified rows
-- filled up with empty rows if necessary
-- assumes at least first row and rows mentioned in tags exist in text
markText text beginRow endRow tags =
  markTextTo startText startTags endRow
  where
  startText = drop (beginRow-1) text
  startTags = getTagsFromRow tags beginRow 


markTextTo :: Text -> [Tag] -> Row -> MarkedText
-- assumes that coords of first tag are coords of first text char
markTextTo text (t:ts) lastRow = 
  (mark2Highlights (mark t)++line):lines
  where
  (line:lines) = go text (start t) ts
  go :: Text -> Coord -> [Tag] -> MarkedText
  go (line:lines) current (t:ts) =
    if row (start t) > lastRow then
      copyLastLines (line:lines) (row current)
    else if row (start t) == row current then
      let (firstLine,restLine) = 
            splitAt (col (start t) - col current) line
          (mRestLine:mRest) = go (restLine:lines) (start t) ts
      in (firstLine ++ mark2Highlights (mark t) ++ mRestLine)
         : mRest
    else -- row (start t) > row current 
      line : go lines Coord{row = row current+1,col = 1} (t:ts)
  go lines current _ = -- either no lines or no tags
    copyLastLines lines (row current)
  copyLastLines :: Text -> Row -> MarkedText
  copyLastLines (line:lines) currentRow =
    if currentRow < lastRow then
      line : copyLastLines lines (currentRow+1)
    else -- currentRow == lastRow
      [line ++ highlightOff]
  copyLastLines [] currentRow = 
    replicate (lastRow-currentRow-1) [] ++ [highlightOff]


splitText :: Text -> Coord -> Coord -> (Text,Text)
-- all texts start and end with incomplete rows
splitText (line:lines) first second =
  if row first == row second then
    let (firstLine,restLine) = splitAt (col second - col first) line
    in ([firstLine],restLine:lines)
  else -- row first < row second
    let (firstText,secondText) = splitText lines Coord{row = row first+1,col = 1} second
    in (line:firstText,secondText)
    


getTagsFromRow :: [Tag] -> Row -> [Tag]
-- first tag starts at beginning of the row
getTagsFromRow tags first = go tags noneM
  where
  go [] m = [Tag{start=Coord{row=first,col=1},mark=m}]
  go (t:ts) m = case compare (row (start t)) first of
    LT -> go ts (mark t)
    EQ -> if col (start t) == 1 then t:ts
          else
            Tag{start=Coord{row=first,col=1},mark=m}:t:ts
    GT -> Tag{start=Coord{row=first,col=1},mark=m}:t:ts
   

data Mark = Empty | Mark [Highlight] [Highlight] deriving Show -- TESTING
  -- second (possibly) contains background colour
  -- first contains all other highlighting
  -- they are separate to simplify combination (does it??)

emptyM = Empty
currentM = Mark [Underscore,Bold] []
markedM = Mark [] [Background Cyan]
correctM = Mark [] [Background Green]
wrongM = Mark [] [Background Yellow]
faultyM = Mark [] [Background Red]
definitionM = Mark [Bold] []
noneM = Mark [] []

combMarks :: Mark -> Mark -> Mark
combMarks Empty _ = Empty
combMarks _ Empty = Empty
combMarks (Mark hi11 hi12) (Mark hi21 hi22) = 
  Mark (hi11 `union` hi21) (hi12 `union` hi22)

data Tag = Tag {start :: Coord, mark :: Mark} deriving Show -- TESTING

combineSlices :: [(Slice,Mark)] -> [Tag]
combineSlices = foldr1 combTags . map (uncurry slice2Tags)


slice2Tags :: Slice -> Mark -> [Tag]
slice2Tags slice m = enter (unSlice slice)
  where
  enter :: [Location] -> [Tag]
  enter [] = []
  enter (l:ls) = if end l < begin l
                   then enter ls 
                   else Tag{start = begin l, mark = m} : exit (l:ls)
  exit :: [Location] -> [Tag]
  exit (l:ls) = Tag{start= afterCoord (end l), mark = noneM} : enter ls

combTags :: [Tag] -> [Tag] -> [Tag]
combTags t1s t2s = go noneM t1s noneM t2s
  where
  go :: Mark -> [Tag] -> Mark -> [Tag] -> [Tag]
  -- keep track of current mark for each tag sequence
  go m1 [] _ ts = map (\t -> t{mark = combMarks m1 (mark t)}) ts
  go _ ts m2 [] = map (\t -> t{mark = combMarks m2 (mark t)}) ts
  go m1 (t1:t1s) m2 (t2:t2s) = case compare (start t1) (start t2) of
    LT -> Tag{start = start t1, mark = combMarks (mark t1) m2} 
          : go (mark t1) t1s m2 (t2:t2s)
    GT -> Tag{start = start t2, mark = combMarks (mark t2) m1} 
          : go m1 (t1:t1s) (mark t2) t2s 
    EQ -> Tag{start = start t1, mark = combMarks (mark t1) (mark t2)}
          : go (mark t1) t1s (mark t2) t2s


-------------------------------------------------------



newtype Slice = Slice {unSlice :: [Location]} 
  -- proper locations, non-overlapping and ordered

compressSlice :: Slice -> Slice
compressSlice (Slice []) = Slice []
compressSlice (Slice [l]) = Slice [l]
compressSlice (Slice (l1:l2:ls)) =
  if isNextCoord (end l1) (begin l2) then
    compressSlice (Slice (Location{begin = begin l1, end = end l2}:ls))
  else
    Slice (l1 : unSlice (compressSlice (Slice (l2:ls))))

mergeSlices :: Slice -> Slice -> Slice
-- additive combination of two slices
mergeSlices rs (Slice []) = rs
mergeSlices (Slice []) rs = rs
mergeSlices (Slice (l1:l1s)) (Slice (l2:l2s)) =
  if end l1 < begin l2 then  -- location 1 before location 2
    Slice (l1 : (unSlice (mergeSlices (Slice l1s) (Slice (l2:l2s)))))
  else if end l2 < begin l1 then  -- location 2 before location 1
    Slice (l2 : (unSlice (mergeSlices (Slice (l1:l1s)) (Slice l2s))))
  else if end l1 < end l2 then
    mergeSlices 
      (Slice l1s) 
      (Slice (Location{begin = min (begin l1) (begin l2),end = end l2} : l2s))
  else -- end l2 <= end l1
    mergeSlices 
      (Slice (Location{begin = min (begin l1) (begin l2),end = end l1} : l1s))
      (Slice l2s)

addToSlice :: Location -> Slice -> Slice
-- maybe a more efficient implementation will be done later
addToSlice loc = mergeSlices (Slice [loc])

subtractFromSlice :: Location -> Slice -> Slice
-- given location must be improper or 
-- nested in one of the locations of the slice
-- or be outside any locations of the slice; no overlap
subtractFromSlice l (Slice []) = emptySlice
subtractFromSlice l1 (Slice (l2:ls)) =
  if isImproperLocation l1 then
    Slice (l2:ls)
  else if begin l1 > end l2 then -- l1 not nested in l2
    Slice (l2 : unSlice (subtractFromSlice l1 (Slice ls)))
  else -- from assumption of nesting follows that l1 nested in l2
    if begin l1 == begin l2 then
      if end l1 == end l2 then
        Slice ls
      else
        Slice (Location{begin=afterCoord (end l1),end=end l2} : ls)
    else
      if end l1 == end l2 then
        Slice (Location{begin=begin l2,end=beforeCoord (begin l1)} : ls)
      else
        Slice (Location{begin=begin l2,end=beforeCoord (begin l1)} :
               Location{begin=afterCoord (end l1),end=end l2} : ls)
  
mergeSlicesMap :: (a -> Slice) -> [a] -> Slice
mergeSlicesMap f ns = foldr mergeSlices emptySlice (map f ns)

manySlices :: (FileNode -> Slice) -> Set FileNode -> Slice
manySlices f set = foldr mergeSlices emptySlice (map f (deSet set))

emptySlice :: Slice
emptySlice = Slice []

mkSlice :: Location -> Slice
mkSlice l = if isVirtLocation l || isNoLocation l then Slice [] else Slice [l]


-------------------------------------------------------------------
-- abstract data type ordered sequence of elements with one current element

data CursorSeq a = CursorSeq {cur :: a, left :: [a], right :: [a]}
-- describes sequence with elements ordered as follows:
-- reverse left ++ cur : right
-- with one element marked as current element

lengthCursorSeq :: CursorSeq a -> Int
lengthCursorSeq cs = 1 + length (left cs) + length (right cs)

cursorSeqToList :: CursorSeq a -> [a]
cursorSeqToList cs = reverse (left cs) ++ cur cs : right cs

mkCursorSeq :: [a] -> a -> [a] -> CursorSeq a
mkCursorSeq l c r = CursorSeq{cur = c, left = reverse l, right = r}

insertNewCurrent :: (a -> Bool) -> a -> CursorSeq a -> CursorSeq a
-- predicate is true for all smaller elements
insertNewCurrent p c cs = let
  elements = reverse (left cs) ++ cur cs : right cs
  (l,r) = span p elements
  in mkCursorSeq l c r

curIndex :: CursorSeq a -> Int
curIndex cs = length (left cs) + 1

selectCurrent :: (a -> Bool) -> CursorSeq a -> Maybe (CursorSeq a)
-- may not find element fulfilling predicate
selectCurrent p cs = let
  elements = reverse (left cs) ++ cur cs : right cs
  (l,r) = break p elements
  in case r of
       [] -> Nothing
       (c:r') -> Just (mkCursorSeq l c r')

rotateLeft :: CursorSeq a -> CursorSeq a
-- jumps at end
rotateLeft cs = let l = left cs in
  if null l 
    then if null (right cs)
           then cs
           else let r = reverse (cur cs:right cs) in
             CursorSeq{cur = head r, left = tail r, right = []}
    else CursorSeq{cur = head l, left = tail l, right = cur cs:right cs} 

rotateRight :: CursorSeq a -> CursorSeq a
-- jumps at end
rotateRight cs = let r = right cs in
  if null r
    then if null (left cs)
           then cs
           else let l = reverse (cur cs:left cs) in
             CursorSeq{cur = head l, left = [], right = tail l}
    else CursorSeq{cur = head r, left = cur cs:left cs, right = tail r}

-------------------------------------------------------------------
-- abstract data type set

newtype Set a = Set {deSet :: [a]}

emptySet :: Set a
emptySet = Set []

insertSet :: Eq a => a -> Set a -> Set a
insertSet x set = if x `elemSet` set then set else Set (x: deSet set)

elemSet :: Eq a => a -> Set a -> Bool
elemSet x set = x `elem` deSet set

removeSet :: Eq a => a -> Set a -> Set a
removeSet x set = Set [y | y <- deSet set, y /= x]

subSet :: Eq a => Set a -> Set a -> Bool
subSet s1 s2 = all (`elemSet` s2) (deSet s1)

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet s1 s2 = foldr insertSet s2 (deSet s1)

-------------------------------------------------------------------
