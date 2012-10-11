module Main where

-- System Imports
import System.IO     (stdin,stdout,stderr,hPutStrLn)
import Data.List     (isSuffixOf, intersperse, nubBy, delete, minimumBy
                     ,isPrefixOf, sortBy)
import Data.Char     (toLower)
import System.Cmd    (system)
import System.Environment (getArgs,getProgName)
import System.Exit   (exitWith,ExitCode(..))
import Foreign.C.String (withCString)
import Numeric       (showHex)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe    (fromJust)

-- Hat Imports
import CommonUI       (Options(..))
import Delta          (doAnim,doDelta,doDetect,doExplore,doObserve,doTrail
                      ,doView,findMain,DetectCommand(..),toCommand,identifyBug
                      ,identifyCycle,findMain,DeltaOption(..),HeuristicMode(..)
                      ,HeuristicBool(..),DetectCommand(..),toCommand
                      ,showHeuristic)
-- import Delta          (questions)
import HighlightStyle (Highlight(..),Colour(..),highlight,getTerminalSize)
import LowLevel       (NodeType(..),openHatFile,FileNode(..),nil,peekTrace
                      ,getResult,getParentNode,getErrorLoc,getErrorMessage
                      ,getSrcRef,getDefnRef,getSubExprs,nodeType)
import SExp           (QName(..),showQN,prettySExp)
import NodeExp        (NodeExp(..),nodeExpForNode,fullEval,flatEval
                      ,removeResultCycles,removeNonResultCycles,getNode
                      ,nodeExp2SExp,isIn,limitDepth,children,(===)
                      ,flatEvalText,fullEvalText,finalResult)
import ADT            (ADT(..),displayTree,displayTrees,trustIO
                      ,trustModule,trustApps,trustConstant,leaves,subADTs
                      ,detectCycles,foldHiddens,trustUnevaluated,(/==)
                      ,trustMatchingFunction)
import EDT            (buildEDT)
import FDT            (buildFDT)
import Explore        (Location(..),getLocation, getDefLocation,redexParent
                      ,Coord(..))
import Pretty         (PrettyOption(..), makeGraph)
import Slice          (Slice,makeSlice)

helpMessage :: String
helpMessage = "\
\hat-delta\n\
\=========\n\
\\n\
\hat-delta is an interactive tool for finding bugs in the trace of a \n\
\program supplied as its argument.  The user must answer a sequence of\n\
\yes/no questions.  Each question asked by hat-delta concerns the reduction \n\
\of a function application to a value.  You answer yes if the reduction is\n\
\correct with respect to your intentions, and no otherwise. After a number \n\
\of questions hat-delta reports an example equation which is the cause of\n\
\the observed faulty behaviour - that is, which function definition is\n\
\incorrect.\n\
\\n\
\Options:\n\
\-------------------------------------------------------------------------\n\
\-c             Disable compression of the ADT based on reporting the same\n\
\               answer for several questions.\n\
\-d depth       Set the maximum depth hat-delta will consider making a jump\n\
\               into the ADT.\n\
\-f function    Set the heuristic function that hat-delta should use.\n\
\               Heuristic functions are input in the form of a Haskell data\n\
\               structure:\n\
\               ValueHeuristic n A floating point value - n\n\
\               Correct          The number of correct evaluations of the\n\
\                                slice.\n\
\               Incorrect        The number of incorrect evaluations of the\n\
\                                slice.\n\
\               Add f1 f2        Add the values of f1 and f2.\n\
\               Negate f         Negate the value of f.\n\
\               Multiply f1 f2   Multiply the values of f1 and f2.\n\
\               Invert f         Invert the value of f.\n\
\-h             Display this help message and exit.\n\
\-n             Set the default value of the heuristic when no slice data\n\
\               is available.\n\
\-q             Read QuickCheck tests to grab data.\n\
\-s sliceDepth  Set the number of child connections hat-delta will follow\n\
\               when generating a program slice.\n\
\-v             Print hat-delta's version number, and exit.\n\
\-------------------------------------------------------------------------\n\
\\n\
\Interactive Commands:\n\
\-------------------------------------------------------------------------\n\
\The hat-delta browser asks you questions, so the basic mode of interaction\n\
\is to type answers:\n\
\yes       Yes, the equation looks correct by my understanding of the\n\
\          meaning of the function.\n\
\no        No, the equation looks incorrect.  Given the displayed arguments,\n\
\          the function is returning the wrong result.\n\
\\n\
\:show     Show the ADTs that hat-delta is currently investigating.\n\
\\n\
\:quit     Exit the interactive tool.\n\
\:help     Shows this help text.\n\
\\n\
\:anim     Start the hat-anim browser in a new window, beginning with the\n\
\          currently queried reduction.\n\
\:delta or Start the hat-delta browser in a new window, beginning with the\n\
\:split    currently queried reduction.\n\
\:detect   Start the hat-detect browser in a new window, beginning with\n\
\          the currently queried reduction.\n\
\:explore  Start the hat-explore browser in a new window, beginning with\n\
\          the currently queried reduction.\n\
\:trail    Start the hat-trail browser in a new window, beginning with the\n\
\          currently queried reduction.\n\
\\n\
\:set      Set an option in the form `:set option value`. Options are:\n\
\          depthLimit   Set the maximum depth hat-delta will consider\n\
\                       making a jump into the ADT. Must be greater\n\
\                       than 0.\n\
\          sliceDepth   Set the number of child connections hat-delta will\n\
\                       follow when generating a program slice. Must be\n\
\                       positive.  A depth of 0 indicates that the whole\n\
\                       function definition should be used as a slice.\n\
\          heuristic    Set the heuristic function that hat-delta should\n\
\                       use.  As described in the command line options\n\
\                       section.\n\
\-------------------------------------------------------------------------\n"

main = do args    <- System.Environment.getArgs
          prog    <- System.Environment.getProgName
          let (modName,options) = getOptions args
          options <- if Version `elem` options
                        then do hPutStrLn stdout versionString
                                exitWith ExitSuccess
                     else if ShowHelp `elem` options
                        then do hPutStrLn stdout helpMessage
                                exitWith ExitSuccess
                     else if modName == ""
                        then do hPutStrLn stderr (usage "no root module")
                                exitWith (ExitFailure 1)
                     else if "--detect" `elem` args
                        then do hPutStrLn stdout hatDetectWarning
                                return $ defaultOptions options
                     else
                        return $ defaultOptions options
          withCString prog (\p -> withCString (hatFile modName) (openHatFile p))
          main <- findMain
          detect (modName,options) $ (removeNonResultCycles . removeResultCycles . nodeExpForNode) main

progName :: String
progName = "hat-delta"

hatDetectWarning :: String
hatDetectWarning =
  highlight [Background Blue] "Warning: hat-detect is now obsolete.  Most users should use hat-delta instead.  hat-delta will now be run in a mode that immitates the behaviour of hat-detect."

version :: Float
version = 2.72

versionString :: String
versionString = progName ++ " version: " ++ (show version) ++ "\n" ++
                "(c) 2005 Thomas Davie\n"

usage :: String -> String
usage err = progName ++ ": " ++ err ++ "\n" ++
            "usage: " ++ progName ++ "[-chqv] [-d depth] [-f heuristicFunction] [-n defaultHeuristicValue] [-s sliceDepth] prog[.hat]"

getOptions :: [String] -> (FilePath, [DeltaOption])
getOptions [] = ("",[])
getOptions (arg:args)
  | arg == "-c"
    = let (f,opts) = getOptions args in (f,DisableADTCompression:opts)
  | arg == "-d"
    = let
        getDepthLimit [] = ("",[])
        getDepthLimit (x:xs) = let (f,opts) = getOptions xs
                               in (f,(DepthLimit $ read x):opts)
      in getDepthLimit args
  | arg == "-f"
    = let
        getHeuristicFunction (x:xs) =
          (f,(Heuristic $ read x):opts)
          where
            (f,opts) = getOptions xs
      in getHeuristicFunction args
  | arg == "-h"
    = let (f,opts) = getOptions args in (f,ShowHelp:opts)
  | arg == "-n"
    = let 
        getSliceDepth [] = ("",[])
        getSliceDepth (x:xs) = let (f,opts) = getOptions xs
                               in (f,(NoSliceValue $ read x):opts)
      in getSliceDepth args
  | arg == "-q"
    = let (f,opts) = getOptions args in (f,QuickCheckMode:opts)
  | arg == "-s"
    = let 
        getSliceDepth [] = ("",[])
        getSliceDepth (x:xs) = let (f,opts) = getOptions xs
                               in (f,(SliceDepth $ read x):opts)
      in getSliceDepth args
  | arg == "-t"
    = let
        getTreeType [] = ("",[])
        getTreeType (x:xs) = let (f,opts) = getOptions xs
                             in (f,(TreeType $ map toLower x):opts)
      in getTreeType args
  | arg == "-v"
    = let (f,opts) = getOptions args in (f,Version:opts)
  | arg == "--detect"
    = let (f,_) = getOptions args
      in (f, [ DisableADTCompression
             , DepthLimit 1
             , SliceDepth 1
             , Heuristic (ValueHeuristic 0.0)])
  | otherwise
    = let (_,opts) = getOptions args in (arg,opts)

defaultOptions :: [DeltaOption] -> [DeltaOption]
defaultOptions =
  defaultTreeType . defaultHeuristic . defaultSliceDepth . defaultDepthLimit
  where
    defaultSliceDepth :: [DeltaOption] -> [DeltaOption]
    defaultSliceDepth options =
      if length (filter isSliceDepth options) == 0
        then (SliceDepth 1:options)
        else options
    defaultDepthLimit :: [DeltaOption] -> [DeltaOption]
    defaultDepthLimit options =
      if length (filter isDepthLimit options) == 0
        then (DepthLimit 3:options)
        else options
    defaultHeuristic :: [DeltaOption] -> [DeltaOption]
    defaultHeuristic options = 
      if length (filter isHeuristic options) == 0
        then (Heuristic (Negate
                          (If (Eq (Add Correct Incorrect)
                                  (ValueHeuristic 0.0))
                              (
                               Multiply (ValueHeuristic 1.0)
                                        (Invert (ValueHeuristic 2.0))
                              )
                              (
                               Multiply Correct
                                        (Invert (Add Correct Incorrect))
                              )
                          )
                        )
               :options)
        else options
    defaultTreeType :: [DeltaOption] -> [DeltaOption]
    defaultTreeType options =
      if length (filter isTreeType options) == 0
        then TreeType "edt" : options
        else options

detect :: (FilePath, [DeltaOption]) -> NodeExp -> IO()
detect opts = interact . questions opts

hatFile :: FilePath -> FilePath
hatFile = (flip rectify) ".hat"

rectify :: FilePath -> String -> FilePath
rectify f ext | ext `isSuffixOf` f = f
              | otherwise = f ++ ext

--- To be removed

data SliceKnowledge = Kn Slice Int Int

trustedModules = ["Prelude", "IO", "Data.Set", "Data.List", "Data.Map"]

instance Show SliceKnowledge where
  show (Kn sl wr cr) =
       show sl
    ++ ": "
    ++ show wr
    ++ " - "
    ++ show cr

data DeltaKnowledge = Slices [SliceKnowledge]

isSliceDepth :: DeltaOption -> Bool
isSliceDepth (SliceDepth _) = True
isSliceDepth _ = False

isDepthLimit :: DeltaOption -> Bool
isDepthLimit (DepthLimit _) = True
isDepthLimit _ = False

isHeuristic :: DeltaOption -> Bool
isHeuristic (Heuristic _) = True
isHeuristic _ = False

isTreeType :: DeltaOption -> Bool
isTreeType (TreeType _) = True
isTreeType _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- Questions are generated by making a list of booleans of answers
-- and then generating a list of questions based on that.  We start
-- with the empty set of knowledge... This may change when we start
-- reading in a corrert program.
questions :: (FilePath, [DeltaOption]) -> NodeExp -> String -> String
questions opts exp =
    unlines
  . map ("\n" ++)
  . (makeQuestions opts exp (Slices []))
  . (map toCommand)
  . lines

-- Make questions makes a list of questions to ask in order
-- to find a bug in the expression given as the first argument.
-- The second argument is a list of answers to the questions.
-- This function must be able to produce at least one question
-- without needing anything from the answer list.
--
-- This implementation simply calls mkQuestions
-- with the continuation (what to do if the user answers no to
-- the first question) being a function that displays "no
-- bugs found".

makeQuestions :: (FilePath, [DeltaOption])
              -> NodeExp
              -> DeltaKnowledge
              -> [DetectCommand]
              -> [String]
makeQuestions (f,opts) n k@(Slices sl) as =
  mkQuestions (f,opts) [(compressTree tree)] newMessage [newKnowledge] as
  where
    newKnowledge =
      if QuickCheckMode `elem` opts
        then Slices
               (compressSlices
                 (   sl
                  ++ (map makeCorrect $ slices opts lvs)
                  ++ (map makeIncorrect
                          (slices opts
                                  (map (\(Branch _ x _ _) -> x) $ snd tests)))))
        else k
    lvs = take 100 $ concat $ map leaves $ fst tests
    tests = collectTests wholeTree
    collectTests :: [ADT] -> ([ADT],[ADT])
    collectTests =   splitOnPass
                   . concat
                   . map findTests
    compressTree =
      if DisableADTCompression `elem` opts
        then id
        else map (removeIrrelevantQuestions opts)
        
    findTests b@(Branch _ (NExpApp _ f _ _) _ ch) =
      case finalResult f of
        NExpIdentifier _ (Plain name) _
          -> if "prop_" `isPrefixOf` name
               then [b]
               else (concat $ map findTests ch)
        NExpIdentifier _ (Qualified _ name) _
          -> if "prop_" `isPrefixOf` name
               then [b]
               else (concat $ map findTests ch)
        otherwise
          -> (concat $ map findTests ch)
    findTests x = []
    
    splitOnPass xs = splitOnPassAux xs ([],[])
    splitOnPassAux [] (ps,fs) = (ps,fs)
    splitOnPassAux (b@(Branch _ test _ _):xs) (ps,fs) =
      case finalResult test of
        NExpIdentifier _ name _
          -> if name == Plain "True"
              then splitOnPassAux xs (b:ps,fs)
              else splitOnPassAux xs (ps,b:fs)
        otherwise
          -> splitOnPassAux xs (ps,fs)
    
    tree =
      if quickCheckMode
        then sortTree workingQCTree
        else wholeTree
      where
        sortTree (Branch _ _ _ ch) = ch
        sortTree (Cycle _ _) = [] 
    
    treeDepth :: Int -> ADT -> Int
    treeDepth = treeDepth' 0
                where
                  treeDepth' :: Int -> Int -> ADT -> Int
                  treeDepth' c m (Branch _ n _ ch)
                    | c == m    = m
                    | otherwise = case ds of
                                    []     -> c
                                    (x:xs) -> maximum ds
                                  where
                                    ds = (map (treeDepth' (c+1) m) ch)
                  treeDepth' c _ (Cycle _ _) = c
    
    newMessage = if quickCheckMode
                   then [identifyTree opts workingQCTree]
                   else ["No bugs found"]
    
    workingQCTree = head (snd tests)
    
    quickCheckMode :: Bool
    quickCheckMode = QuickCheckMode `elem` opts
    
    (TreeType tt) = head $ filter isTreeType opts
    
    wholeTree = (  trustConstant (Plain "otherwise")
                 . (flip (foldr trustModule) trustedModules)
                 . detectCycles
                 . trustUnevaluated
                 -- . trustIO
                 . foldHiddens
                 . (if tt == "fdt"
                      then buildFDT
                      else buildEDT)) n

identifyTree :: [DeltaOption] -> ADT -> String
identifyTree opts (Branch _ n _ _) = identifyBug opts n
identifyTree opts (Cycle _ e) = identifyCycle opts e

mkQuestions :: (FilePath, [DeltaOption])
            -> [[ADT]]
            -> [String]
            -> [DeltaKnowledge]
            -> [DetectCommand]
            -> [String]
mkQuestions _ ([]:_) (report:_) _ _ = [report]
mkQuestions (f,opts)
            (edts@(hd@(Branch _ exp disp chldr):others):prevADTs)
            (report:prevReports)
            (kn@(Slices sl):prevKns)
            ans
  = f' width
    : case ans of
        []            -> ["End of input, exiting"]
        (Yes:xs)      ->
          let lvs = take 100 $ leaves edt'
              compareQuestions (Branch _ n _ _) (Branch _ n' _ _) =
                (flatEval fullEval n) === (flatEval fullEval n')
              compareQuestions (Cycle _ xs) (Cycle _ xs') =
                and (zipWith compareQuestions xs xs')
              compareQuestions _ _ = False
          in mkQuestions (f,opts)
                         ( (trustMatchingFunction True
                                                  (compareQuestions edt')
                                                  edts)
                          :edts:prevADTs)
                         (report:report:prevReports)
                         ((Slices
                            $ compressSlices (sl ++ (map makeCorrect 
                                                         $ slices opts lvs)))
                          :kn:prevKns)
                         xs
        (No:xs)       ->
          mkQuestions (f,opts)
                      (ch:edts:prevADTs)
                      ((identifyBug opts exp'):report:prevReports)
                      ((Slices
                         $ compressSlices (sl ++ (map makeIncorrect
                                                      $ slices opts [exp'])))
                       :kn:prevKns)
                      xs
        (Undo:xs)     ->
          mkQuestions (f,opts)
                      prevADTs
                      prevReports
                      prevKns
                      xs
        (Quit:_)      -> ["User Quit.  No Bugs Found"]
        (Help:_)      -> helpMessage : repeatQuestion
        (Anim:_)      -> seq (unsafePerformIO $ doAnim exp f)
                             repeatQuestion
        (Delta:_)     -> seq (unsafePerformIO $ doDelta exp f)
                             repeatQuestion
        (Split:_)     -> seq (unsafePerformIO $ doDelta exp f)
                             repeatQuestion
        (Detect:_)    -> seq (unsafePerformIO $ doDetect exp f)
                             repeatQuestion
        (Explore:_)   -> seq (unsafePerformIO $ doExplore exp f)
                             repeatQuestion
--        (Observe:_)   -> seq (unsafePerformIO $ doObserve exp f)
--                             repeatQuestion
        (Trail:_)     -> seq (unsafePerformIO $ doTrail exp f)
                             repeatQuestion
        (View:_)      -> seq (unsafePerformIO $ doView exp)
                             repeatQuestion
        (ShowADT:_)   -> displayTrees width (\_ x -> x) edts
                         : repeatQuestion
        (ShowADTHs:_) -> displayTrees width
                                      (showHeuristic (heuristicValue opts kn))
                                      edts
                       : repeatQuestion
        (Children:_)  -> let
                         childQ :: ADT -> String
                         childQ (Branch _ _ f _) = f width
                         childQ (Cycle _ _) = "CYCLE!"
                       in
                         case head edts of
                           (Branch _ _ _ chldr)
                             -> if chldr == []
                                  then "No Children"
                                  else unlines $ map childQ chldr
                           (Cycle _ chldr)
                             -> unlines $ map childQ chldr
                         : repeatQuestion
        (Set var val:_) ->
          case var of
            "depthlimit" -> 
              newOpts (  (DepthLimit $ read val)
                       : (filter (not . isDepthLimit) opts))
            "slicedepth" -> 
              newOpts (  (SliceDepth $ read val)
                       : (filter (not . isSliceDepth) opts))
            "heuristic" ->
              newOpts (  (Heuristic $ read val)
                       : (filter (not . isHeuristic) opts))
            _            -> "Unknown variable" : repeatQuestion
        (Get var:_)   ->
          case var of
            "depthlimit" -> ("Depth Limit: " ++ show depthLimit)
                             : repeatQuestion
            "slicedepth" -> ("Slice Depth: " ++ show sliceDepth)
                             : repeatQuestion
            "heuristic"  -> ("Heuristic: " ++ show heuristic)
                             : repeatQuestion
            _            -> "Unknown variable" : repeatQuestion
        _             -> "Unknown command, type ':help' for help."
                         : repeatQuestion
    where
      edt'@(Branch _ exp' f' ch)
        = minADT opts kn $ subADTs (Just depthLimit) edts
      (DepthLimit depthLimit) = head $ filter isDepthLimit opts
      (SliceDepth sliceDepth) = head $ filter isSliceDepth opts
      (Heuristic heuristic) = head $ filter isHeuristic opts
      repeatQuestion = newOpts opts
      newOpts newOptions = mkQuestions (f,newOptions)
                                       (edts:prevADTs)
                                       (report:prevReports)
                                       (kn:prevKns)
                                       (tail ans)
      (width,_) = unsafePerformIO $ getTerminalSize
      arg :: Int -> NodeExp -> NodeExp
      arg x (NExpApp _ _ as _) = as !! x

mkQuestions (f,opts)
            (edts@(hd@(Cycle _ [Branch _ e _ _]):others):prevADTs)
            (report:prevReports)
            (kn:prevKns)
            ans
  = mkQuestions (f,opts)
                (others:edts:prevADTs)
                ((identifyBug opts
                              e ++ "\nBug in looping function.\n")
                 :report:prevReports)
                (kn:kn:prevKns)
                ans
mkQuestions (f,opts)
            (edts@(hd@(Cycle _ l:others)):prevADTs)
            (report:prevReports)
            (kn:prevKns)
            ans
  = mkQuestions (f,opts)
                (others:edts:prevADTs)
                ((identifyCycle opts l):report:prevReports)
                (kn:kn:prevKns)
                ans
mkQuestions _ _ _ _ _ = error "ADT's corrupt"

minADT :: [DeltaOption] -> DeltaKnowledge -> [ADT] -> ADT
minADT opts kn =
  minimumBy (\(Branch _ x _ _) (Branch _ y _ _)
             -> compare (heuristicValue opts kn x) (heuristicValue opts kn y))

heuristicValue :: [DeltaOption] -> DeltaKnowledge -> NodeExp -> Float
heuristicValue opts kn exp =
    if isNothing sl
      then noSliceHeuristic opts
      else let know = runs kn $ fromJust sl
           in if isNothing know
                then -(evalHeuristic mode $ (Kn (fromJust sl) 0 0))
                else -(evalHeuristic mode $ fromJust know)
    where
      sl = makeSlice sliceDepth exp
      (Heuristic mode) = head $ filter isHeuristic opts
      (SliceDepth sliceDepth) = head $ filter isSliceDepth opts

noSliceHeuristic :: [DeltaOption] -> Float
noSliceHeuristic [] = 0.0
noSliceHeuristic ((NoSliceValue x):xs) = x
noSliceHeuristic (_:xs) = noSliceHeuristic xs

evalHeuristic :: HeuristicMode -> SliceKnowledge -> Float
evalHeuristic (ValueHeuristic n) _ = n
evalHeuristic Correct know =
  (\(Kn _ _ c) -> fromIntegral c) know
evalHeuristic Incorrect know =
  (\(Kn _ w _) -> fromIntegral w) know
evalHeuristic (Add x y) know =
  evalHeuristic x know + evalHeuristic y know
evalHeuristic (Negate x) know =
  -(evalHeuristic x know)
evalHeuristic (Multiply x y) know = 
  evalHeuristic x know * evalHeuristic y know
evalHeuristic (Invert x) know =
  1.0 / (evalHeuristic x know)
evalHeuristic (If c t f) know =
  if evalBool c know
    then evalHeuristic t know
    else evalHeuristic f know

evalBool :: HeuristicBool -> SliceKnowledge -> Bool
evalBool TrueBool  _  = True
evalBool FalseBool _  = False
evalBool (Not x) kn   = not (evalBool x kn)
evalBool (And x y) kn = (evalBool x kn) && (evalBool y kn)
evalBool (Or x y) kn  = (evalBool x kn) || (evalBool y kn)
evalBool (Eq x y) kn  = (evalHeuristic x kn) == (evalHeuristic y kn)
evalBool (Gt x y) kn  = (evalHeuristic x kn) > (evalHeuristic y kn)
evalBool (Lt x y) kn  = (evalHeuristic x kn) < (evalHeuristic y kn)

runs :: DeltaKnowledge -> Slice -> Maybe SliceKnowledge
runs kn@(Slices sls) =
  getRuns sls
  where
    getRuns :: [SliceKnowledge] -> Slice -> Maybe SliceKnowledge
    getRuns [] _ = Nothing
    getRuns (kn@(Kn sl _ _):others) sl'
      | sl == sl' = Just kn
      | otherwise = getRuns others sl'

removeIrrelevantQuestions :: [DeltaOption] -> ADT -> ADT
removeIrrelevantQuestions opts (Branch t n disp ch) =
  Branch t n disp newChildren
  where
    newChildren = map (removeIrrelevantQuestions opts) movedChildren
    movedChildren =
      if isNothing topLevelSlice then ch
      else foldr (++)
                 []
                 (map (findDifferentSlices (fromJust topLevelSlice)) ch)
    topLevelSlice = makeSlice sliceDepth n
    findDifferentSlices :: Slice -> ADT -> [ADT]
    findDifferentSlices sl (Branch t' n' disp' ch') =
      if not (isNothing newSlice) && fromJust newSlice == sl
        then foldr (++) [] (map (findDifferentSlices sl) ch')
        else [Branch t' n' disp' ch']
      where
        newSlice = makeSlice sliceDepth n'
    findDifferentSlices _ cycle = [cycle]
    (SliceDepth sliceDepth) = head $ filter isSliceDepth opts
removeIrrelevantQuestions opts cycle = cycle

elemBy :: a -> [a] -> (a -> a -> Bool) -> Bool
elemBy e l f = any (f e) l

deltaEval :: NodeExp -> NodeExp
deltaEval = flatEval fullEval

slices :: [DeltaOption] -> [NodeExp] -> [Slice]
slices opts =   map fromJust
              . filter (/= Nothing)
              . map (makeSlice sliceDepth)
              where
                (SliceDepth sliceDepth) = head $ filter isSliceDepth opts

makeIncorrect :: Slice -> SliceKnowledge
makeIncorrect sl = Kn sl 1 0

makeCorrect :: Slice -> SliceKnowledge
makeCorrect sl = Kn sl 0 1

compressSlices :: [SliceKnowledge] -> [SliceKnowledge]
compressSlices =
  doCompress . groupBy (\(Kn sl _ _) (Kn sl' _ _) -> sl == sl')
  where
    doCompress [] = []
    doCompress [x] = [x]
    doCompress (x@(Kn sl w c):x1@(Kn sl1 w1 c1):xs)
      | sl == sl1 = doCompress ((Kn sl (w+w1) (c+c1)):xs)
      | otherwise = x : doCompress (x1:xs)

group :: Eq a => [a] -> [a]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [a]
groupBy f xs
  = groupByAux f xs []
    where
      groupByAux :: (a -> a -> Bool) -> [a] -> [a] -> [a]
      groupByAux f [] res = res
      groupByAux f (x:xs) res = groupByAux f xs $ insertInto f x res
      
      insertInto :: (a -> a -> Bool) -> a -> [a] -> [a]
      insertInto f i [] = [i]
      insertInto f i (x:xs)
        | f i x     = i:x:xs
        | otherwise = x:(insertInto f i xs)
      
addSlice :: SliceKnowledge -> [SliceKnowledge] -> [SliceKnowledge]
addSlice s [] = [s]
addSlice s@(Kn sl a b) (s'@(Kn sl' a' b'):others)
  | sl == sl' = (Kn sl (a+a') (b+b')):others
  | otherwise        = s':(addSlice s others)

format :: DeltaKnowledge -> String
format (Slices slices)
  =    "Slices:\n"
    ++ unlines (map show slices)
