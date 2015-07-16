-- hat-observe Main

import Data.Maybe
import Data.List	(sort,isPrefixOf,isSuffixOf,intersperse)
import Control.Monad	(when,liftM)
-- import System
import System.Process   (system)
import System.Exit      (ExitCode(..), exitWith)
import System.Environment (getArgs, getProgName)
import Data.Char	(isDigit,digitToInt,isUpper,toUpper,isAlphaNum,isSpace)
import System.IO	(hFlush,stdout,hSetBuffering,BufferMode(..))
import Foreign.C.String	(withCString)

import Observe		(ObserveResult(..)
			,isFound,isNotFound,isInterrupted,fromFound
			,newObservation,newObservationSrc)
import Ident		(Ident(..),getIdentAt)
import Idents		(collateIdents,getAllIdents,sortIdents,showInfo)
import LowLevel		(FileNode(..),nil,openHatFile,getResult,hatVersionNumber
			,getSrcRef,getDefnRef)
import SrcRef		(SrcRef(..),readSrcRef,defnSrcRef)
import SExp		(SExp(..),Label,prettyEquation,prettyExpression
			,fileNode2SExp,funId,label,QName(..))
import HighlightStyle	(cursorUp,cleareol,highlight,Highlight(..),Colour(..)
			,getTerminalSize, hasEscapes)
import Trie		(Trie,Search(..),emptyTrie,match)
import TExp		(linearise)
import Pattern		(topMatchPat,parsePat,lexPat)
import CmdLine		(initialize,cmdline)
import CommonUI		(hatObserve,hatTrail,hatAnim,hatDetect,hatExplore
                        ,hatView
			,shortHelpText
			,Keep(..),Options(..),initialOptions
			,OptionCmd(..),optionCmd,onOff,number,safeReadInt
			,optionsUpdate,showOption)

----
data InteractiveState =
    State
	{ lastObserved	:: [FileNode]
	, more		:: Bool
	, equationsPerPage :: Int
	, hatFile       :: String
	, currentPos	:: Int
	, showRHS	:: Bool
	, options	:: Options
	, screenWidth   :: Int
	, symbolTable   :: ([Ident],[Ident],[Ident])
	}
initialState :: FilePath -> Int -> InteractiveState
initialState file width = State
	{ lastObserved = []
	, more = False
	, equationsPerPage = 10
	, hatFile = file
	, currentPos = 0
	, showRHS = True
	, options = initialOptions
	, screenWidth = width
	, symbolTable = ([],[],[])
	}
----

-----------------------------------------------------------------------
-- main entry point
-----------------------------------------------------------------------

main = do
    hSetBuffering stdout NoBuffering
    arguments <- System.Environment.getArgs
 -- putStrLn ("cmdline args: "++show arguments)
    let numArgs = length arguments
    when (numArgs < 1) (do putStrLn cmdlineHelp
                           exitWith (ExitFailure 1))
    putStrLn ("\n        hat-observe "++hatVersionNumber
              ++"    ("++shortHelpText++")\n")
    (width,_) <- getTerminalSize
    CmdLine.initialize	-- for readline functionality
    let hatFile = rectify (arguments!!0)
        state   = initialState hatFile width
    prog <- System.Environment.getProgName
    withCString prog (\p-> withCString hatFile (openHatFile p))
    when hasEscapes $ putStrLn (highlight [Foreground Red] "Loading symbol table...")
    collateIdents
    syms <- getAllIdents
    let state' = state { symbolTable = syms }
    putStr clearLine	-- delayed until next output appears
    case numArgs of
      1 -> interactive state'
      2 -> doCommand (Pattern (arguments!!1)) state'
      4 -> doCommand (Location (tail arguments)) state'
      _ -> do putStrLn cmdlineHelp
              exitWith (ExitFailure 1)
  where
    rectify :: FilePath -> FilePath
    rectify f | ".hat" `isSuffixOf` f = f
              | otherwise = f ++ ".hat"

----
showObservation :: InteractiveState -> Int -> FileNode -> IO ()
showObservation state@(State{options=opts}) i node = do
  putStr clearLine  -- delay relies on line-buffered output
  putStrLn (display (screenWidth state) opts node)
  where
  display | equations opts && showRHS state = prettyEquation (show i++" ") ""
          | otherwise                       = prettyExpression (show i++" ")


-- arguments to showObservationList are:
--   i     :: Int		numbered equation
--   max   :: Int		how many equations to show at once
--   exprs :: [FileNode]	roots of equations
-- result is number of equations that have been shown
showObservationList :: InteractiveState -> Int -> Int -> [FileNode] -> IO Int
showObservationList state _ _ [] = do putStr clearLine; return 0
showObservationList state i 0 _  = do putStr clearLine; return 0
showObservationList state i max (e:r) = do
    showObservation state i e
    beginSearching
    count <- showObservationList state (i+1) (max-1) r
    return (count+1)

makeObserveSrc :: [String] -> ObserveResult
makeObserveSrc [mod,line,col] | all isDigit line && all isDigit col =
    newObservationSrc mod (safeReadInt 0 line) (safeReadInt 0 col)
makeObserveSrc _  | otherwise = NotFound


makeObserve :: InteractiveState -> QName -> Maybe QName -> SExp () -> Int
               -> ObserveResult
makeObserve State{options=opts} ident1 ident2 pat arity =
    let observed = newObservation ident1 ident2 (recursiveMode opts) arity in
    if not (isFound observed) then observed
    else ( Found
         . map fromEqn
         . filter (\o-> topMatchPat o pat)
         . (case filterMode opts of
              All -> id
              _   -> uniqueify)
         . map toSEqn
         . fromFound)
         observed


-- This is a unique-ifier which builds a Trie to compare expressions.
-- Each expression is represented as a token stream, obtained by first
-- converting the FileNode to an expression tree, then linearising the
-- tree (by pre-order traversal).  The token stream is matched against
-- the Trie, and if found, the corresponding FileNode is discarded.  If
-- not found, the representation is stored in the Trie, and the expression
-- returned.
uniqueify :: [SExp Label] -> [SExp Label]
uniqueify eqns = trymatch emptyTrie eqns
  where trymatch trie [] = []
        trymatch trie (eqn:eqns) =
            case match (linearise eqn) trie of
              Exists    -> trymatch trie eqns
              New trie' -> eqn: trymatch trie' eqns

-- For unique-ification *and* for pattern matching, we first need an
-- expression-tree representation of the equation at the file node.
-- This representation has a *very* deep cutoff value for correctness,
-- and no sugaring of subexprs to allow all forms of matching.
toSEqn :: FileNode -> SExp Label
toSEqn node =
  SEquation ("",nil)
            (fileNode2SExp 2000 False False True ("l",node))
            (fileNode2SExp 2000 False False False ("r",getResult node True))

-- After unique-ification and pattern matching, we want the original
-- file node back.
fromEqn :: SExp Label -> FileNode
fromEqn (SEquation _ lhs rhs) = snd (label lhs)



showSomeMore :: InteractiveState -> IO (Int,Bool)
showSomeMore state =
  let showNowList = drop (currentPos state) (lastObserved state);
      hasMore = not (null (drop (equationsPerPage state) showNowList))
  in do
    beginSearching
    count <- showObservationList state (currentPos state + 1)
	                         (equationsPerPage state) showNowList
    return (count + currentPos state, hasMore)

beginSearching :: IO ()
beginSearching = when hasEscapes $ putStrLn (highlight [Foreground Red] "searching: (^C to interrupt)")

clearLine = if hasEscapes then cursorUp++cleareol else ""

interactive :: InteractiveState -> IO ()
interactive state
    | more state =
        do cmd <- getCommand "--more--> "
           case cmd of
             Pattern _ -> interactive state{more=False}
             _ -> doCommand cmd state
    | otherwise =
        do cmd <- getCommand "hat-observe> "
           doCommand cmd state


getEquationNumber :: Int -> [FileNode] -> IO (Maybe FileNode)
getEquationNumber n lastObserved =
  let nodes = drop (n-1) lastObserved in
  if n>0 then
       if null nodes then do -- This test may take a while!
	    putStrLn "No equation with this number"
	    return Nothing
       else return (Just (head nodes))
  else return Nothing


----
setState :: Mode -> InteractiveState -> InteractiveState
setState (O o)     state = state {options=optionsUpdate o (options state)}
setState (Group n) state = state {equationsPerPage=max n 1}

showState :: Mode -> InteractiveState -> String
showState (O o)     state  = showOption o (options state)
showState (Group _) state  = "  "++ highlight [Underscore] "group"
                             ++ "\t\t" ++ show (equationsPerPage state)++"\t"
                             ++ "number of equations per group"

----
doCommand :: Cmd -> InteractiveState -> IO()
doCommand (Help s) state = do interactiveHelp (dropWhile isSpace s)
                              interactive state
doCommand Quit state =
    if more state then interactive state{more=False}
    else return ()
doCommand More state =
    if more state then do
        putStr clearLine
        (newPos,newMore) <- showSomeMore state
	interactive (state {more=newMore,currentPos=newPos})
    else do
        when (currentPos state>0) (putStrLn "No more applications observed.")
        interactive state
doCommand (Info mod) state =
     do let (glob,_,_) = symbolTable state
        putStr (showInfo mod (sortIdents glob) "")
	interactive state
doCommand (InfoC mod) state =
     do let (_,_,constrs) = symbolTable state
        putStr (showInfo mod (sortIdents constrs) "")
	interactive state
doCommand Resize state =
     do (width,_) <- getTerminalSize
        interactive (state{screenWidth=width})
doCommand Count state =
     do when (more state)
             (putStrLn "One moment, this may take a while...")
	putStrLn ("Number of (all,unique) matching applications: "
		  ++(show (length (lastObserved state))))
	interactive state

doCommand Status state =
     do mapM_ (\m-> putStrLn (showState m state))
              [ O (Uneval True), O (Strings True), O (Lists True)
              , O (Recursive True), O (Qualify True), O (Equations True)
              , Group 0, O (CutOff 0), O (Filter All) ]
        interactive state
doCommand (Set mode) state =
     do let state' = setState mode state
        putStrLn (showState mode state')
        interactive state'

doCommand (StartTool tool) state =
     do startExternalTool tool state
	interactive state
doCommand (Source n) state =
     do node <- getEquationNumber n (lastObserved state)
        let sr = getSrcRef (fromJust node)
	when (isJust node && sr /= LowLevel.nil)
             (do system (hatView (readSrcRef sr))
                 return ())
	interactive state
doCommand (Definition n) state =
     do node <- getEquationNumber n (lastObserved state)
        let atom = getDefnRef (fromJust node)
	when (isJust node)
             (do defnSR <- liftM defnSrcRef (getIdentAt atom)
                 system (hatView defnSR)
                 return ())
	interactive state

doCommand (Pattern s) state =
    let (pat, ctx) = parsePat (lexPat s) in
    case pat of
      Left str -> do putStrLn ("Error in pattern: "++str)
                     interactive state
      Right p  -> do
        beginSearching
        let fun = funId p
            arity p = case p of
                        SApp _ es -> length es - 1
                        SEquation _ e _ -> arity e
                        _ -> 0
            newObserved = makeObserve state fun ctx p (arity p)
        if isNotFound newObserved || null (fromFound newObserved)
          then do
            putStrLn (clearLine ++ "no match found")
            interactive state
	  else do
            putStr clearLine
            (newPos,newMore) <- showSomeMore
                                   (state { currentPos=0
                                          , showRHS=not (isCon fun)
	 			          , lastObserved=fromFound newObserved})
            interactive (state { lastObserved=fromFound newObserved
                               , more=newMore
                               , showRHS=not (isCon fun)
                               , currentPos=newPos})
doCommand (Location ss) state =
    do putStrLn (highlight [Foreground Red]
                           "searching (source reference): (^C to interrupt)")
       let newObserved = makeObserveSrc ss
       if isNotFound newObserved || null (fromFound newObserved)
         then do
           putStrLn (clearLine ++"no match found")
           interactive state
         else do
           putStr clearLine
           (newPos,newMore) <- showSomeMore
                                  (state { currentPos=0
                                         , showRHS=True
	 			         , lastObserved=fromFound newObserved})
           interactive (state {lastObserved=fromFound newObserved
                              ,more=newMore
                              ,showRHS=True
                              ,currentPos=newPos})
doCommand (Shell cmd) state =
     do err <- system cmd
        when (err/=ExitSuccess) (putStrLn "shell command exited abnormally")
        interactive state
doCommand Unknown state =
     do putStrLn ("Unknown command.  "++shortHelpText)
        interactive state


isCon :: QName -> Bool
isCon qn = con v
  where con (x:xs) = isUpper x || x `elem` ":[,"
        v = case qn of Plain v -> v; Qualified _ v -> v

-- Start up a new window with a new instance of a hat tool.
startExternalTool tool state =
  let file = hatFile state
      checkOK errcode s = when (errcode/=ExitSuccess)
                               (putStrLn ("ERROR: Unable to start "++s++"."))
  in case tool of
      Choose -> do
          putStrLn "choose :observe, :detect, :trail, :animate, or :explore"
      Observe pat -> do
          errcode <- system (hatObserve file pat)
          checkOK errcode "hat-observe"
      Trail num -> do
          node <- getEquationNumber num (lastObserved state)
          if isJust node then do
              errcode <- system (hatTrail file (fromJust node))
              checkOK errcode "hat-trail"
            else putStrLn ("ERROR: Equation "++show num++" not available.")
      Anim num -> do
          node <- getEquationNumber num (lastObserved state)
          if isJust node then do
              errcode <- system (hatAnim file (fromJust node))
              checkOK errcode "hat-anim"
            else putStrLn ("ERROR: Equation "++show num++" not available.")
      Detect num -> do
          node <- getEquationNumber num (lastObserved state)
          if isJust node then do
              errcode <- system (hatDetect file (fromJust node))
              checkOK errcode "hat-detect"
            else putStrLn ("ERROR: Equation "++show num++" not available.")
      Explore num -> do
          node <- getEquationNumber num (lastObserved state)
          if isJust node then do
              errcode <- system (hatExplore file (fromJust node))
              checkOK errcode "hat-explore"
            else putStrLn ("ERROR: Equation "++show num++" not available.")


interactiveHelp s = do
    putStrLn (if null s then basicHelptext
              else detailedHelp s)
  where
    basicHelptext = "\
\------------ :help <cmd> for more detail ----------------------------------\n\
\ <query>            observe named function (:help query for pattern syntax)\n\
\ <RETURN>           show more observations (if available)\n\
\ :info              see a list of all observable functions\n\
\ :Info              see a list of all observable data constructors\n\
\ :detect <n>        start hat-detect on equation <n>\n\
\ :trail <n>         start hat-trail browser on equation <n>\n\
\ :animate <n>       start hat-anim on equation <n>\n\
\ :explore <n>       start hat-explore on equation <n>\n\
\ :observe [query]   start hat-observe in a new window with the new query\n\
\ :source <n>        show the source application for equation <n>\n\
\ :Source <n>        show the source definition for identifier in eqn <n>\n\
\ :set               show all current mode settings\n\
\ :set <flag>        change one mode setting (:help set for more detail)\n\
\ :+[n]              short-cut to increase cutoff depth by <n> (default 1)\n\
\ :-[n]              short-cut to decrease cutoff depth by <n> (default 1)\n\
\ :resize            detect new window size for pretty-printing\n\
\ :help <cmd>   :?   show this help text (:help <cmd> for more detail)\n\
\ :quit              quit\n\
\---------------------------------------------------------------------------"

    detailedHelp s
      | ':' == head s         = detailedHelp (tail s)
      | s `isPrefixOf` "query" = "\
\---------------------------------------------------------------------------\n\
\ * A simple function identifier finds all applications of that function.\n\
\     e.g. myfn\n\
\ * To restrict the number of equations, follow the function name with\n\
\   argument or result patterns.\n\
\     e.g. myfn ((:) 1 ((:) 2 _)) \n\
\          myfn \"Hello World!\" (1:(2:_)) = [1,_]\n\
\ * Another way of refining the search is to ask for calls only from a\n\
\   specific enclosing function.\n\
\     e.g. myfn _ (Con 2 _) (1 `Con` 3) in myOtherFn\n\
\\n\
\ The full query syntax is:\n\
\    identifier [pattern]*  ['=' pattern]?  ['in' identifier]?\n\
\ where\n\
\    pattern = '_'                              wildcard\n\
\            | num                              number\n\
\            | ''' char '''                     character\n\
\            | '\"' string '\"'                   string\n\
\            | '[' pattern [',' pattern]* ']'   literal list\n\
\            | Con                              nullary constructor\n\
\            | '(' Con [pattern]* ')'           constructor application\n\
\ Infix functions/constructors take the normal infix syntax.\n\
\---------------------------------------------------------------------------"
      | s `isPrefixOf` "info" || s `isPrefixOf` "Info" = "\
\ :info [module]     see a list of all observable functions\n\
\ :Info [module]     see a list of all observable data constructors\n\
\    Identifiers are listed for the named module, or if no module is named,\n\
\    then for all modules, sorted alphabetically, with occurrence counts.\n\
\    A blue count indicates completed calls, a red count indicates\n\
\    uncompleted calls (unevaluated call counts are not shown)."
      | s `isPrefixOf` "set"  = "\
\ :set               show all current mode settings\n\
\ :set <flag>        change one mode setting\n\
\   <flag> can be: uneval [on|off]      show unevaluated expressions in full\n\
\                  strSugar [on|off]    sugar character strings\n\
\                  listSugar [on|off]   sugar lists\n\
\                  recursive [on|off]   show recursive calls\n\
\                  qualified [on|off]   show all identifiers qualified\n\
\                  equations [on|off]   show rhs of equations\n\
\                  [all|unique]         show all equations or only unique\n\
\                  group <n>            number of equations listed per page\n\
\                  cutoff <n>           cut-off depth for deeply nested exprs"
      | s `isPrefixOf` "detect"  = "\
\ :detect <n>        start hat-detect on equation <n>\n\
\    hat-detect is a browser that asks\n\
\    questions about whether certain equations are correct or incorrect,\n\
\    using an automatic method to locate the source of a bug."
      | s `isPrefixOf` "explore"  = "\
\ :explore <n>        start hat-explore on equation <n>\n\
\    hat-detect allows source-based browsing."
      | s `isPrefixOf` "animate"  = "\
\ :animate <n>       start hat-anim on equation <n>\n\
\    hat-anim is new and potentially buggy.  It is a browser that animates\n\
\    the reduction of an expression to its final value."
      | s `isPrefixOf` "trail"  = "\
\ :trail <n>         start hat-trail browser on equation <n>\n\
\    hat-trail is an interactive browser that permits exploration backwards\n\
\    from a value, expression, or error message, through the function calls\n\
\    that ultimately led to the production of that value.\n\
\    When invoked from within hat-observe, hat-trail begins with the\n\
\    expression on the left of the equation numbered <n> in the list of\n\
\    equations shown by hat-observe."
      | s `isPrefixOf` "observe"  = "\
\ :observe [query]   start hat-observe in a new window with the new query\n\
\    With no argument, a new interactive hat-observe window is started for\n\
\    the same traced program.  Given a query argument, the new window starts\n\
\    with an immediate search for the expression pattern before giving\n\
\    an interactive prompt."
      | otherwise = " topic '"++s++"' has no further help text"

cmdlineHelp = "\
\Usage: hat-observe prog[.hat]\n\
\         An interactive tool to show actual function applications within\n\
\         a traced run of a Haskell program."

{-
cmdlineHelp = "\
\Usage:   hat-observe [-v] [-r] [-xu] identifier [in topidentifier] filename\n\
\Description:\n\
\       prints a table of all applications and results of the given\n\
\       top-level identifier [within the application of topidentifier].\n\
\Options:\n\
\       v: verbose mode. Unevaluated expressions are shown in full.\n\
\       r: recursive mode.  Do not omit recursive function applications.\n\
\       xu: expert's mode for a very fast response. All applications\n\
\           of the identifier are shown, rather than only the most\n\
\           general ones."
-}


data Cmd  = Pattern String | Location [String]
          | More | Info String | InfoC String | Count
          | Help String | Quit | Unknown
          | StartTool Tool | Source Int | Definition Int
          | Status | Set Mode | Shell String | Resize
data Tool = Trail Int | Anim Int | Observe String | Detect Int | Explore Int 
          | Choose
data Mode = Group Int | O OptionCmd

getCommand :: String -> IO Cmd
getCommand prompt = do
    s <- cmdline prompt
    if null s then return More
   -- else if all isDigit s then return (number (StartTool Choose) [s] 0)
      else if head s /= ':' then return (Pattern s)
      else case words (tail s) of
          [] -> return Unknown
          (cmd:ss)
              | cmd `isPrefixOf` "quit" -> return Quit
              | cmd `isPrefixOf` "help" -> return (Help (unwords ss))
              | cmd `isPrefixOf` "location" -> return (Location ss)
              | cmd `isPrefixOf` "detect"   ->
				return (number (StartTool . Detect) ss 0)
              | cmd `isPrefixOf` "explore"   ->
				return (number (StartTool . Explore) ss 0)
              | cmd `isPrefixOf` "trail"    ->
				return (number (StartTool . Trail) ss 0)
              | cmd `isPrefixOf` "animate"    ->
				return (number (StartTool . Anim) ss 0)
              | cmd `isPrefixOf` "observe"  ->
				return (StartTool (Observe (unwords ss)))
              | cmd `isPrefixOf` "source"   -> return (number Source ss 0)
              | cmd `isPrefixOf` "Source"   -> return (number Definition ss 0)
              | cmd `isPrefixOf` "info"     -> return (Info (unwords ss))
              | cmd `isPrefixOf` "Info"     -> return (InfoC (unwords ss))
              | cmd `isPrefixOf` "resize"   -> return Resize
              | cmd `isPrefixOf` "set"      ->
                  case ss of
                      []      -> return Status
                      (s:sss) ->
                        case optionCmd ss of
                          Just o  -> return (Set (O o))
                          Nothing -> if s `isPrefixOf` "group"
                                     then return (Set (number Group sss 10))
                                     else return Unknown
              | head cmd == '?' -> return (Help (unwords (tail cmd:ss)))
              | head cmd == '+' -> return (number (Set . O . Deeper)
                                                  (tail cmd:ss) 1)
              | head cmd == '-' -> return (number (Set . O . Shallower)
                                                  (tail cmd:ss) 1)
              | head cmd == '!' -> return (Shell (unwords (tail cmd:ss)))
              | otherwise  ->  return Unknown

