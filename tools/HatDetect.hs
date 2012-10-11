import LowLevel		(FileNode(..),nil,hatVersionNumber,openHatFile
			,getSrcRef,getDefnRef)
import qualified SrcRef           (SrcRef(..), readSrcRef)
import SExp		(prettyEquation)
import TExp		(TExp,linearise)
import HighlightStyle	(highlight,Highlight(..),Colour(..),getTerminalSize)
import Foreign.C.String (withCString)
import Numeric          (showHex)
import System.IO.Unsafe (unsafePerformIO)
import Detect		(findMain,edtNextChild,ParentSet,newParentSet,anySuspect)
import CmdLine		(initialize,cmdline)
import CommonUI		(hatObserve,hatTrail,hatAnim,hatView,shortHelpText
			,Options(..),initialOptions,Keep(..)
			,OptionCmd(..),optionCmd,onOff,number,safeReadInt
			,optionsUpdate,showOption,showOnOff)
import Data.Maybe
import System.Environment (getArgs,getProgName)
import System.Exit      (exitWith,ExitCode(..))
import System.Cmd    (system)
import Control.Monad    (when)
import Data.List        (isPrefixOf,isSuffixOf,intersperse)
import Data.Char        (isDigit,digitToInt,toLower,isSpace)
import System.IO        (hFlush,stdout,hSetBuffering,BufferMode(..))
import System.Directory (doesFileExist)


{-
Issues to resolve:
  The 'memoise' option is really the same as Filter Unique ?
  Should the :observe command take the current equation, a numbered eqn,
  or a string query?
  Should the :detect command exist at all (only for orthogonality)?
  Should the :trail command take an equation number, or the current eqn?
-}

-----------------------------------------------------------------------
-- type for state of session
data State = State
	{ filename    :: FilePath	-- .hat file name
	, treePath    :: [ParentSet]	-- path to here through EDT (head=here)
	, currentNode :: FileNode	-- location of current equation
--	, children    :: [FileNode]	-- current EDT children
--	, recentNodes :: [(FileNode, LinExpr, Bool)] -- already considered nodes
	, trusted     :: [FileNode]	-- trusted fun-identifiers
	, postponed   :: [(Int,FileNode)]-- postponed questions
	, questnumber :: Int		-- current question number
	, screenWidth :: Int		-- for pretty-printing
	, options     :: Options	-- user-configurable display options
	, memoMode    :: Bool		-- memoizeMode
	, reconsider  :: Bool		-- True when reconsidering a
	}				--   postponed question
initialState :: FilePath -> ParentSet -> State
initialState file start = State
	{ filename = file
	, treePath = [start]
	, currentNode = LowLevel.nil
--	, children = [start]
--	, recentNodes = []
	, trusted = []
	, postponed = []
	, questnumber = 1
	, screenWidth = 80
	, options = initialOptions
	, memoMode = True
	, reconsider = False
	}

setState :: Mode -> State -> State
setState (O o)       state = state {options=optionsUpdate o (options state)}
setState (Memoise b) state = state {memoMode=b}

showState :: Mode -> State -> String
showState (O o)       state = showOption o (options state)
showState (Memoise _) state = "  "++ highlight [Underscore] "memoise"
                              ++ showOnOff (memoMode state)
                              ++ "remember previous answers"


-----------------------------------------------------------------------
main = do
    hSetBuffering stdout NoBuffering
    arguments <- System.Environment.getArgs
    let numArgs = length arguments
    when (numArgs < 1) (do putStrLn cmdlineHelp
                           exitWith (ExitFailure 1))
    putStrLn ("\n        hat-detect "++hatVersionNumber
             ++" ("++shortHelpText++")\n")
    (width,_) <- getTerminalSize
    CmdLine.initialize	-- for readline functionality
    let hatFile = rectify (arguments!!0)
    prog <- System.Environment.getProgName
    withCString prog (\p-> withCString hatFile (openHatFile p))
    case numArgs of
      1 -> do mainNode <- findMain
              if mainNode == LowLevel.nil
                then error "Bad file format! \"Main.main\" could not be found!"
                else do mainPS <- newParentSet mainNode
                        askQuestion mainNode mainPS
                                    (initialState hatFile mainPS)
                                                           { screenWidth=width }
      2 -> case safeReadInt 0 (arguments!!1) of
             0 -> do putStrLn cmdlineHelp
                     exitWith (ExitFailure 1)
             a -> do startPS <- newParentSet (FileNode a)
                     askQuestion (FileNode a) startPS
                                 (initialState hatFile startPS)
                                               { screenWidth=width }
                     putStrLn (highlight [Foreground Blue] "Press return.")
                     _ <- getLine
                     return ()
      _ -> do putStrLn cmdlineHelp
              exitWith (ExitFailure 1)
    putStrLn (highlight [Foreground Blue] "Done.")
  where
    rectify :: FilePath -> FilePath
    rectify f | ".hat" `isSuffixOf` f = f
              | otherwise = f ++ ".hat"

-----------------------------------------------------------------------



-- Add new node to the list of already-seen nodes.  List holds node, linear
-- representation of it, and the value of the users answer (Yes=True, No=False)
addToRecentNodes :: [(FileNode,[TExp],Bool)] -> FileNode -> Bool
                    -> [(FileNode,[TExp],Bool)]

addToRecentNodes recentNodes node answerYes =
 {- (node, linearise (toHatExpressionTree 100 node), answerYes)
    :  -}
    recentNodes

-- check whether node is less general than an earlier given answer
memoizeCheck :: [(FileNode,[TExp],Bool)] -> FileNode -> Maybe Bool
memoizeCheck recentNodes node =
    memoizeCheck' recentNodes [] {-(linearise (toHatExpressionTree 100 node))-}
  where
    memoizeCheck' [] _ = Nothing
 -- memoizeCheck' ((_,expr2,answer):recentNodes) expr1 =
 --     if compareExpr expr1 expr2 then Just answer
 --     else memoizeCheck' recentNodes expr1

-- The main interactive loop
interactive :: FileNode -> ParentSet -> State -> IO ()
interactive node ps state = do
--  node = parent of current question, ps = complete trail of parents
    cmd <- getCommand "hat-detect> "
    doCommand cmd node ps state

askQuestion :: FileNode -> ParentSet -> State -> IO ()
askQuestion node ps state = do
    c <- edtNextChild ps -- (head (treePath state))
 -- t <- anySuspect c
    if c==LowLevel.nil
      then do putStrLn ("bug found at "++showHex (int node) "")
              putStrLn (prettyEquation "    " ""
                                       (screenWidth state)
                                       (options state)
                                       node)
              return ()
      else do putStrLn (prettyEquation (show (questnumber state)++"  ")
                                       "" -- (showHex (int c) (" "++show t))
                                       (screenWidth state)
                                       (options state)
                                       c)
              interactive node ps state { currentNode = c
					, questnumber = questnumber state +1 }

{-
interactive :: State -> IO (Bool,Int,State)
interactive state
    | null (children state) && null (postponed state) =
        -- nothing else to do
        return (True,0,state)
interactive state
    | null (children state) && not (null (postponed state)) =
        -- take one postponed question as new current question
        interactive state {children=[node]
                          ,questnumber=qn
                          ,postponed=tail (postponed state)
                          ,reconsider=False
                          }
    where (qn,node) = head (postponed state)
interactive state | not (null (children state)) =
  let node = head (children state) -- ask about the next remaining EDT child
      answer = memoizeCheck (recentNodes state) node in
  if node  `elem`  map (\(a,_,_)->a) (recentNodes state)
      || (memoMode state && isJust answer)
  then if fromJust answer -- don't ask about identical(!) node in memoizemode
       then doCommand (Answer Yes) state -- pretend user answered "YES"
       else doCommand (Answer No)  state -- pretend user answered "NO"
  else if hatLeftmost node `elem` trusted state
       then doCommand (Answer Yes) state -- pretend user answered "YES"
       else do when (reconsider state) (putStrLn "reconsider: ")
               putStr (prettyEquation (show (questnumber state)++" ") ("? ")
                                      (screenWidth state)
                                      (options state)
                                      node)
               hFlush stdout
               cmd <- getCommand "hat-detect> "
               doCommand cmd state
-}


doCommand :: Cmd -> FileNode -> ParentSet -> State -> IO ()

doCommand Quit n ps state = return ()
doCommand (Help s) n ps state =
     do interactiveHelp (dropWhile isSpace s)
        interactive n ps state
doCommand Status n ps state =
     do mapM_ (\m-> putStrLn (showState m state))
              [ O (Uneval True), O (Strings True), O (Lists True)
              , O (Qualify True), O (Equations True), Memoise True
              , O (CutOff 0), O (Filter All) ]
        interactive n ps state
doCommand (Set mode) n ps state =
     do	let state' = setState mode state
        putStrLn (showState mode state')
        interactive n ps state'
----
{-
doCommand Children n ps state =
     do putStrLn ("children = "++show (children state))
        interactive n ps state
doCommand Trust n ps state = -- user defined function trusting
     do let child = head (children state)
            trustFun = hatLeftmost child
        putStrLn (showExpression trustFun "   Ok, \""
                  ++ "\" is trusted from now on.")
        (b,q,state') <- interactive n ps state
				{recentNodes = addToRecentNodes
                                                   (recentNodes state)
                                                   child True
				,trusted = trustFun:trusted state
				,questnumber = questnumber state + 1
				,reconsider = False
				}
        if q/=questnumber state then return (b,q,state')
          else interactive n ps state { options = options state'
                                   , memoMode = memoMode state'
                                   , reconsider = False
                                   }
-}
doCommand Untrust n ps state =
     do putStrLn "Ok, all functions are untrusted now."
        interactive n ps state {trusted=[]}

---- answering the question: yes, no, ?yes, or ?no
doCommand (Answer Yes) n ps state =
    askQuestion n ps state	-- look for next sibling
doCommand (Answer No) n ps state = do
    let c = currentNode state
    p <- newParentSet c		-- descend into this subtree
    askQuestion c p state {treePath=(p:treePath state)}
{-
doCommand (Answer Yes) n ps state =
     do let child = head (children state)
        (b,q,state') <- interactive n ps state
				{ children = tail (children state)
				, recentNodes = addToRecentNodes
							(recentNodes state)
							child True
				, questnumber = questnumber state + 1
				, reconsider = False
				}
        if q/=questnumber state then return (b,q,state')
	  else interactive n ps state { options = state'
                                   , memoMode = memoMode state'
                                   , reconsider = False
                                   }
doCommand (Answer QueryYes) n ps state =
     do let child = head (children state)
        (b,q,state') <- interactive n ps state
				{ children  = tail (children state)
				, postponed = postponed state ++
						[(questnumber state,child)]
				, questnumber = questnumber state + 1
				, reconsider = False
				}
        if q/=questnumber state then return (b,q,state')
	  else interactive n state ps { options = options state'
                                   , memoMode = memoMode state'
                                   , reconsider = False
                                   }
doCommand (Answer No) n ps state =
     do let child = head (children state)
	    newchildren = edtChildren child
        (b,q,state') <- interactive n ps state
				{ children = newchildren
				, recentNodes = addToRecentNodes
							(recentNodes state)
							child False
				, postponed = []
				, questnumber = questnumber state + 1
				, reconsider = False
				}
        if q==questnumber state then
	       interactive n ps state { options = options state'
                                   , memoMode = memoMode state'
                                   , reconsider = False
                                   }
          else if b && q==0 then
             do let lmo = hatLeftmost child
		    src = if isInvalidNode lmo then HatNoSourceRef
                                               else hatSourceRef lmo
                putStrLn ("\nErroneous reduction:\n"
		          ++ highlight [Foreground Blue]
                                       (prettyEquation "" ""
                                                (screenWidth state)
                                                (options state)
                                                child)
                          ++ showExpression lmo
                                  "\nBug found within the body of function: \""
                          ++ "\"\n"
		          ++ "line "++show (row src)++", column "
			  ++ show (column src)
			  ++ " in module \"" ++ moduleName src
			  ++ "\", file: \"" ++ moduleFile src ++"\"")
		putStr ("\n:q to quit, any other key to go back to question "
                        ++ show (questnumber state) ++": ")
		cmd <- getCommand "hat-detect> "
		case cmd of
                    Quit -> return (False,-1,state')
                    _    -> interactive state
	  else return (b,q,state')

doCommand (Answer QueryNo) n ps state | not (reconsider state) =
     do let child = head (children state)
	    newchildren = edtChildren child
        (b,q,state') <- interactive n ps state
				{ children = newchildren
				, recentNodes = addToRecentNodes
							(recentNodes state)
							child False
				, postponed = []
				, questnumber = questnumber state + 1
				, reconsider = False
				}
        if q==questnumber state then
	       interactive n ps state { options = options state'
                                   , memoMode = memoMode state'
                                   , reconsider = False
                                   }
          else if b && q==0 then
	              interactive n ps state { options = options state'
                                          , trusted = trusted state'
                                          , memoMode = memoMode state'
                                          , reconsider = True
                                          }
	  else return (b,q,state')
doCommand (Answer QueryNo) n ps state | reconsider state =
     do putStrLn ("The question has already been deferred once.\n\
                  \You must answer it now with y/y?/n ")
        interactive n ps state
doCommand (AskQuestion q) n ps state =
        if q>0 && q<questnumber state then
	     return (False,q,state) -- return q, and ask question again
        else do putStrLn "No question with this number!"
	        interactive n ps state
----
-}
doCommand (StartTool (Observe q)) n ps state =
     do errcode <- system (hatObserve (filename state) q)
     	when (errcode/=ExitSuccess)
	     (putStrLn ("ERROR: Unable to start hat-observe.\n\
                        \Check settings and availability of hat-observe."))
        interactive n ps state
doCommand (StartTool Trail) n ps state =
     do errcode <- system (hatTrail (filename state) (currentNode state))
        when (errcode/=ExitSuccess)
             (putStrLn "ERROR: Unable to start hat-trail.\n\
	               \Check settings and availability of hat-trail.")
        interactive n ps state
doCommand (StartTool Anim) n ps state =
     do errcode <- system (hatAnim (filename state) (currentNode state))
        when (errcode/=ExitSuccess)
             (putStrLn "ERROR: Unable to start hat-anim.\n\
	               \Check settings and availability of hat-anim.")
        interactive n ps state
----
doCommand (Source q) n ps state =
     do let srcref = getSrcRef (currentNode state)
        when (srcref /= nil)
             (do let srcRef = SrcRef.readSrcRef srcref
                 system (hatView srcRef) -- (srcRefFile srcref)
                                        -- (srcRefLine srcref)
                                        -- (srcRefCol srcref))
                 return ())
        interactive n ps state
doCommand (Definition q) n ps state =
     do let defn = getDefnRef (currentNode state)
        when (defn /= nil)
             (do let defnRef = SrcRef.readSrcRef defn
                 ok <- doesFileExist (SrcRef.filename defnRef)
                 if not ok then putStrLn "defn source file not found"
                   else do system (hatView defnRef) -- (defnFile defn)
                                                  -- (defnLine defn)
                                                  -- (defnCol defn))
                           return ())
        interactive n ps state
----
doCommand _ n ps state =
     do putStrLn ("Unknown command.  "++shortHelpText)
        interactive n ps state
----


interactiveHelp s = do
    putStrLn (if null s then basicHelpText else detailedHelp s)
  where
    basicHelpText = "\
\------------- :help <cmd> for more detail ---------------------------------\n\
\ y   or  yes      you believe the equation is ok\n\
\ n   or  no       you believe the equation is wrong\n\
\ ?y  or  y?       you are not sure (but try ok for now)\n\
\ ?n  or  n?       you are not sure (but try wrong for now)\n\
\ <n>              go back to question <n>\n\
\ :set             show all current mode settings\n\
\ :set <flag>      change one mode setting (:help set for more detail)\n\
\ :observe <query> start hat-observe with the given query\n\
\ :trail           start hat-trail on the current equation\n\
\ :detect <n>      start another hat-detect on equation number <n>\n\
\ :animate <n>     start hat-anim on equation number <n>\n\
\ :source <n>      show the source application for equation <n>\n\
\ :Source <n>      show the source definition for identifier in equation <n>\n\
\ :trust           trust all applications of the current function\n\
\ :untrust         untrust ALL functions which were previously trusted\n\
\ :+[n]            short-cut to increase cutoff depth by <n> (default 1)\n\
\ :-[n]            short-cut to decrease cutoff depth by <n> (default 1)\n\
\ :resize          detect new window size for pretty-printing\n\
\ :help <cmd>  :?  show this help text (:help <cmd> for more detail)\n\
\ :quit            quit\n\
\---------------------------------------------------------------------------"

    detailedHelp s
      | ':' == head s  = detailedHelp (tail s)
      | s `isPrefixOf` "set" = "\
\ :set               show all current mode settings\n\
\ :set <flag>        change one mode setting\n\
\   <flag> can be: uneval [on|off]      show unevaluated expressions in full\n\
\                  strSugar [on|off]    sugar character strings\n\
\                  listSugar [on|off]   sugar lists\n\
\                  recursive [on|off]   show recursive calls\n\
\                  qualified [on|off]   show all identifiers qualified\n\
\                  equations [on|off]   show rhs of equations\n\
\                  [all|unique]         ask all questions or only unique q's\n\
\                  memoise [on|off]     memoise answers already given\n\
\                  cutoff <n>           cut-off depth for deeply nested exprs"
      | s `isPrefixOf` "detect"  = "\
\ :detect            start hat-detect on the current equation\n\
\    Start a fresh copy of hat-detect."
      | s `isPrefixOf` "animate"  = "\
\ :animate           start hat-anim on the current equation\n\
\    hat-anim is an interactive browser showing the forward reduction of an\n\
\    expression towards its final result."
      | s `isPrefixOf` "trail"  = "\
\ :trail <n>         start hat-trail browser on equation <n>\n\
\    hat-trail is an interactive browser that permits exploration backwards\n\
\    from a value, expression, or error message, through the function calls\n\
\    that ultimately led to the production of that value.\n\
\    When invoked from within hat-detect, hat-trail begins with the\n\
\    expression on the left of the equation numbered <n> in the list of\n\
\    equations shown by hat-detect."
      | s `isPrefixOf` "observe"  = "\
\ :observe <query>   start hat-observe in a new window with the new query\n\
\    With no argument, a new interactive hat-observe window is started for\n\
\    the same traced program.  Given a query argument, the new window starts\n\
\    with an immediate search for the expression pattern before giving\n\
\    an interactive prompt."
      | otherwise = " topic '"++s++"' has no further help text"


cmdlineHelp =
   "usage: hat-detect file-name\n"
   ++"       algorithmic debugging on a hat trace file"


data Cmd = Answer Answer | AskQuestion Int
         | Trust | Untrust | Children
         | Help String | Quit | Unknown
         | StartTool Tool | Source Int | Definition Int
         | Status | Set Mode | Shell String | Resize
data Answer = Yes | No | QueryYes | QueryNo
data Tool = Trail | Anim | Observe String | Detect Int
data Mode = O OptionCmd
          | Memoise Bool

getCommand :: String -> IO Cmd
getCommand prompt = do
    s <- cmdline prompt
    if null s then return Unknown
      else if all isDigit s then return (number AskQuestion [s] 0)
      else if head s `elem` "yYnN?mM" then return (answer (map toLower s))
      else if head s /= ':' then return Unknown
      else case words (tail s) of
          [] -> return Unknown
          (cmd:ss)
              | cmd `isPrefixOf` "quit" -> return Quit
              | cmd `isPrefixOf` "help" -> return (Help (unwords ss))
              | cmd `isPrefixOf` "observe" ->
				return (StartTool (Observe (unwords ss)))
              | cmd `isPrefixOf` "trail"   ->
				return (StartTool Trail)
              | cmd `isPrefixOf` "animate" ->
				return (StartTool Anim)
              | cmd `isPrefixOf` "detect"  ->
				return (number (StartTool . Detect) ss 0)
              | cmd `isPrefixOf` "trust"    -> return Trust
              | cmd `isPrefixOf` "untrust"  -> return Untrust
              | cmd `isPrefixOf` "children" -> return Children
              | cmd `isPrefixOf` "source"   -> return (number Source ss 0)
              | cmd `isPrefixOf` "Source"   -> return (number Definition ss 0)
              | cmd `isPrefixOf` "resize"   -> return Resize
              | cmd `isPrefixOf` "set" ->
                  case ss of
                    []      -> return Status
                    (m:sss) ->
                        case optionCmd ss of
                          Just o  -> return (Set (O o))
                          Nothing -> if m `isPrefixOf` "memoise"
                                     then return (maybe Unknown Set
                                                        (onOff Memoise sss))
                                     else return Unknown
              | head cmd == '?' -> return (Help (unwords (tail cmd:ss)))
              | head cmd == '+' ->
                          return (number (Set . O . Deeper) (tail cmd:ss) 1)
              | head cmd == '-' ->
                          return (number (Set . O . Shallower) (tail cmd:ss) 1)
              | head cmd == '!' -> return (Shell (unwords (tail cmd:ss)))
              | otherwise  ->  return Unknown


-- answer expects some partial checking of the string to have been done already
answer :: String -> Cmd
answer "y"   = Answer Yes
answer "n"   = Answer No
answer "yes" = Answer Yes
answer "no"  = Answer No
answer "y?"  = Answer QueryYes
answer "n?"  = Answer QueryNo
answer "?y"  = Answer QueryYes
answer "?n"  = Answer QueryNo
answer "?"   = Answer QueryNo
answer "maybe" = Answer QueryNo
answer _     = Unknown

