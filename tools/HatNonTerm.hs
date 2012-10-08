module Main where

import NonTermLib
import LowLevel 
import SExp
import PrettyLibHighlight (Doc,pretty,nest,text,(<>))
import qualified PrettyLibHighlight as Pretty (highlight)
import HighlightStyle     (goto,cls,clearDown,clearUp,cleareol,highlightOff
                          ,highlight,Highlight(..),Colour(..)
                          ,enableScrollRegion,getTerminalSize
                          ,savePosition,restorePosition)
import Control.Monad      (when)
import System.Cmd         (system)
import System.Environment (getArgs,getProgName,getEnv)
import System.Exit        (exitWith,ExitCode(..))
import Data.List          (isPrefixOf,isSuffixOf,group,groupBy)
import System.IO          (hSetBuffering,BufferMode(..),stdin,stdout,stderr
                          ,hPutStrLn,hFlush)
import Foreign.C.String   (withCString)


main = 
  do args    <- System.Environment.getArgs
     prog    <- System.Environment.getProgName
     initialiseCount
     checkHelp args
     hatname <- getHatName args
     hatfile <- return (rectify hatname)
     withCString prog (\p-> withCString hatfile (openHatFile p))
     errloc <- getErrorLoc
     errmsg <- getErrorMessage
     checkError errmsg
     (columns, lines) <- getTerminalSize
     let state = (setOptions args) { progname=hatname, file=hatfile 
                                    ,width=columns , height=lines } 
     putStrLn ("---- Hat Non-Term: " ++ hatname ++ " ----")
     showFunc state
     showPath state
     if (showCount state) then  
       putStrLn ("no. reads from file: " ++ (show getCount) )
       else return ()
  where
    -- find the suspicious function
    targetfunc = greatestDist getRootNode

    -- show the suspicious function. 
    -- this is separated from showing the path, because the program
    -- might not find a path at all
    showFunc state 
      | targetfunc < (FileNode 4) = do putStrLn "hat-nonterm: no function found"
                                       exitWith (ExitFailure 1)
      | otherwise                 = putStrLn ("suspicious function is " 
                                       ++ getFuncName targetfunc 
                                       ++ " in module " 
				       ++ getFuncModule targetfunc)

    -- show the non-termination path, if one can be found
    showPath state 
        | funcpath == []   = do putStrLn ("hat-nonterm: no path found") 
                                exitWith (ExitFailure 1)
        | (showNode state) = do mapIO putStrLn
	                          (inter (map (showSctx state) funcpath) 
	    		                 (map show funcpath) )
        | otherwise        = do mapIO (putStrLn . showSctx state ) funcpath 
      where funcpath = nAppSearch state getRootNode targetfunc

    rectify :: FilePath -> FilePath
    rectify f | ".hat" `isSuffixOf` f = f
              | otherwise = f ++ ".hat"

    checkError :: String -> IO ()
    checkError msg | msg `isPrefixOf` "Interrupted (^C)"= return ()
                   | otherwise = 
		      do hPutStrLn stderr ("hat-nonterm: not interrupted") 
		         exitWith (ExitFailure 1)

    checkHelp args | member "--help" args = do showHelp
                                               exitWith ExitSuccess
                   | otherwise            = return ()

    getHatName args = 
       case args of (f:_) -> return f
                    _     -> do hPutStrLn stderr ("hat-nonterm: no trace file")
		                showHelpShort
                                exitWith (ExitFailure 1)

    showHelpShort = do putStrLn "Usage: hat-nonterm [PROG] [OPTIONS]..."
    showHelp = 
      do showHelpShort
         putStrLn "  --showqual={t,f}     Show function module names"
	 putStrLn "  --showrt={t,f}       Show all nodes from the root node"
	 putStrLn "  --srcref={t,f}       Show source references for nodes"
         putStrLn "  --cutoff=<int>       Cutoff expression at depth <int>"
         putStrLn "  --numfn=<int>        Show <int> instances of the function" 


-- This function calls the search function, and cuts off the head of the
-- path if the correct option is set
nAppSearch :: State -> FileNode -> FileNode -> [Sctx]
nAppSearch state startnode func 
    | (showRoot state) = reverse (map reverse resultpath)
    | otherwise        = cutAt func (reverse (map reverse resultpath))
  where 
    resultpath = nAppSearchAux [startnode] (numFn state) func []
    
    cutAt _ [] = [] 
    cutAt func (x:xs) | funcAppMember func x = (x:xs)
                      | otherwise            = cutAt func xs

    funcAppMember _ [] = False
    funcAppMember func (x:xs) 
        | nodeType x == ExpApp 
         && getFuncPtr x == func = True
        | otherwise              = funcAppMember func xs


-- This function searches through the ART graph for a path containing N 
-- applications of a particular function. It implements the search process
-- for a non-termination path described in the report
--
-- It works as follows, starting from the root node:
--    * if a node is of the wrong kind (ie SrcRef etc.) the function returns 
--      an empty search path, indicating failure
--    * if the current node is a call to the required function, *and* the 
--      counter indicates that only one more instance needs to be found, 
--      return the search path
--    * otherwise, call the function on the node result. If this returns a
--      path, return it.
--    * otherwise, do the same for the nodes children
--    * otherwise, return an empty path
nAppSearchAux :: Sctx -> Int -> FileNode -> [Sctx] -> [Sctx]
nAppSearchAux curr@(currnode:stack) n func path
    | haltAtNode currnode    = []
    | nodeistarget && n == 1 = (curr:path)
    | resultpath /= []       = resultpath
    | otherwise              = searchChildren (getNodeChildren currnode)
  where
    nodepeek = peekResultMod (getResultHT currnode True)
    nodeistarget = (nodeType currnode) == ExpApp 
                    && (getFuncPtr currnode) == func 
		    && nodepeek == interrupted
    
    -- call the function on the node result, remembering to decrement the 
    -- counter if the current node is an call to the target function
    resultnode = peekResultMod currnode
    resultsctx = [resultnode]
    resultpath | resultnode == currnode = []
               | nodeistarget = nAppSearchAux resultsctx (n-1) func (curr:path)
               | otherwise    = nAppSearchAux resultsctx n func (curr:path)

    -- as above, for the node's children
    searchChildren [] = []
    searchChildren (ch:rest) 
        | haltAtNode ch                      = searchChildren rest
        | nodeistarget && childpathapp /= [] = childpathapp
        | childpathnoapp /= []               = childpathnoapp
        | otherwise                          = searchChildren rest
      where 
        chfunc = getFuncPtr ch
	chsctx = (ch:currnode:stack)
        childpathapp = nAppSearchAux chsctx (n-1) func path 
	childpathnoapp = nAppSearchAux chsctx n func path 


-- Check if its ok to keep following this node path
haltAtNode node | node <= (FileNode 4)		= True
                | ntype == ExpApp		= False
                | ntype == ExpValueApp		= False
                | ntype == ExpValueUse		= False
                | ntype == ExpConstUse		= False
                | ntype == ExpConstDef		= False
                | ntype == ExpGuard		= False
                | ntype == ExpCase		= False
                | ntype == ExpIf		= False
                | ntype == ExpForward		= False
                | ntype == ExpProjection	= False
                | ntype == Module		= True
                | ntype == SrcPos		= True
                | ntype == ExpChar		= True
                | ntype == ExpInt 		= True
                | ntype == ExpInteger		= True
                | ntype == ExpRat		= True
                | ntype == ExpRational		= True
                | ntype == ExpFloat		= True
                | ntype == ExpDouble		= True
                | ntype == ExpFieldUpdate	= True
                | ntype == ExpHidden		= True
                | ntype == ExpDoStmt		= True
                | ntype == AtomVariable		= True
                | ntype == AtomConstructor	= True
                | ntype == AtomAbstract		= True
  where ntype = nodeType node


-- A list of Function applications, along with the first and last
-- times that they appear with an interrupted result
type FnDist = (FileNode, FileNode, FileNode)

-- find the distance between two instances of a function
fnDist :: FileNode -> FileNode -> Int
fnDist (FileNode x) (FileNode y) | x < y     = y - x 
                                 | otherwise = x - y

-- This function finds the function, with the two furthest-apart
-- interrupted instances. It first calls firstLastApp to build a list of
-- *all* interrupted functions distances. It then uses foldr to find the
-- one with the greatest distance, and returns a pointer to it
greatestDist :: FileNode -> FileNode
greatestDist startnode = func
  where (func, _, _) = (foldr maxdist (nil,nil,nil) fstlast)
        fstlast = firstLastApp startnode 
        maxdist (fn1, s1, e1) (fn2, s2, e2)
          | (fnDist s1 e1) >= (fnDist s2 e2) = (fn1, s1, e1) 
          | otherwise                       = (fn2, s2, e2)
	

-- This function builds a list of functions that were interrupted,
-- along with pointers to their first and last applications It scans
-- through the ART file in a linear manner, using nextFileNode to get
-- the following node for a given node. If the node corresponds to an
-- interrputed function, it tries to add it to the list of functions,
-- depending on whether the instance is either the earliest or the
-- latest found so far.
firstLastApp :: FileNode -> [FnDist]
firstLastApp node 
   | next == nil             = []
   | nodeType node == ExpApp
    && peek == interrupted   = addApp (getFuncPtr node) node (firstLastApp next)
   | otherwise               = firstLastApp next
  where 
    next = nextFileNode node
    result = (getResultHT node True)
    peek = peekResultMod result

    addApp :: FileNode -> FileNode -> [FnDist] -> [FnDist]
    addApp fn app [] = [(fn, app, app)]
    addApp fn app ((x, fst, last):xs) 
      | fn == x && app < fst   = ((x, app, last):xs) 
      | fn == x && app > last  = ((x, fst, app):xs)
      | otherwise              = ((x, fst, last):(addApp fn app xs))

