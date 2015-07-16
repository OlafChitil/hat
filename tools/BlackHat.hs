module Main where

import NonTermLib
import LowLevel 
import SExp
import System.Process     (system)
import System.Environment (getArgs,getProgName,getEnv)
import System.Exit        (exitWith,ExitCode(..))
import Data.List          (isPrefixOf,isSuffixOf,group,groupBy)
import Control.Monad      (when)
import System.IO          (hSetBuffering,BufferMode(..),stdin,stdout,stderr
                          ,hPutStrLn,hFlush)
import Foreign.C.String   (withCString)
import HighlightStyle     (getTerminalSize)


main = 
  do args    <- System.Environment.getArgs
     prog    <- System.Environment.getProgName
     initialiseCount
     checkHelp args
     hatname <- getHatName args
     hatfile <- return (rectify hatname)
     withCString prog (\p-> withCString hatfile (openHatFile p))
     errmsg <- getErrorMessage
     (columns, lines) <- getTerminalSize
     let state = (setOptions args) { progname=hatname, file=hatfile
                                    , width=columns , height=lines }
     checkError state errmsg
     putStr ("---- Black Hat: " ++ hatname ++ " ----\n")
     showPath state
     if (showCount state) then  
       putStrLn ("No. reads from file: " ++ (show getCount) )
       else return ()
  where
    showPath state
        | bhpath == []     = do putStr (progname state)
	                        putStrLn (": no black hole found") 
                                exitWith (ExitFailure 1)
	| (showNode state) = mapIO putStrLn
	                       (inter (map (showSctx state) bhpath) 
			              (map show bhpath) )
        | otherwise = mapIO (putStrLn . showSctx state ) bhpath 
      where bhpath = blackHoleSearch state getRootNode

    rectify :: FilePath -> FilePath
    rectify f | ".hat" `isSuffixOf` f = f
              | otherwise = f ++ ".hat"

    checkError :: State -> String -> IO ()
    checkError state msg  
      | msg `isPrefixOf` "<<loop>>" = return ()
      | otherwise = do hPutStrLn stderr ((progname state) 
                                         ++": error is not a loop") 
		       exitWith (ExitFailure 1)

    checkHelp args | member "--help" args = do showHelp
                                               exitWith ExitSuccess
                   | otherwise            = return ()

    getHatName args = 
       case args of (f:_) -> return f
                    _     -> do hPutStrLn stderr 
		                         ("black-hat: no trace file")
		                showHelpShort
                                exitWith (ExitFailure 1)

    showHelpShort = putStrLn "Usage: black-hat [PROGNAME] [OPTIONS]..."
    showHelp =
      do showHelpShort
         putStrLn "  --showqual={t,f}     Show function module names"
	 putStrLn "  --showrt={t,f}       Show all nodes from the root node" 
	 putStrLn "  --srcref={t,f}       Show source references for nodes"
         putStrLn "  --cutoff=<int>       Cutoff expression at depth <int>"


-- This function calls the function which implements the search process
-- proper. It also cuts off the head of the list, if the correct option 
-- is set.
blackHoleSearch :: State -> FileNode -> [Sctx]
blackHoleSearch state startnode 
    | (showRoot state) = reverse (map reverse resultpath)
    | otherwise        = cutAt node (reverse (map reverse resultpath))
  where 
    (node, resultpath) = blackHoleSearchAux state [startnode] []
    
    cutAt _ [] = []                           -- shouldn't happen
    cutAt func (x:xs) | member node x = (x:xs)
                      | otherwise     = cutAt func xs


-- This function looks for a black-hole path in the ART graph. It implements
-- the search process documented in the report. Basically, the function uses 
-- a depth-first search of the ART graph.
-- 
-- From the root node, each node is considered in turn.
--   * if it is the wrong kind of node (ie a Module, SrcPos) the function
--     returns (nil, []), indicating failure
--   * if it has already appeared on the search path, the search returns the
--     search path, and the head node, indicating success
--   * otherwise, the function calls itself on the node's result. If
--     any of them return a search path, indicating success, this path 
--     is returned
--   * otherwise, the function calls itself on the node's children, as
--     above
--   * otherwise, the function returns (nil, []) indicating failure
blackHoleSearchAux :: State -> Sctx -> [Sctx] -> (FileNode, [Sctx])
blackHoleSearchAux state curr@(currnode:stack) path
    | haltAtNode currnode      = (nil, [])
    | nodeInPath currnode path = (currnode, (curr:path))
    | resultpath /= []         = (resultnode, resultpath)
    | otherwise                = searchChildren (getNodeChildren currnode)
  where
    nodeInPath currnode []     = False
    nodeInPath currnode (x:xs) 
      | incMember currnode x = True
      | otherwise            = nodeInPath currnode xs
    
    incMember m [] = False
    incMember m (y:ys) | (showCount state) && 
                         incrementCount 1 && m == y = True
		       | m == y                     = True
                       | otherwise                  = incMember m ys
    
    -- call the function on the node's result
    resultsctx = [(peekResultMod currnode)]
    (resultnode, resultpath) 
      | (peekResult currnode) == currnode = (nil, [])
      | otherwise              = blackHoleSearchAux state resultsctx (curr:path)

    -- call the function on all of the node's children
    searchChildren [] = (nil, [])
    searchChildren (ch:rest) | childpath /= [] = (childnode, childpath)
                             | otherwise       = searchChildren rest
      where 
	(childnode, childpath) = blackHoleSearchAux state chsctx path 
	chsctx = (ch:currnode:stack)


-- This function indicates if the search process should fail and
-- backtrack when it reaches a node.
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
