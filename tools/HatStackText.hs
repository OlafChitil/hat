module Main where       -- HatStack main program

import LowLevel           (openHatFile,FileNode(..),nil,getParentNode
                          ,getErrorLoc,getErrorMessage
                          ,getSrcRef)
import SrcRef             (SrcRef(..),readSrcRef)
import SExp               (SExp(..),Label,fileNode2SExp,sExp2Doc,prune)
import PrettyLibHighlight (Doc,pretty,nest,text,(<>),parens)
import HighlightStyle     (getTerminalSize)
import System.Environment (getArgs,getProgName)
import System.Exit        (exitWith,ExitCode(..))
import Foreign.C.String   (withCString)
import System.IO          (hPutStrLn,stderr)
import Data.List          (isSuffixOf)
import Control.Monad      (when)

import Data.Maybe (isNothing)
import HatStack (hatStack)

main = do
    args    <- System.Environment.getArgs
    hatfile <- case args of (f:_) -> return (rectify f)
                            _     -> do hPutStrLn stderr
                                                  ("hat-stack: no trace file")
                                        exitWith (ExitFailure 1)
    stack <- hatStack hatfile
    when (isNothing stack)
         (do hPutStrLn stderr ("Tracefile \""++hatfile
                               ++"\" contains no reference to a program error.")
             exitWith (ExitFailure 1))
             
    let Just (errmsg, stck) = stack
    putStrLn ("Program terminated with error:\n\t"++errmsg)
    putStrLn ("Virtual stack trace:")
    (width,lines) <- getTerminalSize
    mapM_ (putStrLn . paint width) stck


rectify :: FilePath -> FilePath
rectify f | ".hat" `isSuffixOf` f = f
          | otherwise = f ++ ".hat"



paint :: Int -> (SExp Label, Maybe (String, Int)) -> String
paint width (sexp, srcpos) =
    let doc = sExp2Doc False True False (\_->id) (prune 10 sexp) in
        pretty width
               (parens (maybe (text "unknown")
                              (\(mod,line)-> text mod <> text ":"
                                             <> text (show line))
                              srcpos)
               <> text "\t" <> nest 3 doc)

