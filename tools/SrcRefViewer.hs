-- Simple program to display a src reference.
-- Reports the filename and location, and shows a section of the file
-- with the cursor on the actual location.  We try to place the
-- important line as near the centre of the screen as possible.

module Main where

import System.Environment (getProgName, getArgs)
import System.Cmd     (system)
import System.Exit    (exitWith, ExitCode(..))
import System.IO      (hSetBuffering,BufferMode(..),stdin,stdout)
import Data.Char      (isSpace,isDigit)
import Run            (runAndReadStdout)
import HighlightStyle (getTerminalSize,cls,goto,highlight,lineWrap
                      ,Highlight(..),Colour(..))

main = do
    args <- System.Environment.getArgs
    case args of
      [filename,sline,scol] ->
              let line = (read sline)::Int
                  column = (read scol)::Int in
              display filename line column 0 0
      [filename,sline,scol,sline2,scol2] ->
              let line = (read sline)::Int
                  column = (read scol)::Int
                  endline = (read sline2)::Int
                  endcolumn = (read scol2)::Int in
              display filename line column endline endcolumn
      _ -> do prog <- System.Environment.getProgName
              putStrLn ("Usage: "++prog++" srcfile line col [endline endcol]")
              exitWith (ExitFailure 1)

display :: FilePath -> Int -> Int -> Int -> Int -> IO ()
display srcfile line column endline endcolumn = do
    len <- runAndReadStdout ("wc -l "++srcfile)
    let n = read (takeWhile isDigit (dropWhile isSpace len))
    case n of
      0 -> do putStrLn ("File "++srcfile++" not found.")
              exitWith (ExitFailure 1)
      _ -> do (width,height) <- getTerminalSize
              f <- readFile srcfile
              hSetBuffering stdin NoBuffering
              hSetBuffering stdout NoBuffering
              let middle= (height `div` 2) - 1
                  trim  = if line < middle then 0 else line - middle
                  fs    = (expandTabs . unlines . take (height-2)
                          . drop trim . lines) f
                  line' = line-trim
                  (a,b,c) = split (line',column,endline-trim,endcolumn) fs
              putStr (cls ++ goto 1 1 ++ lineWrap False)
              putStr (highlight [Bold] ("---- "++srcfile++" ---- line: "
                                       ++show line++" ---- column: "
                                       ++show column++" ----"))
              if endline==0
                then putStr (goto 1 2 ++ fs ++ goto column (line'+1))
                else putStr (goto 1 2 ++ a
                             ++ highlight [Bold, Foreground Magenta] b ++ c
                             ++ goto column (line'+1))
              System.Cmd.system ("stty -icanon min 1 -echo")
              awaitQuit
              System.Cmd.system ("stty icanon echo")
              putStr (goto 1 height)
              return ()

split :: (Int,Int,Int,Int) -> String -> (String,String,String)
split (sl,sc,el,ec) str
  | sl==el    =
    let (a,b) = splitAt (el-1) (lines str)
        (c,d) = splitAt ec (head b)
        (e,f) = splitAt (sc-1) c
    in (unlines a++e, f, unlines (d:tail b))
  | otherwise =
    let ls = lines str
        (a,b)   = splitAt (el-1) (lines str)
        (a',b') = splitAt  ec    (head b)
        (p,q)   = splitAt (sl-1) a
        (p',q') = splitAt (sc-1) (head q)
    in (unlines p++p', unlines (q':tail q)++a', unlines (b':tail b))

awaitQuit :: IO ()
awaitQuit = do
  q <- getChar
  case q of
    'q' -> return ()
    'x' -> return ()
    _   -> awaitQuit

expandTabs :: String -> String
expandTabs = expand 0
  where expand n []         = []
        expand n ('\n':xs)  = '\n': expand 0 xs
        expand 8 ('\t':xs)  = replicate 8     ' ' ++ expand 0 xs
        expand n ('\t':xs)  = replicate (8-n) ' ' ++ expand 0 xs
        expand 8 (x:xs)     = x : expand 1 xs
        expand n (x:xs)     = x : expand (n+1) xs
