module Main where       -- HatAnim main program

import Data.Char          (toLower)
import LowLevel           (openHatFile,getBridgeValue,hatVersionNumber
                          ,FileNode(..),nil,peekTrace,getResult,getParentNode
                          ,getErrorLoc,getErrorMessage
                          ,getSrcRef,getDefnRef)
import NodeExp            (NodeExp(..),nodeExpForNode
                          ,compressClosures,removeResultCycles
                          ,removeNonResultCycles,limitDepth,hideFunction
                          ,fullEval,condEval,nodeExp2SExp
                          ,singleStepEval,getNode,(===))
import SExp               (SExp(..),prettySExp,optParens)
import HighlightStyle     (goto,cls,clearDown,clearUp,cleareol,highlightOff
                          ,highlight,Highlight(..),Colour(..)
                          ,enableScrollRegion,getTerminalSize
                          ,savePosition,restorePosition)
import System.Process     (system)
import System.Environment (getArgs,getProgName,getEnv)
import System.Exit        (exitWith,ExitCode(..))
import Data.List          (isPrefixOf,isSuffixOf,group,groupBy)
import System.IO          (hSetBuffering,BufferMode(..),stdin,stdout,stderr
                          ,hPutStrLn,hFlush)
import Data.Char          (digitToInt)
import Foreign.C.String   (withCString)
import Numeric            (showHex)
import CommonUI           (hatTrail,hatObserve,hatDetect,hatAnim,hatView
                          ,hatExplore,Keep(..)
                          ,Options(..),initialOptions
                          ,OptionCmd(..),optionCmd,onOff,number
                          ,optionsUpdate,showOption,showOnOff)
import Pretty             (PrettyOption(..),makeGraph)

data AnimState = AnimState {size :: (Int,Int),depth :: Int
                           ,hides :: [String],colouring :: Bool
                           ,rowsDisplayed :: Int,untouched :: NodeExp
                           ,command :: String}

main = do
    args    <- System.Environment.getArgs
    prog    <- System.Environment.getProgName
    hatfile <- case args of (f:_) -> return (rectify f)
                            _     -> do hPutStrLn stderr
                                                  ("hat-anim: no trace file")
                                        exitWith (ExitFailure 1)
    withCString prog (\p-> withCString hatfile (openHatFile p))
    errloc  <- getErrorLoc
    errmsg  <- getErrorMessage
    output  <- readOutputFile hatfile
    bridge  <- readBridgeFile
    let (start,end) = case args of
                       (_:s:e:_) -> (atoi s, atoi e)
                       (_:s:_)   -> (atoi s, 0)
                       _         -> (0,0)
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    system ("stty -icanon min 1 -echo")
    (columns,lines) <- getTerminalSize
    putStr (show start)
    let startExp = (removeNonResultCycles
                     (removeResultCycles
                       (nodeExpForNode
                         (FileNode start))))
    animLoop AnimState{size=(columns,lines),depth=10,hides=[],colouring=True
                      ,rowsDisplayed=1,untouched=startExp,command=""}

animLoop :: AnimState -> IO ()
animLoop state =
  animLoopRedraw state True
  where
    animLoopRedraw :: AnimState -> Bool -> IO ()
    animLoopRedraw state@AnimState{size=(cols,lines),rowsDisplayed=rows
                                  ,command=com}
                   fullDraw =
      do
        let
          animation :: String
          animation = if fullDraw then
                         interface (cols,lines)
                         ++ (goto 1 5)
                         ++ animText state
                      else ""
        putStr animation
        putStr (goto 1 lines ++ com ++ take (cols - length com) (repeat ' '))
        x <- getChar
        if x == '\n'
          then runCommand state
          else if x == '\b' then
            if com == "" then
              animLoopRedraw state{rowsDisplayed=if rows > 1 then rows-1
                                                             else rows}
                             True
            else
              animLoopRedraw state{command=removeLast com} False
          else
            animLoopRedraw state{command=com ++ [x]} False

animText :: AnimState -> String
animText AnimState{size=(cols,lines),depth=depth,hides=hides,colouring=colouring
                  ,rowsDisplayed=rows,command=com,untouched=untouched} =
  unlines $! (map indent text)
  where
    text :: [String]
    text = fittingLines outputLines

    fittingLines :: [String] -> [String]
    fittingLines lns
      = drop ((length lns) - numLines) revLines
        where
          revLines = (reverse lns)
          numLines = countLines 0 lns
          countLines :: Int -> [String] -> Int
          countLines l [] = 0
          countLines l (y:ys)
            = if (takenLines y) > (lines - l - 6) then 0
              else (countLines (l + takenLines y) ys) + 1
          takenLines :: String -> Int
          takenLines = (+1) . length . (filter (=='\n'))

    outputLines :: [String]
    outputLines =   reverse
                  $ take rows
                  $ map pretty
                  $ exps hides $  foldr hideFunction untouched hides
        
    pretty :: NodeExp -> String
    pretty exp = 
      prettySExp ""
                 (cols - 3)
                 Options {listSugar=True,showQual=False,colourBracks=colouring
                         ,cutoffDepth=depth,unevalMode=True,stringSugar=True
                         ,equations=False,filterMode=Unique,recursiveMode=True}
                   (nodeExp2SExp
                     (limitDepth depth exp))
    
exps :: [String] -> NodeExp -> [NodeExp]
exps h e = e:if ns === e then [] else exps h ns
           where
             ns = foldr hideFunction (singleStepEval e) h

indent :: String -> String
indent xs = "-> " ++ (indAux xs)
            where
              indAux :: String -> String
              indAux [] = []
              indAux ('\n':xs) = "\n   " ++ (indAux xs)
              indAux (x:xs) = x:(indAux xs)

runCommand :: AnimState -> IO ()
runCommand state@AnimState{command = com} =
  getCommand command args state
  where
    (command, args) =
      case wrds of 
        []        -> ("", [])
        otherwise -> (head wrds, tail wrds)
    wrds :: [String]
    wrds = words com

getCommand :: String -> [String] -> AnimState -> IO()
getCommand "" _ state@AnimState{rowsDisplayed=rows,untouched=untouched
                               ,hides=hides} =
  if length (exps hides (foldr hideFunction untouched hides)) == rows
    then animLoop state{rowsDisplayed=rows,command = ""}
    else animLoop state{rowsDisplayed=rows+1,command = ""}

getCommand ":q" [] _ =
  exitWith ExitSuccess

getCommand ":r" _ state =
  do (newCols,newLines) <- getTerminalSize
     animLoop state{size=(newCols,newLines),command = ""}
              
getCommand ":d" [] state@AnimState{size=(cols,lines),depth=depth} =
  do putStr (goto 1 lines ++ "depth: " ++ (show depth))
     getChar
     animLoop state{command = ""}

getCommand ":d" [newDepth] state@AnimState{size=(cols,lines)} =
  if isInteger newDepth
  then
    animLoop state{depth=(atoi newDepth),command = ""}
  else
    do invalid (cols,lines)
       animLoop state{command = ""}

getCommand ":h" [] state@AnimState{size=(cols,lines),hides=hides} =
  do putStr (goto 1 4 ++ clearDown)
     putStr (goto 1 4
            ++ "Hidden items: "
            ++ (take (cols - 15) (repeat '-')))
     putStr (goto 1 5 ++ (unlines hides))
     getChar
     animLoop state{command = ""}

getCommand ":h" args state@AnimState{hides=hides} =
  let newHides = (hides ++ args)
  in animLoop state{hides = newHides,command = ""}

getCommand ":uh" args state@AnimState{hides=hides} =
  let newHides = (removeAll args hides)
  in animLoop state{hides=newHides,command=""}

getCommand ":?" _ state@AnimState{size=(cols,lines)} =
  do putStr (goto 1 4 ++ clearDown)
     putStr (goto 1 4
            ++ "Help: "
            ++ (take (cols - 5) (repeat '-')))
     putStr (goto 1 5 ++
            "Press return to advance one reduction step, and backspace to go back a reduction step.\n" ++
            (highlight [Bold, Foreground Blue] "Commands:\n") ++
            ":r     Resize the display.\n" ++
            ":d     Display expression cutoff depth.\n" ++
            ":d n   Set cutoff depth to n.\n" ++
            ":h     Display all hidden expressions.\n"  ++
            ":h x*  Hide application of all functions listed.\n" ++
            ":uh x* Unhide application of all functions listed.\n" ++
            ":c     Toggle bracket colouring.\n" ++
            ":q     Exit hat-anim")
     getChar
     animLoop state{command = ""}

getCommand ":c" [] state@AnimState{colouring=colouring} =
  animLoop state{colouring = not colouring, command = ""}

getCommand _ _ state@AnimState{size = (cols,lines)} =
  do invalid (cols,lines)
     animLoop state{command = ""}

invalid :: (Int,Int) -> IO ()
invalid (_,lines) =
  do putStr (goto 1 lines ++ "INVALID COMMAND")
     getChar
     putStr ""

interface :: (Int,Int) -> String
interface (cols,lines) =
    (goto 1 1 ++ clearDown)
    ++ (goto 1 1 ++ "Output: " ++ take (cols - 8) (repeat '-'))
    ++ (goto 1 4 ++ "Animation: " ++ take (cols - 11) (repeat '-'))
    ++ (goto 1 (lines - 1) ++ take cols (repeat '-'))

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x:(removeLast xs)

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

rectify :: FilePath -> FilePath
rectify f | ".hat" `isSuffixOf` f = f
          | otherwise = f ++ ".hat"

readOutputFile :: FilePath -> IO String
readOutputFile hat = do readFile (hat++".output")

readBridgeFile :: IO [FileNode]
readBridgeFile = do until (==nil) getBridgeValue
      where
        until :: (a->Bool) -> IO a -> IO [a]
        until pred action = do
          v <- action
          if pred v then return [] else do vs <- until pred action
                                           return (v:vs)

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = foldr remove ys xs

stripWhiteSpace :: String -> String
stripWhiteSpace (' ':xs) = xs
stripWhiteSpace x = x

isInteger :: String -> Bool
isInteger [] = True
isInteger (x:xs) = isDigit x && isInteger xs

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

atoi :: String -> Int
atoi [] = 0
atoi (x:xs) = (digitToInt x) * (10 ^ length xs) + atoi xs