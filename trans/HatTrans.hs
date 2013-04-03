{- ---------------------------------------------------------------------------
This `main' function is basically just the front-end of the nhc98
compiler.  It parses the .hs source file, creates the .hx file, and
then stops immediately after writing a new transformed .hs file.
-} 
module Main where

import System.Environment(getArgs)
import System.Console.GetOpt(getOpt,usageInfo,ArgOrder(..),OptDescr(..),ArgDescr(..))
import Language.Haskell.Exts.Annotated(ParseMode(..),ParseResult,fromParseResult,parseFileWithMode)
import Language.Haskell.Exts.Fixities(Fixity,preludeFixities)


main = do
  args <- getArgs
  let flagArgs = processArgs args

  {- parse source code -}
  let filePath = ?
  let parseMode = ParseMode {parseFilename = filePath
                            ,extensions = {}
                            ,ignoreLanguagePragmas = False
                            ,ignoreLinePragmas = True
                            ,fixities = Just preludeFixities} 
  moduleAST <- fromParseResult (parseFileWithMode parseMode filePath)
    
  {- Ensure we can write our output files. -}
  let hatDir = sSrcDir flags
  dir <- doesDirectoryExist hatDir
  when (not dir) (createDirectoriesRecursively hatDir)


{- If first argument is True, then print second and third with formatting -}
dumpIntermediate :: Bool -> String -> String -> IO ()
dumpIntermediate flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
    else return ()


{- End Module Main ----------------------------------------------------------}
