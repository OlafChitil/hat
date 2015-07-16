module Main where

-- System Imports
import System.IO     (stdin,stdout,stderr,hPutStrLn)
import Data.List     (isSuffixOf)
import System.Process (system)
import System.Environment (getArgs,getProgName)
import System.Exit   (exitWith,ExitCode(..))
import Foreign.C.String (withCString)
import Numeric       (showHex)

import LowLevel      (openHatFile,closeHatFile,FileNode(..),nil,peekTrace
                     ,getResult,getParentNode,getErrorLoc,getErrorMessage
                     ,getSrcRef,getDefnRef)
import NodeExp       (nodeExpForNode)
import Detect        (findMain)
import Pretty        (PrettyOption(..), makeGraph)

main = do args     <- System.Environment.getArgs
          prog     <- System.Environment.getProgName
          let opts = gatherOptions args
          let modNames = map getName (filter isModuleName opts)
          _ <- if modNames == []
                 then do hPutStrLn stderr (usage "no root module")
                         exitWith (ExitFailure 1)
                 else if ShowVersion `elem` opts
                   then do hPutStrLn stdout versionString
                   else return ()
          foldr (prettyFile prog opts) (return ()) modNames

prettyFile :: String -> [PrettyOption] -> String -> IO () -> IO ()
prettyFile prog opts x act =
  do act
     withCString prog (\p -> withCString (hatFile x) (openHatFile p))
     mainNode <- findMain
     writeFile (dotFile x) (makeGraph (nodeExpForNode mainNode) opts)
     closeHatFile

isModuleName :: PrettyOption -> Bool
isModuleName (ShowModule x) = True
isModuleName _ = False

getName :: PrettyOption -> String
getName (ShowModule x) = x

progName :: String
progName = "pretty-hat"

version :: Float
version = 0.1

versionString :: String
versionString = progName ++ " version: " ++ (show version) ++ "\n" ++
                "(c) 2005 Thomas Davie\n"

usage :: String -> String
usage err = progName ++ ": " ++ err ++ "\n" ++
            "usage: " ++ progName ++ " [MODULE]\n"

gatherOptions :: [String] -> [PrettyOption]
gatherOptions = map getOption

getOption :: String -> PrettyOption
getOption "-d" = ShowDecOffsets
getOption "-x" = ShowHexOffsets
getOption "-p" = ShowParents
getOption "-e" = MkEDT
getOption "-s" = ShowSrcPoses
getOption "-v" = ShowVersion
getOption x    = ShowModule x

dotFile :: FilePath -> FilePath
dotFile = (flip rectify) ".dot"

hatFile :: FilePath -> FilePath
hatFile = (flip rectify) ".hat"

rectify :: FilePath -> String -> FilePath
rectify f ext | ext `isSuffixOf` f = f
              | otherwise = f ++ ext
