module Main where

-- System Imports
import System.IO     (stdin,stdout,stderr,hPutStrLn)
import System.Cmd    (system)
import System.Environment (getArgs,getProgName)
import System.Exit   (exitWith,ExitCode(..))
import Control.Monad (when)
import Data.List     (isSuffixOf)

import CommonUI      (Options(..),hatDetect)
import Detect        (findMain)
import LowLevel      (openHatFile)
import Foreign.C.String (withCString)

main = do args    <- System.Environment.getArgs
          prog    <- System.Environment.getProgName
          modName <- case args of
                       ("-v":_) -> do hPutStrLn stdout versionString
                                      exitWith ExitSuccess
                       (f:_)    -> return f
                       _        -> do hPutStrLn stderr (usage "no root module")
                                      exitWith (ExitFailure 1)
          withCString prog (\p-> withCString (hatFile modName) (openHatFile p))
          main <- findMain
          errCode <- system $ hatDetect (hatFile modName) main
          checkOK errCode "hat-delta"

checkOK errcode s = when (errcode/=ExitSuccess)
                         (putStrLn ("ERROR: Unable to start "++s++"."))

progName :: String
progName = "hat-detect"

version :: Float
version = 1.0

versionString :: String
versionString = progName ++ " version: " ++ (show version) ++ "\n" ++
                "(c) 2005 Thomas Davie\n"

usage :: String -> String
usage err = progName ++ ": " ++ err ++ "\n" ++
            "usage: " ++ progName ++ " [MODULE]\n"

hatFile :: FilePath -> FilePath
hatFile = (flip rectify) ".hat"

rectify :: FilePath -> String -> FilePath
rectify f ext | ext `isSuffixOf` f = f
              | otherwise = f ++ ext
