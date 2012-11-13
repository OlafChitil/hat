module Error where

import Control.Exception
import Data.List
import System.IO
import System.Exit

exit :: IO a
exit = exitWith (ExitFailure 1)

can'tOpen :: String -> IOException -> IO a
can'tOpen filename ioError =
  do
    hPutStr stderr ("Can't open "++filename ++ "\n")
    exit

errorStr :: String -> String -> String
errorStr filename msg = "In file "++filename++":\n"++msg ++ "\n"

can'tOpenStr :: String -> [String] -> a -> String
can'tOpenStr name [filename] ioerror =
   "Can't open "++ filename  ++ " when trying to read "++name++".\n"
can'tOpenStr name filename ioerror =
   "Can't open any of:\n "++ concatMap (++"\n ") (nub filename)
   ++ "when trying to read "++name++".\n"

errorMsg :: String -> String -> IO a
errorMsg filename msg =
  do
    hPutStr stderr  (errorStr filename msg)
    exit

can'tOpenAnyOf :: String -> [String] -> a -> IO b
can'tOpenAnyOf name filename ioError =
  do
    hPutStr stderr (can'tOpenStr name filename ioError)
    exit

errorLC :: Int -> Int -> String -> a
errorLC  l c msg =
  error ("Error at line "++show l ++", column " ++ show c ++ ": " ++ msg++"\n")

