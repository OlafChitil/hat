module System ( 
     ExitCode(ExitSuccess,ExitFailure),
     getArgs, getProgName, getEnv, system, exitWith, exitFailure
   ) where

import PreludeBuiltinTypes as NotHat.T
import SystemBuiltinTypes as NotHat.T
import SystemBuiltin as NotHat.T
import qualified NotHat.System.Process as NotHat.Process
import qualified NotHat.System.Environment as NotHat.Environment
import qualified NotHat.System.Exit as NotHat.Exit

foreign import ccall "NotHat.Environment.getArgs"
 getArgs :: IO [String]
foreign import ccall "NotHat.Environment.getProgName"
 getProgName :: IO String
foreign import ccall "NotHat.Environment.getEnv"
 getEnv :: String -> IO String
foreign import ccall "NotHat.Process.system"
 system :: String -> IO ExitCode
foreign import ccall "NotHat.Exit.exitWith"
 exitWith :: ExitCode -> IO a
foreign import ccall "NotHat.Exit.exitFailure"
 exitFailure :: IO a