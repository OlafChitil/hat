module System ( 
     ExitCode(ExitSuccess,ExitFailure),
     getArgs, getProgName, getEnv, system, exitWith, exitFailure
   ) where

import PreludeBuiltinTypes
import SystemBuiltinTypes
import SystemBuiltin
import qualified NotHat.System.Cmd as NotHat.Cmd
import qualified NotHat.System.Environment as NotHat.Environment
import qualified NotHat.System.Exit as NotHat.Exit

foreign import haskell "Environment.getArgs"
 getArgs :: IO [String]
foreign import haskell "Environment.getProgName"
 getProgName :: IO String
foreign import haskell "Environment.getEnv"
 getEnv :: String -> IO String
foreign import haskell "Cmd.system"
 system :: String -> IO ExitCode
foreign import haskell "Exit.exitWith"
 exitWith :: ExitCode -> IO a
foreign import haskell "Exit.exitFailure"
 exitFailure :: IO a