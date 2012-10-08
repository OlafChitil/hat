module System ( 
     ExitCode(ExitSuccess,ExitFailure),
     getArgs, getProgName, getEnv, system, exitWith, exitFailure
   ) where

import PreludeBuiltinTypes
import SystemBuiltinTypes
import SystemBuiltin
import qualified TraceOrigSystem.Cmd as TraceOrigCmd
import qualified TraceOrigSystem.Environment as TraceOrigEnvironment
import qualified TraceOrigSystem.Exit as TraceOrigExit

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