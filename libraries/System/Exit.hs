module System.Exit (  
    ExitCode(ExitSuccess, ExitFailure),  exitWith,  exitFailure,  exitSuccess  
  ) where

import PreludeBuiltinTypes
import SystemBuiltinTypes
import SystemBuiltin
import qualified NotHat.System.Exit

foreign import haskell "System.Exit.exitWith"
 exitWith :: ExitCode -> IO a
foreign import haskell "System.Exit.exitFailure"
 exitFailure :: IO a
foreign import haskell "System.Exit.exitSuccess"
 exitSuccess :: IO a
