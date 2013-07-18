module System.Exit (  
    ExitCode(ExitSuccess, ExitFailure),  exitWith,  exitFailure,  exitSuccess  
  ) where

import PreludeBuiltinTypes
import SystemBuiltinTypes
import SystemBuiltin as NotHat.T
import qualified NotHat.System.Exit

foreign import ccall "NotHat.System.Exit.exitWith"
 exitWith :: ExitCode -> IO a
foreign import ccall "NotHat.System.Exit.exitFailure"
 exitFailure :: IO a
foreign import ccall "NotHat.System.Exit.exitSuccess"
 exitSuccess :: IO a
