module System.Environment (  
    getArgs,  getProgName,  getEnv  
  ) where

import PreludeBuiltinTypes
import IOBuiltin
import qualified NotHat.System.Environment

foreign import ccall "NotHat.System.Environment.getArgs"
  getArgs :: IO [String]

foreign import ccall "NotHat.System.Environment.getProgName"
  getProgName :: IO String

foreign import ccall "NotHat.System.Environment.getEnv"
  getEnv :: String -> IO String
