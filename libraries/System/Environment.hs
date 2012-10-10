module System.Environment (  
    getArgs,  getProgName,  getEnv  
  ) where

import PreludeBuiltinTypes
import IOBuiltin
import qualified NotHat.System.Environment

foreign import haskell "System.Environment.getArgs"
  getArgs :: IO [String]

foreign import haskell "System.Environment.getProgName"
  getProgName :: IO String

foreign import haskell "System.Environment.getEnv"
  getEnv :: String -> IO String
