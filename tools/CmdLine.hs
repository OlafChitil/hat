module CmdLine (initialize,cmdline) where

-- simple hack to use the following
import System.Console.Haskeline(InputT, runInputT, defaultSettings, getInputLine)

initialize :: IO ()
initialize = return ()

cmdline :: String -> IO String
cmdline prompt = runInputT defaultSettings getLine
  where
  getLine :: InputT IO String
  getLine = do
    minput <- getInputLine prompt
    case minput of
      Nothing -> return ""
      Just input -> return input

{-
#if USE_READLINE

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600
import System.Console.Readline as Readline (initialize, readline, addHistory)
#else
import Readline (initialize, readline, addHistory)
#endif
import Char	(isSpace)
import Monad	(when)

cmdline :: String -> IO String
cmdline prompt = do
    ms <- Readline.readline prompt
    case ms of
        Nothing -> return ""
        Just s  -> do when (not (all isSpace s)) (Readline.addHistory s) 
                      return s

#else

import IO (hFlush,stdout)

initialize :: IO ()
initialize = return ()

cmdline :: String -> IO String
cmdline prompt = do putStr prompt; hFlush stdout; getLine

#endif
-}