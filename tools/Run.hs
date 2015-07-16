module Run
  ( runAndReadStdout
  , tmpfile
  ) where

import System.Directory (removeFile)
import System.Process   (system)
import System.Exit      (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)

-- #ifdef __HBC__
-- import UnsafePerformIO
-- #ifdef __HASKELL98__
-- import GetPid
-- getProcessID = getPid
-- #else
-- getProcessID = return 3154      -- arbitrary number
-- #endif
-- #endif
-- #ifdef __NHC__
-- import FFIExtensions (unsafePerformIO)
-- foreign import ccall "getpid" getProcessID :: IO Int
-- #endif
-- #ifdef __GLASGOW_HASKELL__
-- import FFIExtensions (unsafePerformIO)
-- foreign import ccall "getpid" getProcessID :: IO Int
-- #endif
foreign import ccall "getpid" getProcessID :: IO Int

-- Generate a temporary filename unique to this process.
tmpfile :: String -> String
tmpfile root = unsafePerformIO $ do p <- getProcessID
                                    return ("/tmp/"++root++"."++show p)

-- Run a shell command and collect its output.
runAndReadStdout :: String -> IO String
runAndReadStdout cmd = do
    let output = tmpfile "hmakeconfig"
    err <- system (cmd++" >"++output)
    case err of
        ExitFailure _ -> ioError (userError ("Command ("++cmd++") failed"))
        _ -> return ()
    s <- readFile output
    removeFile output   -- file will not be removed until readFile closes it
    return (safeinit s) -- strip trailing newline added by shell
  where
    safeinit []     = []
    safeinit ['\n'] = []
    safeinit [x]    = [x]
    safeinit (x:xs) = x : safeinit xs

