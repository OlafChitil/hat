-- hat-check
-- verify the correct format of a *.hat file and calculate some statistics
-- independent of other libraries for reading the *.hat file format
--
-- Haskell wrapper for hat-check which is implemented in C

module Main where

import System.Environment (getProgName, getArgs)
import Foreign.C.Types (CInt)
import Foreign.C.String (newCString, CString)
import Foreign.Ptr (Ptr)
import Foreign.Marshal (free)
import Foreign.Marshal.Array (withArray)

foreign import ccall "hat_check"
  hatCheck :: Int -> Ptr CString -> IO ()


main = do
  progName <- getProgName
  args <- getArgs
  listPtr <- mapM newCString (progName:args)
  withArray listPtr (hatCheck (length listPtr)) 
  mapM_ free listPtr