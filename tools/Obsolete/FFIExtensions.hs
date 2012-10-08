module FFIExtensions
#if defined(__GLASGOW_HASKELL__)
  ( unsafePerformIO
  , module Foreign
  , module CString
#elif defined(__NHC__)
#if __NHC__ >= 115
  ( module NHC.FFI
#else
  ( module FFI
#endif
#endif
  , showHex
  )
  where

#if defined(__GLASGOW_HASKELL__)
import Foreign
import CString
#if __GLASGOW_HASKELL__ < 504
import IOExts   (unsafePerformIO)
import Numeric (showInt)
showHex :: (Integral a) => a -> String -> String
showHex = showInt	-- poor approximation
#else
import System.IO.Unsafe   (unsafePerformIO)
import Numeric (showHex)
#endif
#elif defined(__NHC__)
#if __NHC__ >= 115
import NHC.FFI
#else
import FFI
#endif
import Numeric (showHex)
#endif
