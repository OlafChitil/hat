module SysDeps (
   PackedString, packString, unpackPS, trace, isAlphaNum
) where

import qualified Data.ByteString.Char8 as PackedString
import Debug.Trace(trace)
import Data.Char(isAlphaNum)

type PackedString = PackedString.ByteString

packString :: String -> PackedString
packString = PackedString.pack

unpackPS :: PackedString -> String
unpackPS = PackedString.unpack

{--
#if __GLASGOW_HASKELL__ >= 502
import Data.PackedString as PackedString
#else
import PackedString
#endif

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace (trace)
#elif __GLASGOW_HASKELL__ >= 502
import Debug.Trace (trace)
#else
import IOExts      (trace)
#endif

#if defined(__HASKELL98__)
import Char        (isAlphaNum)
#else
import Data.Char        (isAlphanum)

isAlphaNum :: Char -> Bool
isAlphaNum = isAlphanum
#endif
--}