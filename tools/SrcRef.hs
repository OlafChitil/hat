{-# LANGUAGE EmptyDataDecls #-}

module SrcRef
  ( SrcRef(..)
  , readSrcRef		-- :: FileNode -> SrcRef
  , defnSrcRef		-- :: Ident -> SrcRef
  ) where

import LowLevel		(FileNode(..))
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr      (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.C.String	(CString, peekCString)
import Ident		(Ident(..))

-- Contents of a SrcRef node.
data SrcRef = SrcRef
  { filename	:: String
  , line	:: Int
  , column	:: Int
  , lineend	:: Int
  , columnend	:: Int
  } deriving (Eq,Show)

data SR;
foreign import ccall "artutils.h" readSRAt     :: FileNode -> IO (Ptr SR)

foreign import ccall "artutils.h" srFile       :: Ptr SR -> IO CString
foreign import ccall "artutils.h" srLine       :: Ptr SR -> IO Int
foreign import ccall "artutils.h" srColumn     :: Ptr SR -> IO Int
foreign import ccall "artutils.h" srLineEnd    :: Ptr SR -> IO Int
foreign import ccall "artutils.h" srColumnEnd  :: Ptr SR -> IO Int

readSrcRef :: FileNode -> SrcRef
readSrcRef n = unsafePerformIO $ do
    p   <- readSRAt n	-- p is malloc'ed in C land
    nm  <- srFile p
    l   <- srLine p
    c   <- srColumn p
    le  <- srLineEnd p
    ce  <- srColumnEnd p
    snm <- peekCString nm
    free p		-- dispose of p again
    return SrcRef
	{ filename  = snm
	, line      = l
	, column    = c
	, lineend   = le
	, columnend = ce
	}

---------------------------------------------------------------------
defnSrcRef :: Ident -> SrcRef
defnSrcRef i = SrcRef
	{ filename	= i_srcfile i
	, line		= i_defnline i
	, column	= i_defncol i
	, lineend	= i_defnlineend i
	, columnend	= i_defncolend i
	}
