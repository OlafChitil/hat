module Ident
  ( Ident(..)
  , getIdentAt		-- :: FileNode -> IO Ident
  ) where

import LowLevel		(FileNode(..))
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr      (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.C.String	(CString, peekCString)

--foreign import ccall "malloc.h &free" finaliserFree :: FunPtr (Ptr a -> IO ())

-- All possible relevant information about an identifier
data Ident = Ident
  { i_name	:: String
  , i_modname	:: String
  , i_srcfile	:: String
  , i_fixity	:: Int
  , i_arity	:: Int
  , i_defnline	:: Int
  , i_defncol	:: Int
  , i_defnlineend :: Int
  , i_defncolend  :: Int
  , i_isTraced	:: Bool
  , i_caf       :: Bool
  , i_uses      :: Int
  , i_pending   :: Int
  , i_thunks    :: Int
  }

foreign import ccall "artutils.h" readAtomAt    :: FileNode -> IO (Ptr Ident)
foreign import ccall "artutils.h" identName     :: Ptr Ident -> IO CString
foreign import ccall "artutils.h" identModName  :: Ptr Ident -> IO CString
foreign import ccall "artutils.h" identSrcFile  :: Ptr Ident -> IO CString
foreign import ccall "artutils.h" identFixity   :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identArity    :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identDefnLine :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identDefnCol  :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identDefnLineEnd :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identDefnColEnd  :: Ptr Ident -> IO Int
foreign import ccall "artutils.h" identIsTraced :: Ptr Ident -> IO Bool

getIdentAt :: FileNode -> IO Ident
getIdentAt n = do
    p <- readAtomAt n	-- p is malloc'ed in C land
    nm  <- identName p
    mod <- identModName p
    src <- identSrcFile p
    fix <- identFixity p
    ar  <- identArity p
    dl  <- identDefnLine p
    dc  <- identDefnCol p
    dle <- identDefnLineEnd p
    dce <- identDefnColEnd p
    tr  <- identIsTraced p
    snm  <- peekCString nm
    smod <- peekCString mod
    ssrc <- peekCString src
    free p		-- dispose of p again
    return Ident
	{ i_name     = snm
	, i_modname  = smod
	, i_srcfile  = ssrc
	, i_fixity   = fix
	, i_arity    = ar
	, i_defnline = dl
	, i_defncol  = dc
        , i_defnlineend = dle
        , i_defncolend  = dce
	, i_isTraced = tr
        , i_caf = error "Ident.getIdentAt i_caf"
        , i_uses = error "Ident.getIdentAt i_uses"
        , i_pending = error "Ident.getIdentAt i_pending"
        , i_thunks = error "Ident.getIdentAt i_thunks"
	}

---------------------------------------------------------------------
