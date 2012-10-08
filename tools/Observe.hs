module Observe
  ( ObserveResult(..)
  ,   isFound
  ,   isNotFound
  ,   isInterrupted
  ,   fromFound
  , newObservation	-- :: QName -> Maybe QName -> Bool -> Int
			--      -> ObserveResult
  , newObservationSrc	-- :: String -> Int -> Int -> ObserveResult
-- , observableInfo	-- :: Int -> IO ()
  ) where

import LowLevel
import Foreign.Ptr      (Ptr)
import Foreign.C.String (CString, withCString)
import System.IO.Unsafe (unsafePerformIO)
import SExp (QName(..))

------------------------------------------------------------------------
data ObserveResult = Found [FileNode]
                   | NotFound
                   | Interrupted

isInterrupted :: ObserveResult -> Bool
isInterrupted Interrupted = True
isInterrupted _           = False

isNotFound :: ObserveResult -> Bool
isNotFound NotFound       = True
isNotFound _              = False

isFound :: ObserveResult -> Bool
isFound (Found _)         = True
isFound _                 = False

fromFound :: ObserveResult -> [FileNode]
fromFound (Found r)         = r
fromFound _                 = []

------------------------------------------------------------------------
foreign import ccall "observeutils.h"
    setObserveContext :: Bool -> Bool -> Int -> CString -> IO ()
foreign import ccall "observeutils.h"
    lookForFirstApp   :: CString -> IO FileNode
foreign import ccall "observeutils.h"
    lookForFirstSrc   :: Int -> Int -> CString -> IO FileNode
foreign import ccall "observeutils.h"
    nextObservation   :: FileNode -> IO FileNode
--foreign import ccall observableInfo    :: Int -> IO ()

-- Get all observations for ident1 (within ident2 if available)
-- If ident2 is Nothing, then the `recursive' flag selects whether
-- calls from ident1 to itself are included.
newObservation :: QName -> Maybe QName -> Bool -> Int -> ObserveResult
newObservation ident1 ident2 rec arity =
  unsafePerformIO $
  do _ <- case ident2 of
            Nothing -> withCString "" (setObserveContext False rec arity)
            Just id -> withCString (noQ id) (setObserveContext True True arity)
     n <- withCString (noQ ident1) lookForFirstApp
     case int n of
       1 -> return NotFound
       3 -> return Interrupted
       _ -> return (Found (n: moreObservations n))
  where
    noQ qn = case qn of Plain v -> v; Qualified _ v -> v

moreObservations :: FileNode -> [FileNode]
moreObservations n =
  unsafePerformIO $
  do n' <- nextObservation n
     case int n' of
       1 -> return []
       3 -> return []
       _ -> return (n': moreObservations n')

-- Get all observations at the given source location.
newObservationSrc :: String -> Int -> Int -> ObserveResult
newObservationSrc moduleName line column =
  unsafePerformIO $
  do withCString "" (setObserveContext False True 0)
     n <- withCString moduleName (lookForFirstSrc line column)
     case int n of
       1 -> return NotFound
       3 -> return Interrupted
       _ -> return (Found (n: moreObservations n))

------------------------------------------------------------------------
{-
foreign import ccall observeGlobalsNum   :: IO Int
foreign import ccall observeGlobalsArray :: IO (Ptr (Ptr Info))
-}
