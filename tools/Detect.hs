{-# LANGUAGE EmptyDataDecls #-}

-- Low-level routines specifically for hat-detect.
module Detect
  ( findMain, edtNextChild
  , ParentSet, newParentSet
  , anySuspect
  ) where

import LowLevel (FileNode(..),nil,unevaluated
                ,SimpleNodeType(..),simpleNodeType,NodeType(..),nodeType)
import Numeric (showHex)
import Foreign.Ptr

data PS
type ParentSet = Ptr PS

foreign import ccall "parentset.h" newParentSet    :: FileNode -> IO ParentSet
foreign import ccall "parentset.h" freeParentSet   :: ParentSet -> IO ()
foreign import ccall "parentset.h" extendParentSet :: ParentSet -> FileNode -> IO ()

foreign import ccall "detectutils.h" findMainUse   :: Bool -> IO FileNode
foreign import ccall "detectutils.h" nextChild       :: ParentSet -> IO FileNode
foreign import ccall "detectutils.h" anySuspect      :: FileNode -> IO Bool

findMain :: IO FileNode
findMain = findMainUse False

-- For debugging:
-- foreign import ccall showParentSet   :: ParentSet -> IO ()

edtNextChild :: ParentSet -> IO FileNode
edtNextChild ps = do
 -- candidate ps
    c <- candidate ps
    b <- anySuspect c
    if c==LowLevel.nil || b then return c else edtNextChild ps
  where
  candidate ps = do
    c <- nextChild ps
 -- putStrLn ("edtNextChild: "++showHex (int c) "")
    if c==LowLevel.unevaluated	-- actually EOF
      then return LowLevel.nil
      else if c==LowLevel.nil
      then candidate ps
      else case simpleNodeType c of
        NodeConditional -> do extendParentSet ps c
                              --showParentSet ps
                              candidate ps
        NodeIdentifier  -> candidate ps
        NodeBasicValue  -> candidate ps
        NodeCAF         -> return c
        NodeApplication -> case nodeType c of
                             ExpApp      -> return c
                             ExpValueApp -> candidate ps
        NodeSugar       -> return c
        NodeSpecial     -> case nodeType c of
                             ExpProjection -> return c
                             _             -> do extendParentSet ps c
                                                 candidate ps
        _               -> error "unexpected node in edtNextChild"
