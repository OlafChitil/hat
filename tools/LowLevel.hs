{-# LANGUAGE EmptyDataDecls #-}

module LowLevel
  ( openHatFile		-- :: CString -> CString -> IO ()
  , closeHatFile  -- :: IO ()
  , getBridgeValue	-- :: IO FileNode
  , getErrorLoc		-- :: IO FileNode
  , getErrorMessage	-- :: IO CString
  , hatVersionNumber	-- :: String

  , FileNode(..)
  , nil			-- :: FileNode
  , unevaluated		-- :: FileNode
  , entered		-- :: FileNode
  , interrupted		-- :: FileNode
  , lambda		-- :: FileNode
  , dolambda		-- :: FileNode

  , NodeType(..)
  , nodeType		-- :: FileNode -> NodeType
  , SimpleNodeType(..)
  , simpleNodeType	-- :: FileNode -> SimpleNodeType

  , getParentNode	-- :: FileNode -> FileNode
  , getResult		-- :: FileNode -> Bool -> FileNode
  , peekResult		-- :: FileNode -> FileNode

  , getValue		-- :: FileNode -> String
  , getValueMod		-- :: FileNode -> String
  , getFixity		-- :: FileNode -> Int
  , isLiteral		-- :: FileNode -> Bool
  , isConstructor	-- :: FileNode -> Bool
  , isConstrFields	-- :: FileNode -> Bool
  , isLambda		-- :: FileNode -> Bool
  , isDoLambda		-- :: FileNode -> Bool

  , getAtom		-- :: FileNode -> String
  , getAtomMod		-- :: FileNode -> String
  , getAtomFixity	-- :: FileNode -> Int

  , getSubExprs		-- :: FileNode -> [FileNode]
  , peekSubExprs        -- :: FileNode -> [FileNode]
  , peekExpArg          -- :: FileNode -> Int -> FileNode
  , getFieldLabels	-- :: FileNode -> [String]

  , getSrcRef		-- :: FileNode -> FileNode
  , getDefnRef		-- :: FileNode -> FileNode

  , peekTrace		-- :: FileNode -> FileNode

  , nodeSequence	-- :: IO [(FileNode,NodeType)]

  , hiddenChildren      -- :: FileNode -> [FileNode]
  ) where

import Foreign.Ptr      (Ptr)
import Foreign.C.String	(CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)
import Numeric (showHex)

-- Reference into the .hat file
newtype FileNode = FileNode {int::Int}
  deriving (Eq, Ord)
instance Show FileNode where showsPrec _ (FileNode i) = showHex i
nil         = FileNode 0
unevaluated = FileNode 1
entered     = FileNode 2
interrupted = FileNode 3
lambda      = FileNode 4
dolambda    = FileNode 5

instance Num FileNode where
  (+) (FileNode x) (FileNode y) = FileNode (x+y)

-- There are 25 basic types of node, encoded in the lower 5 bits of the tag
-- They fall into four broad classes: module info, srcpos, expressions, atoms.
data NodeType
  = Module
  | SrcPos
  | ExpApp
  | ExpValueApp
  | ExpChar
  | ExpInt
  | ExpInteger
  | ExpRat
  | ExpRational
  | ExpFloat
  | ExpDouble
  | ExpValueUse
  | ExpConstUse
  | ExpConstDef
  | ExpGuard
  | ExpCase
  | ExpIf
  | ExpFieldUpdate
  | ExpProjection
  | ExpHidden
  | ExpForward
  | ExpDoStmt
  | AtomVariable
  | AtomConstructor
  | AtomAbstract
  | ListCons
  deriving (Eq,Show)

instance Enum NodeType where
  toEnum  0 = Module
  toEnum  1 = SrcPos
  toEnum  2 = ExpApp
  toEnum  3 = ExpValueApp
  toEnum  4 = ExpChar
  toEnum  5 = ExpInt
  toEnum  6 = ExpInteger
  toEnum  7 = ExpRat
  toEnum  8 = ExpRational
  toEnum  9 = ExpFloat
  toEnum 10 = ExpDouble
  toEnum 11 = ExpValueUse
  toEnum 12 = ExpConstUse
  toEnum 13 = ExpConstDef
  toEnum 14 = ExpGuard
  toEnum 15 = ExpCase
  toEnum 16 = ExpIf
  toEnum 17 = ExpFieldUpdate
  toEnum 18 = ExpProjection
  toEnum 19 = ExpHidden
  toEnum 20 = ExpForward
  toEnum 21 = ExpDoStmt
  toEnum 26 = AtomVariable
  toEnum 27 = AtomConstructor
  toEnum 28 = AtomAbstract
  toEnum 29 = ListCons
  toEnum n  = error ("NodeType.toEnum "++show n)

-- For most purposes, we don't care about the exact node type, and a
-- simplified division of nodes into kinds is useful.
data SimpleNodeType
  = NodeModule		-- Module
  | NodeSrcPos		-- SrcPos
  | NodeApplication	-- ExpApp, ExpValueApp
  | NodeBasicValue	-- ExpChar, ExpInt, ..., ExpDouble
  | NodeIdentifier	-- ExpValueUse
  | NodeCAF		-- ExpConstUse, ExpConstDef
  | NodeConditional	-- ExpGuard, ExpCase, ExpIf
  | NodeSugar		-- ExpFieldUpdate, ExpDoStmt
  | NodeSpecial		-- ExpProjection, ExpHidden, ExpForward
  | NodeAtom		-- AtomVariable, AtomConstructor, AtomAbstract
  | NodeList            -- ListCons
  deriving (Eq)


-- For opening files, and and collecting values from the bridge file.
foreign import ccall "artutils.h" openHatFile    :: CString -> CString -> IO ()
foreign import ccall "artutils.h" closeHatFile   :: IO ()
foreign import ccall "artutils.h" getBridgeValue :: IO FileNode
foreign import ccall "artutils.h" getErrorLoc    :: IO FileNode
foreign import ccall "artutils.h" errorMessage   :: IO CString
foreign import ccall "artutils.h" versionNumber  :: IO CString
getErrorMessage  :: IO String
getErrorMessage = do msg <- errorMessage
                     peekCString msg
hatVersionNumber :: String
hatVersionNumber = unsafePerformIO $ do num <- versionNumber
                                        peekCString num


-- Find out what node type we have a reference to.
nodeType :: FileNode -> NodeType
nodeType n = toEnum (getNodeType n)
foreign import ccall "artutils.h" getNodeType   :: FileNode -> Int

-- Give a simple node type to a reference.
simpleNodeType :: FileNode -> SimpleNodeType
simpleNodeType n =
    case nodeType n of
      Module -> NodeModule
      SrcPos -> NodeSrcPos
      ExpApp      -> NodeApplication
      ExpValueApp -> NodeApplication
      ExpValueUse -> NodeIdentifier
      ExpConstUse -> NodeCAF
      ExpConstDef -> NodeCAF
      ExpFieldUpdate -> NodeSugar
      ExpDoStmt      -> NodeSugar
      ExpGuard -> NodeConditional
      ExpCase  -> NodeConditional
      ExpIf    -> NodeConditional
      ExpProjection -> NodeSpecial
      ExpHidden     -> NodeSpecial
      ExpForward    -> NodeSpecial
      AtomVariable    -> NodeAtom
      AtomConstructor -> NodeAtom
      AtomAbstract    -> NodeAtom
      ListCons -> NodeList
      _  -> NodeBasicValue

-- For any node type, get its parent.  If it doesn't have one, the
-- zero node is returned.
foreign import ccall "artutils.h" parentNode :: FileNode -> FileNode
getParentNode :: FileNode -> FileNode
getParentNode n = peekTrace (parentNode n)

-- For any node, return its result pointer.  Only an application, CAF,
-- case/if/guard, field update, or hidden node actually has one - in all
-- other cases we get back 0.  The Boolean denotes whether to stop at
-- the first Hidden node.
foreign import ccall "artutils.h" getResult     :: FileNode -> Bool -> FileNode

-- Because getResult stops one link in the chain *before* an Unevaluated,
-- Entered, Interrupted, or Lambda node, peekResult looks just one step
-- down the result chain.
foreign import ccall "artutils.h" peekResult    :: FileNode -> FileNode

-- For nodes of value kind, we get back a string representation of the value
-- (Integer, Double etc) or name (identifier, constructor, etc),
-- and its fixity.  The predicate isLiteral reports True for values of basic
-- types like Int, Char, Double etc, and isConstructor identifies Constrs.
-- The predicate isLambda can only be used on an ExpValueUse node.
foreign import ccall "artutils.h" getNm         :: FileNode -> CString
foreign import ccall "artutils.h" getNmMod      :: FileNode -> CString
foreign import ccall "artutils.h" getFixity     :: FileNode -> Int
foreign import ccall "artutils.h" isLiteral     :: FileNode -> Bool
foreign import ccall "artutils.h" isConstructor :: FileNode -> Bool
foreign import ccall "artutils.h" isConstrFields:: FileNode -> Bool
foreign import ccall "artutils.h" isLambda      :: FileNode -> Bool
foreign import ccall "artutils.h" isDoLambda    :: FileNode -> Bool
getValue :: FileNode -> String
getValue n = unsafePerformIO (peekCString (getNm n))
getValueMod :: FileNode -> String
getValueMod n = unsafePerformIO (peekCString (getNmMod n))

data Atom;
foreign import ccall "artutils.h" readAtomAt    :: FileNode -> Ptr Atom
foreign import ccall "artutils.h" identName     :: Ptr Atom -> CString
foreign import ccall "artutils.h" identModName  :: Ptr Atom -> CString
foreign import ccall "artutils.h" identFixity   :: Ptr Atom -> Int
getAtom :: FileNode -> String
getAtom n = unsafePerformIO (peekCString (identName (readAtomAt n)))
getAtomMod :: FileNode -> String
getAtomMod n = unsafePerformIO (peekCString (identModName (readAtomAt n)))
getAtomFixity :: FileNode -> Int
getAtomFixity n = identFixity (readAtomAt n)

-- For Trace nodes excluding kind TNm, get any argument *values*.
getSubExprs :: FileNode -> [FileNode]
getSubExprs n = let arity = getExpArity n
                in (map (getExpArg n) [0..arity])
foreign import ccall "artutils.h" getExpArity   :: FileNode -> Int
foreign import ccall "artutils.h" getExpArg     :: FileNode -> Int -> FileNode

-- get all argument expressions (or atoms)
peekSubExprs :: FileNode -> [FileNode]
peekSubExprs n = let arity = getExpArity n
                 in (map (peekExpArg n) [0..arity])
foreign import ccall "artutils.h" peekExpArg    :: FileNode -> Int -> FileNode

-- For a ExpFieldUpdate node only, get the list of updated labels.
getFieldLabels :: FileNode -> [String]
getFieldLabels n = let arity = getExpArity n
                   in map (getAtom . getFieldLabel n) [0..(arity-1)]
foreign import ccall "artutils.h" getFieldLabel :: FileNode -> Int -> FileNode


-- For any node type, get its source reference.  If it doesn't have one,
-- we get a null pointer (0) back.
foreign import ccall "artutils.h" getSrcRef     :: FileNode -> FileNode

-- For a variable, constructor, or application, get the Atom node that
-- contains its definition information.  If it isn't a variable,
-- constructor, or application, we get a null pointer (0) back.
foreign import ccall "artutils.h" getDefnRef    :: FileNode -> FileNode


-- Look past any SATs, indirections, or hidden nodes, to `real' trace.
foreign import ccall "artutils.h" peekTrace     :: FileNode -> FileNode


-- Lazily return the complete sequence of node addresses and their types
nodeSequence :: IO [(FileNode,NodeType)]
nodeSequence = do q_init; list '0'
  where list _ = do n  <- currentfilepos
                    c' <- q_peek
                    c  <- q_tag
                    q_skipNode c' 
                    if c=='\31' then return []	-- end of file
                      else return ((FileNode n, toEnum (fromEnum c))
                                  : unsafePerformIO (list c))
foreign import ccall "artutils.h" q_init         :: IO ()
foreign import ccall "artutils.h" currentfilepos :: IO Int
foreign import ccall "artutils.h" q_peek         :: IO Char
foreign import ccall "artutils.h" q_tag          :: IO Char
foreign import ccall "artutils.h" q_skipNode     :: Char -> IO Int

hiddenChildren :: FileNode -> [FileNode]
-- assumes given FileNode is Hidden node
-- gives list of all children, possibly empty.
hiddenChildren hidden = go (peekExpArg hidden 1)
  where
  go node = if node == nil then [] 
                           else peekExpArg node 1 : go (peekExpArg node 2)
