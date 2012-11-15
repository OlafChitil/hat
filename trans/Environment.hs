-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment
  ,TySynBody(TApp,TFun,THelper,TVar),TyCls(Ty,Cls,Syn)
  ,arity,isLambdaBound,isTracedQName,mutateLetBound,fixPriority
  ,clsTySynInfo,isExpandableTypeSynonym,typeSynonymBody
  ) where

import Language.Haskell.Exts.Annotated (QName, Name)


newtype Environment = Env Int

-- AuxiliaryInfo is the extra information we need to know about identifiers.
data AuxiliaryInfo = 
         Value {- variable or constructor -}
	   { args     :: Int
	   , fixity   :: Fixity
	   , priority :: Int
	   , letBound :: Bool 
           , traced   :: Bool}
       | TyCls TyCls -- needed for im/export of (..)
       deriving (Show,Read)
data Fixity = L | R | Pre String | Def | None deriving (Eq,Show,Read)
data TyCls = Ty [String]{- data constructors -} [String]{- field labels -}
           | Cls [String]{- methods -}
           | Syn Int {- no. helper type syns -} TySynBody
  deriving (Show,Read)
data TySynBody = 
  TApp TySynBody TySynBody | TFun | THelper | TVar Int {- arg no. -}
  deriving (Show,Read)


arity :: Environment -> QName l -> Maybe Int
arity = undefined

typeSynonymBody :: Environment -> QName l -> Maybe TySynBody
typeSynonymBody = undefined

isLambdaBound :: Environment -> QName l -> Bool
isLambdaBound = undefined

isTracedQName :: Environment -> QName l -> Bool
isTracedQName = undefined

isExpandableTypeSynonym :: Environment -> QName l -> Bool
isExpandableTypeSynonym = undefined

mutateLetBound :: Environment -> Name l -> Environment
mutateLetBound = undefined

fixPriority :: Environment -> Name l -> Int
fixPriority = undefined

clsTySynInfo :: Environment -> QName l -> TyCls
clsTySynInfo = undefined