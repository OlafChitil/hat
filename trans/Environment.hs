-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment
  ,TySynBody(TApp,TFun,THelper,TVar),TyCls(Ty,Cls,Syn)
  ,arity,isLambdaBound,isTracedQName,mutateLetBound,fixPriority, hasPriority
  ,clsTySynInfo,isExpandableTypeSynonym,typeSynonymBody
  ) where

import Language.Haskell.Exts.Annotated (QName, Name)
import Data.Map as Map

type EnvInfo = (Identifier,AuxiliaryInfo)
type QualId = (String,String)  -- unqualified name followed by module name
newtype Environment = Env (Map QualId EnvInfo) (Map String EnvInfo)
  deriving Show

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con TypeSort String{-type-} String{-con-}
		| Field String{-type-} String{-field-}
		| Method String{-class-} String{-method-}
                | TypeClass String
	deriving (Show,Read,Eq,Ord)
data TypeSort = Data | Newtype deriving (Show,Read,Eq,Ord)

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

lookupEnv :: Environment -> QName l -> (Identifier,AuxiliaryInfo)
lookupEnv (Env qualMap _) (Qual _ mod name) = Map.lookup (getId name,getId mod) qualMap
lookupEnv (Env _ map) (UnQual _ name) = Map.lookup (getId name) map
lookupEnv _ (Special _ _) = error "Environment.lookupEnv: called with special qName."

arity :: Environment -> QName l -> Maybe Int
arity env qName = case lookupEnv env qName of
  Just (_,Value{args=a}) -> Just a
  _                      -> Nothing

typeSynonymBody :: Environment -> QName l -> Maybe TySynBody
typeSynonymBody env qName = case lookupEnv env qName of
  Just (TypeClass _,TyCls (Syn _ tySynBody)) -> Just tySynBody
  _                                          -> Nothing

isLambdaBound :: Environment -> QName l -> Bool
isLambdaBound env qName = case lookupEnv env qName of
  Just (_,Value{letBound=b}) -> not b
  _                          -> error "Environment.isLambdaBound: argument not a value variable."

isTracedQName :: Environment -> QName l -> Bool
isTracedQName env qName = case lookupEnv env qName of
  Just (_,Value{traced=b}) -> b
  _                        -> error "Environment.isTracedQName: argument not a value variable."

isExpandableTypeSynonym :: Environment -> QName l -> Bool
isExpandableTypeSynonym env qName = case lookupEnv env qName of
  Just (TypeClass _,TyCls (Syn n _)) -> n > 0
  _                                  -> False

-- make given local variable let-bound
-- assumes name is for a local value variable
mutateLetBound :: Environment -> Name l -> Environment
mutateLetBound (Env qualMap map) name = 
  Env qualMap (adjust (\(i,a@Value{}) -> (i,a{letBound=True,args=0})) name map)

-- Obtain combined fixity and priority of local name.
fixPriority :: Environment -> Name l -> Int
fixPriority (Env _ map) name = case Map.lookup (getId name) map of
  Just (_,Value{fixity=f,priority=p}) -> encode f p
  _                                   -> 3 -- default fixity and priority
  where
  encode Def     _ = 3
  encode L       n = 2 + (n*4)
  encode R       n = 1 + (n*4)
  encode None    n = 0 + (n*4)
  encode (Pre _) n = 0 + (n*4)

-- Obtain priority of given local name. Result in 0-9.
hasPriority :: Environment -> Name l -> Int
hasPriority (Env _ map) name = case Map.lookup (getId name) map of
  Just (_,Value{priority=p}) -> p
  _                          -> 9  -- default priority

clsTySynInfo :: Environment -> QName l -> TyCls
clsTySynInfo env qName = case lookupEnv env qName of
  Just (TypeClass _,TyCls tyCls) -> tyCls
  _                              -> error "Environment.clsTySynInfo: argument not a type or class."