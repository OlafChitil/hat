-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment,Identifier,AuxiliaryInfo
  ,TySynBody(TApp,TFun,THelper,TVar),TyCls(Ty,Cls,Syn)
  ,arity,isLambdaBound,isTracedQName,mutateLetBound,fixPriority, hasPriority
  ,clsTySynInfo,isExpandableTypeSynonym,typeSynonymBody
  ) where

import Language.Haskell.Exts.Annotated
import SynHelp (Id(getId))
import qualified Data.Set as Set
import Relation
import Data.Maybe (fromMaybe)

type EnvInfo = (Identifier,AuxiliaryInfo)
type QualId = (String,String)  -- unqualified name followed by module name
-- data Environment = Env (Map QualId EnvInfo) (Map String EnvInfo)
--   deriving Show

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
       deriving (Eq,Ord,Show,Read)
data Fixity = L | R | Pre String | Def | None deriving (Eq,Ord,Show,Read)
data TyCls = Ty [String]{- data constructors -} [String]{- field labels -}
           | Cls [String]{- methods -}
           | Syn Int {- no. helper type syns -} TySynBody
  deriving (Eq,Ord,Show,Read)
data TySynBody = 
  TApp TySynBody TySynBody | TFun | THelper | TVar Int {- arg no. -}
  deriving (Eq,Ord,Show,Read)

type Entity = (Identifier, AuxiliaryInfo)
type Environment = Relation (QName ()) Entity


-- Get the environment for all top-level definitions of a module.
-- Only produce unqualified QNames.
moduleDefines :: Module l -> Environment
moduleDefines = undefined

-- Determine the exports of a module
exports :: Module l -> Environment -> Rel (Name ()) Entity
exports mod@(Module l maybeModuleHead _ _ _) env =
  case maybeModuleHead of
    Nothing -> exportList [EVar l (UnQual l "main")]
    Just (ModuleHead _ _ _ Nothing) -> getQualified `mapDom` moduleDefines mod
    Just (ModuleHead _ _ _ (Just (ExportSpecList _ list))) -> exportList list
  where
  exportList list = getQualified `mapDom` unionRelations exports
    where
    exports = filterExportSpec env `map` list 

-- Determine exports for one export specification of the export list
filterExportSpec :: Environment -> ExportSpec l -> Environment
filterExportSpec env (EModuleContents _ moduleName) =
  (qual moduleNameT `mapDom` unqs) `intersectRelation` qs
  where
  moduleNameT = amap (const ()) moduleName
  (qs,unqs) = partitionDom isQual env
filterExportSpec env eSpec =
  unionRelations [mSpec, mSub]
  where
  mSpec = restrictRng notIsCon (restrictDom (== qName) env)
  allSubs = owns `Set.map` rng mSpec
  subs = restrictRng (`Set.member` allSubs) rel
  (qName,mSub) = case eSpec of
    EVar _ qName -> (qName, emptyRelation)
    EAbs _ qName -> (qName, emptyRelation)
    EThingsAll _ qName -> (qName, subs)
    EThingsWith _ qName cNames -> 
      (qName, restrictDom ((`elem` map toId cNames) . toId) subs)

-- Filter with one import declaration from the export environment of the imported module
imports :: Rel (Name ()) Entity -> ImportDecl l -> Environment
imports exports importDecl = 
  if importQualified importDecl then qs else unionRelations [unqs, qs]
  where
  qs = mkQual (importQual importDecl) `mapDom` incoming
  unqs = mkUnqual `mapDom` incoming
  listed = unionRelations (map (filterImportSpec isHiding exports) impSpecs)
  incoming = if isHiding then exports `minusRelation` listed else listed
  (isHiding,impSpecs) = case importSpecs importDecl of
    Nothing -> (True, []) 
    Just (ImportSpecList _ h impSpecs) -> (h,impSpecs)
  
-- Qualifier for any qualfied imports of the given import declaration.
-- If exists, the 'as' module name; otherwise the name of the imported module itself.
importQual :: ImportDecl l -> ModuleName l
importQual imDecl = maybe (importModule imDecl) (importAs imDecl)

-- Filter given environment with one import specification from an import list
filterImportSpec :: Bool -> Rel (Name ()) Entity -> ImportSpec l -> Rel (Name ()) Entity
filterImportSpec isHiding rel iSpec =
  unionRels [fSpec,fSub]
  where
  fSpec = restrictRng consider (restrictDom (== name) rel)
  allSubs = owns `Set.map` rng fSpec
  subs = restrictRng (`Set.member` allSubs) rel
  (name,fSub,noSubSpec) = case iSpec of
    IVar _ name -> (name, emptyRelation, True)
    IAbs _ name -> (name, emptyRelation, True)
    IThingsAll _ name -> (name, subs, False)
    IThingsWith _ name cNames -> 
      (name, restrictDom ((`elem` map toId cNames) . toId) subs, False)
  consider = if isHiding && noSubSpec then const True else notIsCon
   
-- -------------------------------------------------------------------------------------
-- Looking up, inserting and mutating individual environment entries.

lookupEnv :: Environment -> QName l -> Maybe (Identifier,AuxiliaryInfo)
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
  Env qualMap (adjust (\(i,a@Value{}) -> (i,a{letBound=True,args=0})) (getId name) map)

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