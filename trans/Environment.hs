-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment,Identifier,AuxiliaryInfo
  ,TySynBody(TApp,TFun,THelper,TVar),TyCls(Ty,Cls,Syn)
  ,arity,isLambdaBound,isTracedQName,mutateLetBound,fixPriority, hasPriority
  ,clsTySynInfo,isExpandableTypeSynonym,typeSynonymBody
  ) where

import Language.Haskell.Exts.Annotated
import SynHelp (Id(getId),getQualified,mkQual,qual,isQual)
import qualified Data.Set as Set
import qualified Data.Map as Map (adjust)
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
	   , fixity   :: Environment.Fixity
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

notIsCon :: Entity -> Bool
notIsCon (Environment.Con _ _ _,_) = False
notIsCon _ = True

-- Is a value, excluding data constructor.
isValue :: Entity -> Bool
isValue (Environment.Var _, _) = True
isValue (Field _ _, _) = True
isValue (Method _ _, _) = True
isValue _ = False

-- Is a type, including type synoym.
isType :: Entity -> Bool
isType (TypeClass _, TyCls (Ty _ _)) = True
isType (TypeClass _, TyCls (Syn _ _)) = True
isType _ = False

-- Is the given entity owned by any entity in the given set?
-- Ownership means constructor/field of a data type or method of a class.
isOwned :: Entity -> Set.Set Entity -> Bool
isOwned (Environment.Con _ ty _,_) owners = 
  not . Set.null . Set.filter (isTypeId ty) $ owners
isOwned (Field ty _,_) owners = not . Set.null . Set.filter (isTypeId ty) $ owners
isOwned (Method cls _,_) owners = not . Set.null . Set.filter (isClassId cls) $ owners
isOwned _ _ = False

-- Check whether given entity is a type of given id.
isTypeId :: String -> Entity -> Bool
isTypeId id (TypeClass tyCls, TyCls (Ty _ _)) = id == tyCls
isTypeId _ _ = False

-- Check whether given entity is a class of given id.
isClassId :: String -> Entity -> Bool
isClassId id (TypeClass tyCls, TyCls (Cls _)) = id == tyCls
isClassId _ _ = False

dropAnn :: Annotated ast => ast l -> ast ()
dropAnn = fmap (const ())

-- -----------------------------------------------------------------------------------
-- Obtain an environment from parts of a syntax tree.

-- Get the environment for all top-level definitions of a module.
-- Only produce unqualified QNames.
moduleDefines :: Module l -> Environment
moduleDefines (Module _ _ _ _ decls) =
  unionRelations (map declEnv decls)
  -- NEED to merge infix knowledge

declEnv :: Decl l -> Environment
declEnv (TypeDecl _ declHead _) = declHeadEnv declHead
declEnv (TypeFamDecl l declHead _) = 
  notSupported l "type family declaration"
declEnv (DataDecl _ _ _ declHead qualConDecls _) = 
  unionRelations (declHeadEnv declHead : map qualConDeclEnv qualConDecls)
declEnv (GDataDecl l _ _ declHead _ gadtDecls _) =
  notSupported l "gadt declaration"
declEnv (DataFamDecl l declHead _) = 
  notSupported l "data family declaration"
declEnv (TypeInsDecl _ _ _) = emptyRelation
declEnv (DataInsDecl l _ _ _ _) = 
  notSupported l "data family instance declaration"
declEnv (GDataInsDecl l _ _ _ _) =
  notSupported l "gadt family instance declaration"
declEnv (ClassDecl _ _ declHead _ maybeClassDecls) = ???
declEnv (InstDecl _ _ _ _) = emptyRelation
declEnv (DerivDecl _ _ _) = emptyRelation
declEnv (InfixDecl _ assoc maybePri ops) =
declEnv (DefaultDecl _ _) = emptyRelation
declEnv (SpliceDecl l _) = 
  notSupported l "splice declaration"
declEnv (TypeSig _ names _) =
  -- needed if for methods in classes
declEnv (FunBind _ matches) = matchEnv (head matches)
declEnv (PatBind _ pat _ _ _) = patEnv pat
declEnv (ForImp l _ _ _ name ty) =
  -- only for NoHat. import
  Map.singleton (dropAnn (UnQual l name))
    (Var (getId name), Value{args = length tyArgs, fixity = Def, priority = 9
                            ,letBound = True, traced = True})
  where
  (tyArgs,_) = decomposeFunType ty
declEnv (ForExp _ _ _ _ _) = emptyRelation
declEnv (RulePragmaDecl _ _) = emptyRelation
declEnv (DeprPragmaDecl _ _) = emptyRelation
declEnv (WarnPragmaDecl _ _) = emptyRelation
declEnv (InlineSig _ _ _ _) = emptyRelation
declEnv (InlineConlikeSig _ _ _) = emptyRelation
declEnv (SpecSig _ _ _) = emptyRelation
declEnv (SpecInlineSig _ _ _ _ _) = emptyRelation
declEnv (InstSig _ _ _) = emptyRelation
declEnv (AnnPragma _ _) = emptyRelation


matchEnv :: Match l -> Environment
matchEnv (Match l name pats _ _) = 
  Map.singleton (dropAnn (UnQual l name))
    (Var (getId name), Value{args = length pats, fixity = Def, priority = 9
                            ,letBound = True, traced = True})
matchEnv (InfixMatch l pat name pats rhs maybeBinds) =
  matchEnv (Match l name (pat:pats) rhs maybeBinds)

patEnv :: Pat l -> Environment
patEnv (PVar l name) = 
  Map.singleton (dropAnn (UnQual l name))
    (Var (getId name), Value{args = 0, fixity = Def, priority = 9
                            ,letBound = True, traced = True})
patEnv (PLit _ _) = emptyRelation
patEnv (PNeg _ pat) = patEnv pat
patEnv (PNPlusK l name _) = patEnv (PVar l name)
patEnv (PInfixApp _ patl _ patr) = unionRelations [patEnv patl, patEnv patr]
patEnv (PApp _ _ pats) = unionRelations . map patEnv $ pats
patEnv (PTuple _ pats) = unionRelations . map patEnv $ pats
patEnv (PList _ pats) = unionRelations . map patEnv $ pats
patEnv (PParen _ pat) = patEnv pat
patEnv (PRec _ _ patFields) = unionRelation . map patField $ patFields
patEnv (PAsPat _ _ pat) = patEnv pat
patEnv (PWildCard _) = emptyRelation
patEnv (PIrrPat _ pat) = patEnv pat
patEnv (PatTypeSig _ pat _) = patEnv pat
patEnv (PViewPat _ _ pat) = patEnv pat
patEnv (PRPat l _) = notSupported l "regular list pattern"
patEnv (PXTag l _ _ _ _) = notSupported l "XML element pattern"
patEnv (PXETag l _ _ _) = notSupported l "XML singleton element pattern"
patEnv (PXPcdata l _) = notSupported l "XML PCDATA pattern"
patEnv (PXPatTag l _) = notSupported l "XML embedded pattern"
patEnv (PXRPats l _) = notSupported l "XML regular list pattern"
patEnv (PExplTypeArg l _ _) = notSupported l "explicit generics style type argument"
patEnv (PQuasiQuote l _ _) = notSupported l "quasi quote pattern"
patEnv (PBangPat _ pat) = patEnv pat

patField :: PatField l -> Environment
patField (PFieldPat _ _ pat) = patEnv pat
patField (PFieldPun _ _) = emptyRelation
patField (PFieldWildcard _) = emptyRelation

-- -----------------------------------------------------------------------------------
-- Determine export and import environments

-- In the hx-file of a module only unqualified names are used 
type HxEnvironment = Relation (Name ()) Entity

-- Determine the exports of a module
exports :: Module l -> Environment -> HxEnvironment
exports mod@(Module l maybeModuleHead _ _ _) env =
  case maybeModuleHead of
    Nothing -> exportList [EVar l (UnQual l (Ident l "main"))]
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
  moduleNameT = dropAnn moduleName
  (qs,unqs) = partitionDom isQual env
filterExportSpec env eSpec =
  unionRelations [mSpec, mSub]
  where
  mSpec = restrictRng notIsCon (restrictDom (== qNameT) env)
  allOwners = rng mSpec
  subs = restrictRng (`isOwned` allOwners) env
  qNameT = dropAnn qName
  (qName,mSub) = case eSpec of
    EVar _ qName -> (qName, emptyRelation)
    EAbs _ qName -> (qName, emptyRelation)
    EThingAll _ qName -> (qName, subs)
    EThingWith _ qName cNames -> 
      (qName, restrictDom ((`elem` map getId cNames) . getId) subs)

-- Filter with one import declaration from the export environment of the imported module
imports :: HxEnvironment -> ImportDecl l -> Environment
imports exports importDecl = 
  if importQualified importDecl then qs else unionRelations [unqs, qs]
  where
  qs = mkQual (importQual importDecl) `mapDom` incoming
  unqs = UnQual () `mapDom` incoming
  listed = unionRelations (map (filterImportSpec isHiding exports) impSpecs)
  incoming = if isHiding then exports `minusRelation` listed else listed
  (isHiding,impSpecs) = case importSpecs importDecl of
    Nothing -> (True, []) 
    Just (ImportSpecList _ h impSpecs) -> (h,impSpecs)
  
-- Qualifier for any qualfied imports of the given import declaration.
-- If exists, the 'as' module name; otherwise the name of the imported module itself.
importQual :: ImportDecl l -> ModuleName ()
importQual imDecl = dropAnn (fromMaybe (importModule imDecl) (importAs imDecl))

-- Filter given environment with one import specification from an import list
filterImportSpec :: Bool -> HxEnvironment -> ImportSpec l -> HxEnvironment
filterImportSpec isHiding exports iSpec =
  unionRelations [fSpec,fSub]
  where
  fSpec = restrictRng consider (restrictDom (== nameT) exports)
  allOwners = rng fSpec
  subs = restrictRng (`isOwned` allOwners) exports
  nameT = dropAnn name
  (name,fSub,noSubSpec) = case iSpec of
    IVar _ name -> (name, emptyRelation, True)
    IAbs _ name -> (name, emptyRelation, True)
    IThingAll _ name -> (name, subs, False)
    IThingWith _ name cNames -> 
      (name, restrictDom ((`elem` map getId cNames) . getId) subs, False)
  consider = if isHiding && noSubSpec then const True else notIsCon
   
-- -------------------------------------------------------------------------------------
-- Looking up, inserting and mutating individual environment entries.

-- For generating an appropriate error message.
one :: String -> [a] -> a
one msg [] = error (msg ++ " not found.")
one msg [x] = x
one msg _ = error (msg ++ " ambigious.")

-- Not a data constructor
lookupValueEnv :: Environment -> QName l -> Entity
lookupValueEnv env qName = 
  one ("Environment.lookupValueEnv: " ++ prettyPrint qName)
    (filter isValue . Set.toList . applyRelation env $ dropAnn qName)

-- Not a class
lookupTypeEnv :: Environment -> QName l -> Entity
lookupTypeEnv env qName =
  one ("Environment.lookupTypeEnv: " ++ prettyPrint qName)
    (filter isType . Set.toList . applyRelation env $ dropAnn qName)
    
arity :: Environment -> QName l -> Maybe Int
arity env qName = case lookupValueEnv env qName of
  (_,Value{args=a}) -> Just a
  _                 -> Nothing

typeSynonymBody :: Environment -> QName l -> Maybe TySynBody
typeSynonymBody env qName = case lookupTypeEnv env qName of
  (TypeClass _,TyCls (Syn _ tySynBody)) -> Just tySynBody
  _                                     -> Nothing

isLambdaBound :: Environment -> QName l -> Bool
isLambdaBound env qName = case lookupValueEnv env qName of
  (_,Value{letBound=b}) -> not b
  _                     -> error "Environment.isLambdaBound: failed."

isTracedQName :: Environment -> QName l -> Bool
isTracedQName env qName = case lookupValueEnv env qName of
  (_,Value{traced=b}) -> b
  _                   -> error "Environment.isTracedQName: failed."

isExpandableTypeSynonym :: Environment -> QName l -> Bool
isExpandableTypeSynonym env qName = case lookupTypeEnv env qName of
  (TypeClass _,TyCls (Syn n _)) -> n > 0
  _                             -> False

-- make given local variable let-bound
-- assumes name is for a local value variable
mutateLetBound :: Environment -> Name l -> Environment
mutateLetBound env name = 
  Map.adjust (Set.map mutate) (UnQual () (dropAnn name)) env
  where
  mutate :: Entity -> Entity
  mutate (i@(Environment.Var _),a@Value{}) = (i,a{letBound=True,args=0})
  mutate other = other

-- Obtain combined fixity and priority of local name.
fixPriority :: Environment -> Name l -> Int
fixPriority env name = 
  case lookupValueEnv env (UnQual () (dropAnn name)) of
    (_,Value{fixity=f,priority=p}) -> encode f p
    _                              -> 3 -- default fixity and priority
  where
  encode Def     _ = 3
  encode L       n = 2 + (n*4)
  encode R       n = 1 + (n*4)
  encode None    n = 0 + (n*4)
  encode (Pre _) n = 0 + (n*4)

-- Obtain priority of given local name. Result in 0-9.
hasPriority :: Environment -> Name l -> Int
hasPriority env name = 
  case lookupValueEnv env (UnQual () (dropAnn name)) of
    (_,Value{priority=p}) -> p
    _                     -> 9  -- default priority

clsTySynInfo :: Environment -> QName l -> TyCls
clsTySynInfo env qName = 
  one ("Environment.clsTySynInfo: " ++ prettyPrint qName)
    [tyCls | (TypeClass _, TyCls tyCls) <- 
               Set.toList (applyRelation env (dropAnn qName))]
