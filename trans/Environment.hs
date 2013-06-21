-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment,Identifier,AuxiliaryInfo,Entity,HxEntity
  ,lookupExpEnv, lookupTypeEnv, eArity, eNo, eBody, eCons, eFields
  ,isClass,isSyn,isType
  ,TySynBody(TApp,TFun,THelper,TVar)
  ,Scope(Local,Global),isLocal
  ,globalEnv,moduleDefines,prettyEnv
  ,declsEnv,maybeBindsEnv,bindsEnv, patsEnv
  ,arity,isLambdaBound,isTracedQName,fixPriority, hasPriority
  ,isExpandableTypeSynonym,typeSynonymBody
  ,nameTransTySynHelper,expandTypeSynonym
  ,imports,exports,hxEnvironmentToList,listToHxEnvironment
  ,defineNameEnv,env2Fixities,makeAllLambdaBound, lambdaVarEnv
  ,wiredEnv
  ) where

import Language.Haskell.Exts.Annotated hiding (Var,Con,Fixity,EVar)
import qualified Language.Haskell.Exts.Annotated as Syntax (Exp(Var,Con),Fixity(Fixity),ExportSpec(EVar))
import qualified Language.Haskell.Exts as Short(Assoc(..),QName(..),Name(..))
import SynHelp (Id(getId),getQualified,mkQual,qual,isQual,notSupported
               ,tyVarBind2Name,declHeadTyVarBinds,declHeadName,instHeadQName,getArityFromConDecl
               ,getConDeclFromQualConDecl,getConstructorFromConDecl,eqName,mkName,mkQName
               ,getFieldNamesFromConDecl,decomposeFunType,isFunTyCon,tyAppN,getModuleNameFromModule
               ,UpdId(updateId),dropAnn,noSpan)
import qualified Data.Set as Set
import qualified Data.Map as Map (singleton,adjust)
import Relation
import Data.Maybe (fromMaybe,fromJust)
import Data.List (nubBy)
import Data.Char (isAlpha)

import Debug.Trace

data Scope = Global | Local

isLocal :: Scope -> Bool
isLocal Local = True
isLocal Global = False

-- The following types for representing HxEntities are unnecessarily complicated and bizarr.
-- However, their Show instances are used in the hx-files, and hence any type changes
-- would mean changing the hx-file format and thus rebooting Hat from scratch.
-- Only do that when it becomes really necessary.
type HxEntity = (Identifier,AuxiliaryInfo)

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con TypeSort String{-type-} String{-con-}
		| Field String{-type-} String{-field-}
		| Method String{-class-} String{-method-}
                | TypeClass String
	deriving (Show,Read,Eq,Ord)
data TypeSort = Data | Newtype deriving (Show,Read,Eq,Ord)

toTypeSort :: DataOrNew l -> TypeSort
toTypeSort (DataType _) = Data
toTypeSort (NewType _) = Newtype

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

-- SrcSpanInfo for the definition of a name.
-- For variables and methods the span encompasses the complete definition (equations).
-- For data constructors and fields the span encompasses only the name itself.
-- From infix declarations and type signatures only an empty src can be determined.
-- Methods can have many spans, as there may be many definitions in a class and several instances.
-- Fields can have many spans, but only one is currently being used (dubious).
-- For types and classes there is no span.

data Entity 
  = EVar {eId :: String, eSrc :: SrcSpanInfo, eArity :: Int, eFixity :: Fixity, ePriority :: Int
         ,eLetBound :: Bool, eTraced :: Bool}
  | ECon {eId :: String, eSrc :: SrcSpanInfo, eArity :: Int, eFixity :: Fixity, ePriority :: Int
         ,eTraced :: Bool, eTy :: String, eTySort :: TypeSort, eFields :: [String]}
  | EField {eId :: String, eSrcs :: [SrcSpanInfo], eArity :: Int, eFixity :: Fixity, ePriority :: Int
           ,eTraced :: Bool, eTy :: String}
  | EMethod {eId :: String, eSrcs :: [SrcSpanInfo], eArity :: Int, eFixity :: Fixity, ePriority :: Int
            ,eTraced :: Bool, eCls :: String}
  | EType {eId :: String, eCons :: [String], eFields :: [String]}
  | ESyn {eId :: String, eNo :: Int, eBody :: TySynBody}
  | EClass {eId :: String, eMethods :: [String]}
  | ESig {eId :: String}  -- type signature for an expression identifier, needed for methods in classes
  | EInfix {eId :: String, eFixity :: Fixity, ePriority :: Int}  -- infix info for an expression identifier
  deriving (Eq, Ord, Show)

defaultVar :: Entity
defaultVar = EVar {eId = undefined, eSrc = noSpan, eArity = undefined, eFixity = Def
                  ,ePriority = 9, eLetBound = True, eTraced = undefined}

eVar :: String -> SrcSpanInfo -> Int -> Bool -> Entity
eVar id s a t = defaultVar {eId = id, eSrc = s, eArity = a, eTraced = t}

defaultCon :: Entity
defaultCon = ECon {eId = undefined, eSrc = noSpan, eArity = undefined, eFixity = Def
                  ,ePriority = 9, eTraced = undefined
                  ,eTy = undefined, eTySort = undefined, eFields = undefined}

eCon :: String -> SrcSpanInfo -> Int -> String -> TypeSort -> [String] -> Bool -> Entity
eCon id s a ty ts fields t = 
  defaultCon {eId = id, eSrc = s, eArity = a, eTy = ty, eTySort = ts, eFields = fields, eTraced = t}

defaultField :: Entity 
defaultField = EField {eId = undefined, eSrcs = [], eFixity = Def, ePriority = 9
                      ,eArity = 1, eTraced = False, eTy = undefined}

eField :: String -> SrcSpanInfo -> String -> Bool -> Entity
eField id s ty t = defaultField {eId = id, eSrcs = [s], eTy = ty, eTraced = t}

defaultMethod :: Entity
defaultMethod = EMethod {eId = undefined, eSrcs = [], eFixity = Def, ePriority = 9
                        ,eArity = -1, eTraced = False, eCls = undefined}

eMethod :: String -> [SrcSpanInfo] -> String -> Bool -> Entity
eMethod id s cls t = defaultMethod {eId = id, eSrcs = s, eCls = cls, eTraced = t}

eType :: String -> [String] -> [String] -> Entity
eType id cons fields = EType {eId = id, eCons = cons, eFields = fields}

eSyn :: String -> Int -> TySynBody -> Entity
eSyn id n body = ESyn {eId = id, eNo = n, eBody = body}

eClass :: String -> [String] -> Entity
eClass id methods = EClass {eId = id, eMethods = methods}

eSig :: String -> Entity
eSig id = ESig {eId = id}

eInfix :: String -> Fixity -> Int -> Entity
eInfix id f p = EInfix {eId = id, eFixity = f, ePriority = p}

type Environment = Relation (QName ()) Entity

isVar :: Entity -> Bool
isVar EVar{} = True
isVar _ = False

isCon :: Entity -> Bool
isCon ECon{} = True
isCon _ = False

isField :: Entity -> Bool
isField EField{} = True
isField _ = False

isMethod :: Entity -> Bool
isMethod EMethod{} = True
isMethod _ = False

isType :: Entity -> Bool
isType EType{} = True
isType _ = False

isSyn :: Entity -> Bool
isSyn ESyn{} = True
isSyn _ = False

isClass :: Entity -> Bool
isClass EClass{} = True
isClass _ = False

isSig :: Entity -> Bool
isSig ESig{} = True
isSig _ = False

isInfix :: Entity -> Bool
isInfix EInfix{} = True
isInfix _ = False

-- Is an expression entity, that is, variable, data constructor, method or field.
isExp :: Entity -> Bool
isExp e = isVar e || isCon e || isField e || isMethod e || isSig e || isInfix e

-- Is a type or type synoym.
isTySyn :: Entity -> Bool
isTySyn e = isType e || isSyn e

entity2HxEntity :: Entity -> HxEntity
entity2HxEntity e 
  | isVar e =
  (Var (eId e)
  ,Value{args = eArity e, fixity = eFixity e, priority = ePriority e, letBound = eLetBound e
        ,traced = eTraced e})
  | isCon e =
  (Con (eTySort e) (eTy e) (eId e)
  ,Value{args = eArity e, fixity = eFixity e, priority = ePriority e, letBound = True, traced = eTraced e})
  | isField e =
  (Field (eTy e) (eId e)
  ,Value{args = eArity e, fixity = eFixity e, priority = ePriority e, letBound = True, traced = eTraced e})
  | isMethod e =
  (Method (eCls e) (eId e)
  ,Value{args = eArity e, fixity = eFixity e, priority = ePriority e, letBound = True, traced = eTraced e})
  | isType e =
  (TypeClass (eId e), TyCls (Ty (eCons e) (eFields e)))
  | isSyn e =
  (TypeClass (eId e), TyCls (Syn (eNo e) (eBody e)))
  | isClass e =
  (TypeClass (eId e), TyCls (Cls (eMethods e)))
  | otherwise = error "Environment.entity2HxEntity: type signature or infix without definition."

hxEntity2Entity :: HxEntity -> Entity
hxEntity2Entity (Var id, val) = 
  EVar {eId = id, eSrc = noSpan, eArity = args val, eFixity = fixity val
       ,ePriority = priority val, eLetBound = letBound val, eTraced = traced val}
hxEntity2Entity (Con s t id, val) =
  ECon {eId = id, eSrc = noSpan, eArity = args val, eFixity = fixity val, ePriority = priority val
       ,eTraced = traced val, eTy = t, eTySort = s, eFields = []}
hxEntity2Entity (Field t id, val) =
  EField {eId = id, eSrcs = [], eArity = args val, eFixity = fixity val, ePriority = priority val
         ,eTraced = traced val, eTy = t}
hxEntity2Entity (Method cls id, val) =
  EMethod {eId = id, eSrcs = [], eArity = args val, eFixity = fixity val, ePriority = priority val
          ,eTraced = traced val, eCls = cls}
hxEntity2Entity (TypeClass id, TyCls (Ty cons fields)) = 
  EType {eId = id, eCons = cons, eFields = fields}
hxEntity2Entity (TypeClass id, TyCls (Syn n body)) =
  ESyn {eId = id, eNo = n, eBody = body}
hxEntity2Entity (TypeClass id, TyCls (Cls methods)) =
  EClass {eId = id, eMethods = methods}

-- Produce environment for one unqualified name.
singleton :: Entity -> Environment
singleton entity = Map.singleton (UnQual () (mkName () (eId entity))) (Set.singleton entity)

-- Produce environment of unqualified names.
fromList :: [Entity] -> Environment
fromList = listToRelation . map (\entity -> (UnQual () (mkName () (eId entity)),entity))


-- Is the given entity owned by any entity in the given set?
-- Ownership means constructor/field of a data type or method of a class.
isOwned :: Entity -> Set.Set Entity -> Bool
isOwned e owners 
  | isCon e = not . Set.null . Set.filter (isTypeId (eTy e)) $ owners
  | isField e = not . Set.null . Set.filter (isTypeId (eTy e)) $ owners
  | isMethod e = not . Set.null . Set.filter (isClassId (eCls e)) $ owners
  | otherwise = False

-- Check whether given entity is a type of given id.
isTypeId :: String -> Entity -> Bool
isTypeId id e = isType e && id == eId e

-- Check whether given entity is a class of given id.
isClassId :: String -> Entity -> Bool
isClassId id e = isClass e && id == eId e

-- -----------------------------------------------------------------------------------
-- Obtain an environment from parts of a syntax tree.

lambdaVarEnv :: Bool -> QName SrcSpanInfo -> Environment
lambdaVarEnv tracing id = singleton ((eVar (getId id) (ann id) 0 tracing) {eLetBound=False})

-- Create global environment of this module, given whether it is tracing (or trusted) and
-- the complete import environment.
globalEnv :: Bool -> Module SrcSpanInfo -> Environment -> Environment
globalEnv tracing mod importEnv = env
  where
  env = unionRelations [importEnv,unqualDefEnv,qualDefEnv]
  unqualDefEnv = moduleDefines tracing env mod -- tying knot of environments here
  qualDefEnv = qual (dropAnn (getModuleNameFromModule mod)) `mapDom` unqualDefEnv

-- Get the environment for all top-level definitions of a module.
-- Only produce unqualified QNames.
-- Take complete environment of module for lookups (cycle)
moduleDefines :: Bool -> Environment -> Module SrcSpanInfo -> Environment
moduleDefines tracing fullEnv (Module _ _ _ _ decls) = declsEnv tracing fullEnv decls

-- Determine local environment for given optional binds.
-- Post-condition: environment will only contain local variable bindings,
-- no types, classes, data constructors, fields or methods.
maybeBindsEnv :: Bool -> Maybe (Binds SrcSpanInfo) -> Environment
maybeBindsEnv tracing  = maybe emptyRelation (bindsEnv tracing)

-- Determine local environment for given binds.
-- Post-condition: environment will only contain local variable bindings,
-- no types, classes, data constructors, fields or methods.
bindsEnv :: Bool -> Binds SrcSpanInfo -> Environment
bindsEnv tracing (BDecls _ decls) = declsEnv tracing emptyRelation decls
bindsEnv _ (IPBinds l _) = notSupported l "binding group for implicit parameters"

-- Note that a fixity or type signature definition also yield an entity in the environment.
-- Such an entity needs to be merged with its main entity.
declsEnv :: Bool -> Environment -> [Decl SrcSpanInfo] -> Environment
declsEnv tracing fullEnv decls = unionRelationsWith mergeSets (map (declEnv tracing fullEnv) decls)
  where
  mergeSets :: Set.Set Entity -> Set.Set Entity -> Set.Set Entity
  mergeSets s1 s2 = Set.unions [vs,e1s,e2s]
    where
    (v1s,e1s) = Set.partition isExp s1
    (v2s,e2s) = Set.partition isExp s2
    vs = if Set.null v1s then v2s
         else if Set.null v2s then v1s
         else if Set.size v1s > 1 || Set.size v2s > 1 then error "Environment.declsEnv: conflicting expr names."
         else Set.singleton (mergeEntities (Set.findMin v1s) (Set.findMin v2s))
  -- pre-condition: both entities are expression entities.
  mergeEntities :: Entity -> Entity -> Entity
  mergeEntities e1 e2 
    | isSig e1 = e2
    | isSig e2 = e1
    | isInfix e1 = e2 {eFixity = eFixity e1, ePriority = ePriority e1}
    | isInfix e2 = e1 {eFixity = eFixity e2, ePriority = ePriority e2}    
    | (isMethod e1 && isMethod e2) || (isField e1 && isField e2) 
    = e1 {eSrcs = eSrcs e1 ++ eSrcs e2
         ,eFixity = eFixity e1 `mergeFixities` eFixity e2
         ,ePriority = ePriority e1 `mergePriorities` ePriority e2}
    | otherwise = error "Environment.mergeEntities: cannot be merged."
  mergeFixities :: Fixity -> Fixity -> Fixity
  mergeFixities Def f = f
  mergeFixities f Def = f
  mergeFixities _ _ = error "Environment.moduleDefines: conflicting fixities."
  mergePriorities :: Int -> Int -> Int
  mergePriorities p1 p2 | p1 == 9 = p2
                        | p2 == 9 = p1
                        | otherwise = 
                            error "Environment.moduleDefines: conflicting priorities."

declEnv :: Bool -> Environment -> Decl SrcSpanInfo -> Environment
declEnv _ fullEnv (TypeDecl _ declHead ty) =
  singleton (eSyn (getId tyName) n body)
  where
  tyName = declHeadName declHead
  Syn n body = splitSynonym fullEnv (map tyVarBind2Name (declHeadTyVarBinds declHead)) ty
declEnv _ _ (TypeFamDecl l declHead _) = 
  notSupported l "type family declaration"
declEnv tracing _ (DataDecl _ ts _ declHead qualConDecls _) = 
  fromList (eType (getId name) (map getId consNames) (map getId fieldNames) :
             [eCon (getId consName) (ann consName) (getArityFromConDecl conDecl) (getId name)
                (toTypeSort ts) (map getId (getFieldNamesFromConDecl conDecl)) tracing
             |conDecl <- conDecls, let consName = getConstructorFromConDecl conDecl] ++
             [eField (getId fieldName) (ann fieldName) (getId name) tracing 
             |fieldName <- fieldNames])
  where
  name = declHeadName declHead
  conDecls = map getConDeclFromQualConDecl qualConDecls
  consNames = map getConstructorFromConDecl conDecls
  fieldNames = nubBy eqName (concatMap getFieldNamesFromConDecl conDecls)
declEnv _ _ (GDataDecl l _ _ declHead _ gadtDecls _) =
  notSupported l "gadt declaration"
declEnv _ _ (DataFamDecl l declHead _ _) = 
  notSupported l "data family declaration"
declEnv _ _ (TypeInsDecl _ _ _) = emptyRelation
declEnv _ _ (DataInsDecl l _ _ _ _) = 
  notSupported l "data family instance declaration"
declEnv _ _ (GDataInsDecl l _ _ _ _ _) =
  notSupported l "gadt family instance declaration"
declEnv _ _ (ClassDecl _ _ declHead _ Nothing) = 
  singleton (eClass (getId className) [])
  where
  className = declHeadName declHead
declEnv tracing finalEnv (ClassDecl _ _ declHead _ (Just classDecls)) =
  unionRelations 
    [singleton (eClass classId methodIds), methodRelation]
  where
  methodIds = map eId . Set.toAscList . rng $ methodRelation
  methodRelation = sig2Method `mapRng` (declsEnv tracing finalEnv decls)
  classId = getId className
  className = declHeadName declHead
  sig2Method  :: Entity -> Entity
  sig2Method e | isSig e = eMethod (eId e) [] classId tracing 
               | isVar e = eMethod (eId e) [eSrc e] classId tracing
               | otherwise = error "Environment.declEnv: unexpected declaration in class."
  decls = map getDecl classDecls
  getDecl :: ClassDecl SrcSpanInfo -> Decl SrcSpanInfo
  getDecl (ClsDecl _ decl) = decl
  getDecl (ClsDataFam l _ _ _) = notSupported l "associated data type declaration"
  getDecl (ClsTyFam l _ _) = notSupported l "associated type synonym declaration"
  getDecl (ClsTyDef l _ _) = notSupported l 
                                 "default choice for an associated type synonym"
declEnv _ _ (InstDecl _ _ _ Nothing) = emptyRelation
declEnv tracing finalEnv (InstDecl _ _ instHead (Just instDecls)) =
  var2Method `mapRng` (declsEnv tracing finalEnv (map getDecl instDecls))
  where
  classId = getId (instHeadQName instHead)
  var2Method :: Entity -> Entity
  var2Method e | isVar e = eMethod (eId e) [eSrc e] classId tracing
               | otherwise = error "Environment.declEnv: unexpected declaration in instance."
  getDecl :: InstDecl SrcSpanInfo -> Decl SrcSpanInfo
  getDecl (InsDecl _ decl) = decl
  getDecl (InsType l _ _) = notSupported l "associated type definition"
  getDecl (InsData l _ _ _ _) = notSupported l "associated data type implementation"
  getDecl (InsGData l _ _ _ _ _) = notSupported l "GADT style assoicated data type type implementation"
declEnv _ _ (DerivDecl _ _ _) = emptyRelation
declEnv tracing _ (InfixDecl _ assoc maybePri ops) =
  fromList (map op2Entity ops)
  -- Environment only has valid entity information for fixity and priority,
  -- otherwise default values.
  where
  op2Entity :: Op l -> Entity
  op2Entity (VarOp _ name) = eInfix (getId name) fixityVal priorityVal
  opToQNameEntity (ConOp _ name) = eInfix (getId name) fixityVal priorityVal
    -- Use Var here as well, as other arguments for Con are unknown here.
  fixityVal = case assoc of
                AssocNone _ -> None
                AssocLeft _ -> L
                AssocRight _ -> R
  priorityVal = fromMaybe 9 maybePri
declEnv _ _ (DefaultDecl _ _) = emptyRelation
declEnv _ _ (SpliceDecl l _) = 
  notSupported l "splice declaration"
declEnv tracing _ (TypeSig _ names _) =
  fromList [eSig (getId name) | name <- names]
  -- needed for methods in classes
declEnv tracing _ (FunBind l matches) = matchEnv l tracing (head matches)
declEnv tracing _ (PatBind l pat _ _ _) = patEnv l tracing pat
declEnv tracing _ (ForImp l _ _ _ name ty) =
  -- only for NoHat. import
  singleton (eVar (getId name) l (length tyArgs) tracing)
  where
  (tyArgs,_) = decomposeFunType ty
declEnv _ _ (ForExp _ _ _ _ _) = emptyRelation
declEnv _ _ (RulePragmaDecl _ _) = emptyRelation
declEnv _ _ (DeprPragmaDecl _ _) = emptyRelation
declEnv _ _ (WarnPragmaDecl _ _) = emptyRelation
declEnv _ _ (InlineSig _ _ _ _) = emptyRelation
declEnv _ _ (InlineConlikeSig _ _ _) = emptyRelation
declEnv _ _ (SpecSig _ _ _) = emptyRelation
declEnv _ _ (SpecInlineSig _ _ _ _ _) = emptyRelation
declEnv _ _ (InstSig _ _ _) = emptyRelation
declEnv _ _ (AnnPragma _ _) = emptyRelation


matchEnv :: SrcSpanInfo -> Bool -> Match SrcSpanInfo -> Environment
matchEnv l tracing (Match _ name pats _ _) = 
  singleton (eVar (getId name) l (length pats) tracing)
matchEnv l tracing (InfixMatch _ pat name pats rhs maybeBinds) =
  matchEnv l tracing (Match l name (pat:pats) rhs maybeBinds)

patsEnv :: SrcSpanInfo -> Bool -> [Pat SrcSpanInfo] -> Environment
patsEnv l tracing pats = unionRelations (map (patEnv l tracing) pats)

-- All occurring variables are let-bound, because this function is for 
-- pattern bindings.
patEnv :: SrcSpanInfo -> Bool -> Pat SrcSpanInfo -> Environment
patEnv l tracing (PVar _ name) = 
  singleton (eVar (getId name) l 0 tracing)
patEnv _ _ (PLit _ _) = emptyRelation
patEnv l tracing (PNeg _ pat) = patEnv l tracing pat
patEnv l tracing (PNPlusK l2 name _) = patEnv l tracing (PVar l2 name)
patEnv l tracing (PInfixApp _ patl _ patr) = unionRelations [patEnv l tracing patl, patEnv l tracing patr]
patEnv l tracing (PApp _ _ pats) = unionRelations . map (patEnv l tracing) $ pats
patEnv l tracing (PTuple _ pats) = unionRelations . map (patEnv l tracing) $ pats
patEnv l tracing (PList _ pats) = unionRelations . map (patEnv l tracing) $ pats
patEnv l tracing (PParen _ pat) = patEnv l tracing pat
patEnv l tracing (PRec _ _ patFields) = unionRelations . map (patField l tracing) $ patFields
patEnv l tracing (PAsPat _ name pat) = 
  unionRelations [singleton (eVar (getId name) (ann name) 0 tracing), patEnv l tracing pat]
patEnv _ _ (PWildCard _) = emptyRelation
patEnv l tracing (PIrrPat _ pat) = patEnv l tracing pat
patEnv l tracing (PatTypeSig _ pat _) = patEnv l tracing pat
patEnv l tracing (PViewPat _ _ pat) = patEnv l tracing pat
patEnv _ _ (PRPat l _) = notSupported l "regular list pattern"
patEnv _ _ (PXTag l _ _ _ _) = notSupported l "XML element pattern"
patEnv _ _ (PXETag l _ _ _) = notSupported l "XML singleton element pattern"
patEnv _ _ (PXPcdata l _) = notSupported l "XML PCDATA pattern"
patEnv _ _ (PXPatTag l _) = notSupported l "XML embedded pattern"
patEnv _ _ (PXRPats l _) = notSupported l "XML regular list pattern"
patEnv _ _ (PExplTypeArg l _ _) = notSupported l "explicit generics style type argument"
patEnv _ _ (PQuasiQuote l _ _) = notSupported l "quasi quote pattern"
patEnv l tracing (PBangPat _ pat) = patEnv l tracing pat

patField :: SrcSpanInfo -> Bool -> PatField SrcSpanInfo -> Environment
patField l tracing (PFieldPat _ _ pat) = patEnv l tracing pat
patField _ _ (PFieldPun l _) = notSupported l "field pun"
patField _ _ (PFieldWildcard _) = emptyRelation

-- -----------------------------------------------------------------------------------
-- Determine export and import environments

-- In the hx-file of a module only unqualified names are used 
type HxEnvironment = Relation (Name ()) Entity  -- Note: not HxEntity for simplicity

-- Determine the exports of a module
exports :: Bool -> Module SrcSpanInfo -> Environment -> HxEnvironment
exports tracing mod@(Module l maybeModuleHead _ _ _) env =
  case maybeModuleHead of
    Nothing -> exportList [Syntax.EVar l (UnQual l (Ident l "main"))]
    Just (ModuleHead _ _ _ Nothing) -> getQualified `mapDom` moduleDefines tracing env mod
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
filterExportSpec env eSpec = unionRelations [mSpec, mSub]
  where
  mSpec = restrictRng (not . isCon) (restrictDom (== qNameT) env)
  allOwners = rng mSpec
  subs = restrictRng (`isOwned` allOwners) env
  qNameT = dropAnn qName
  (qName,mSub) = case eSpec of
    Syntax.EVar _ qName -> (qName, emptyRelation)
    EAbs _ qName -> (qName, emptyRelation)
    EThingAll _ qName -> (qName, subs)
    EThingWith _ qName cNames -> 
      (qName, restrictDom ((`elem` map getId cNames) . getId) subs)

-- Assumes that list is in ascending order without duplicate names.
listToHxEnvironment :: [HxEntity] -> HxEnvironment
listToHxEnvironment =  (getQualified `mapDom`) . fromList . map hxEntity2Entity

hxEnvironmentToList :: HxEnvironment -> [HxEntity]
hxEnvironmentToList = map entity2HxEntity . Set.toAscList . Relation.rng

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
  consider = if isHiding && noSubSpec then const True else (not . isCon)
   
-- -------------------------------------------------------------------------------------
-- Handling type synonym

-- determines the outer functional or applicative part of a type synonym body
-- this part can then be expanded when transforming types for workers
-- the *final* environment is needed, because in the body of a type synonym
-- a type synonym may appear; because type synonyms shall not be recursive,
-- a blackhole cannot occur for type correct programs.
splitSynonym :: SrcInfo l => Environment -> [Name l] -> Type l -> TyCls
splitSynonym env tyVars rhs =
  case go rhs [] of
    Syn 1 THelper -> Syn 0 THelper  -- nothing to split off (bogus THelper)
    syn -> syn
  where
  -- it is vital that this 'go' agrees with the 'go' in 'splitSynonym' in TraceTrans.
  go ty@(TyForall _ _ _ _) [] = Syn 1 THelper
  go (TyFun _ tyL tyR) [] = let Syn h tyR' = go tyR []
                            in Syn (h+1) (TApp (TApp TFun THelper) tyR')
  go (TyTuple _ _ _) [] = Syn 1 THelper
  go (TyList _ _) [] = Syn 1 THelper
  go (TyApp _ tyL tyR) tys = go tyL (tyR:tys)
  go (TyVar _ tyVar) tys = Syn 0 (TVar (fromJust (elemIndexBy eqName tyVar tyVars)))
  go (TyCon _ tyCon) tys
    | isFunTyCon tyCon = case tys of
                           [] -> Syn 1 THelper
                           [ty] -> Syn 1 (TApp TFun THelper)
                           [ty1,ty2] -> let Syn h tyR' = go ty2 []
                                        in Syn (h+1) (TApp (TApp TFun THelper) tyR')
    | isExpandableTypeSynonym env tyCon
    = go (expandTypeSynonym env tyCon tys) []
    | otherwise = Syn 1 THelper
  go (TyParen _ ty) tys = go ty tys
  go (TyInfix _ tyL tyCon tyR) tys =
    if isExpandableTypeSynonym env tyCon
      then go (expandTypeSynonym env tyCon (tyL:tyR:tys)) []
      else Syn 1 THelper
  go (TyKind l ty kind) _ = notSupported l "kind annotation in type synonym"


elemIndexBy :: (a -> a -> Bool) -> a -> [a] -> Maybe Int
elemIndexBy eq x xs = go 0 xs
  where
  go c [] = Nothing
  go c (y:ys) = if x `eq` y then Just c else go (c+1) ys 

-- Expand only to expose function type constructors.
-- Uses the helper type synonyms introduced by the transformation.
expandTypeSynonym :: SrcInfo l => Environment -> QName l -> [Type l] -> Type l
expandTypeSynonym env tySyn tys =
  case typeSynonymBody env tySyn of
    Nothing -> error ("TraceTrans.expandTypeSynonym: " ++ show (getId tySyn) ++
                      " is not a type synonym.")
    Just body -> fst (go body 1)
  where
  l = ann tySyn
  -- go :: TySynBody -> Int -> (Type l, Int)
  go THelper n = (tyAppN (TyCon l (nameTransTySynHelper tySyn n) : tys), n+1)
  go (TVar v) n = (tys!!v, n)
  go TFun n = (TyCon l (Special l (FunCon l)), n)
  go (TApp tyL tyR) n = (TyApp l tyL' tyR', n2)
    where
    (tyL', n1) = go tyL n
    (tyR', n2) = go tyR n1

-- Names of helper synonyms are a bit of a hack; a name conflict is possible.
-- We just do not want to prefix all names in the namespace.
nameTransTySynHelper :: UpdId i => i -> Int -> i
nameTransTySynHelper syn no = updateId update syn
  where 
  update (Ident l name) = Ident l (name ++ "___" ++ show no)
  update (Symbol _ _) = 
    error "TraceTrans.nameTransTySynHelper: synonym name is a symbol."


-- -------------------------------------------------------------------------------------
-- Looking up, inserting and mutating individual environment entries.

-- For generating an appropriate error message.
one :: Environment -> String -> [a] -> a
one env msg [] = error (prettyEnv env ++ msg ++ " not found.")
one env msg [x] = x
one env msg _ = error (prettyEnv env ++ msg ++ " ambigious.")

prettyEnv :: (Pretty a, Ord a) => Relation a b -> String
prettyEnv = ("Environment: " ++) . foldr space "\n" . map prettyPrint . Set.toAscList . Relation.dom
  where
  space xs ys = xs ++ "," ++ ys

-- A name for a variable, data constructor, method or field:
lookupExpEnv :: Environment -> QName l -> Entity
lookupExpEnv env qName = 
  one env ("Environment.lookupExpEnv: `" ++ prettyPrint qName ++ "'")
    (filter isExp . Set.toList . applyRelation env $ dropAnn qName)

-- Not a class
lookupTypeEnv :: Environment -> QName l -> Entity
lookupTypeEnv env qName =
  one env ("Environment.lookupTypeEnv: `" ++ prettyPrint qName ++ "'")
    (filter isTySyn . Set.toList . applyRelation env $ dropAnn qName)

-- Identifiers defined by pattern binding have arity 0.
ar :: AuxiliaryInfo -> Int
ar Value{args=a} = a
ar _             = 0
    
arity :: Environment -> QName l -> Maybe Int
arity env qName = Just (eArity (lookupExpEnv env qName))

typeSynonymBody :: Environment -> QName l -> Maybe TySynBody
typeSynonymBody env qName = if isSyn entity then Just (eBody entity) else Nothing
  where
  entity = lookupTypeEnv env qName

isLambdaBound :: Environment -> QName l -> Bool
isLambdaBound env qName = if isVar entity then not (eLetBound entity) else False
  where 
  entity = lookupExpEnv env qName

isTracedQName :: Environment -> QName l -> Bool
isTracedQName env qName = eTraced (lookupExpEnv env qName)

isExpandableTypeSynonym :: Environment -> QName l -> Bool
isExpandableTypeSynonym env qName = isSyn entity && eNo entity > 0
  where
  entity = lookupTypeEnv env qName

-- Assumes all entities of given environment are expression variables.
makeAllLambdaBound :: Environment -> Environment
makeAllLambdaBound = mapRng (\e -> e {eLetBound = False})

-- Obtain combined fixity and priority
eFixPriority :: Entity -> Int
eFixPriority e = encode (eFixity e) (ePriority e)
  where
  encode Def     _ = 3
  encode L       n = 2 + (n*4)
  encode R       n = 1 + (n*4)
  encode None    n = 0 + (n*4)
  encode (Pre _) n = 0 + (n*4)
-- fixPriority _ = 3 -- default fixity and priority

fixPriority :: Environment -> QName l -> Int
fixPriority env qName = eFixPriority (lookupExpEnv env qName)

-- Obtain priority of given local name. Result in 0-9.
hasPriority :: Environment -> Name l -> Int
hasPriority env name = ePriority (lookupExpEnv env (UnQual () (dropAnn name)))

defineNameEnv :: Scope -> Environment -> (Name SrcSpanInfo -> Int -> Int -> Scope -> Scope -> a) -> 
  (Name SrcSpanInfo -> [Name SrcSpanInfo] -> Int -> Int -> a) -> [a]
defineNameEnv scope env defNameVar defNameCon = concatMap define nameEntries
  where
  hxEnv = getQualified `mapDom` env
  nameEntries = relationToList hxEnv
  define (name, e) 
    | isVar e && eLetBound e =
    [defNameVar (fmap (const (eSrc e)) name) (eFixPriority e) (eArity e) scope scope]
    | isCon e =
    [defNameCon (fmap (const (eSrc e)) name) (map (mkName noSpan) (eFields e)) (eFixPriority e) (eArity e)]
    | isField e =
    [defNameVar (fmap (const (head (eSrcs e))) name) (eFixPriority e) (eArity e) Global Global]
    | isMethod e =
    map (\l -> defNameVar (fmap (const l) name) (eFixPriority e) (eArity e) Global Local) (eSrcs e)
    | otherwise = []  -- for types and classes


-- ----------------
-- Obtain all fixities from a given enviroment, suitable for fixing the parse tree.
-- Don't include identifiers that just have default fixity.
env2Fixities :: Environment -> [Syntax.Fixity]
env2Fixities env = 
  map makeFixity . filter (\entity -> isExp entity && eFixity entity /= Def) . Set.toAscList . rng $ env
  where
  makeFixity :: Entity -> Syntax.Fixity
  makeFixity e = Syntax.Fixity (transFixity (eFixity e)) (ePriority e) (Short.UnQual (mkName (eId e)))
    where
    transFixity :: Fixity -> Short.Assoc
    transFixity None = Short.AssocNone
    transFixity L = Short.AssocLeft
    transFixity R = Short.AssocRight
    transFixity _ = error "Environment.env2Fixities: unexpected associativity."
    mkName :: String -> Short.Name
    mkName s = if isAlpha (head s) then Short.Ident s else Short.Symbol s


-- -------------------------
-- All identifiers whose bindings are fixed by the Haskell language
wiredEnv :: Environment
wiredEnv = listToRelation $
  [(Special () (UnitCon ()), eCon "()" noSpan 0 "()" Data [] False)
  ,(Special () (UnitCon ()), eType "()" ["()"] [])
  ,(Special () (ListCon ()), eType "[]" [":","[]"] [])
  ,(Special () (FunCon ()), eType "->" [] [])
  ,(Special () (Cons ()), eCon ":" noSpan 2 "[]" Data [] False)]
  ++ concatMap mkTuple [2..12]

mkTuple :: Int -> [(QName (), Entity)]
mkTuple n = [(Special () (TupleCon () Boxed n), eCon commas noSpan n commas Data [] False)
            ,(Special () (TupleCon () Boxed n), eType commas [commas] [])]
  where
  commas = replicate (n-1) ',' 