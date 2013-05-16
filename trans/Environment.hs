-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment,Identifier,AuxiliaryInfo
  ,TySynBody(TApp,TFun,THelper,TVar),TyCls(Ty,Cls,Syn)
  ,arity,isLambdaBound,isTracedQName,mutateLetBound,fixPriority, hasPriority
  ,clsTySynInfo,isExpandableTypeSynonym,typeSynonymBody
  ,nameTransTySynHelper,expandTypeSynonym
  ) where

import Language.Haskell.Exts.Annotated hiding (Var,Con)
import SynHelp (Id(getId),getQualified,mkQual,qual,isQual,notSupported
               ,tyVarBind2Name,declHeadTyVarBinds,declHeadName,getArityFromConDecl
               ,getConDeclFromQualConDecl,getConstructorFromConDecl,eqName
               ,getFieldNamesFromConDecl,decomposeFunType,isFunTyCon,tyAppN
               ,UpdId(updateId))
import qualified Data.Set as Set
import qualified Data.Map as Map (singleton,adjust)
import Relation
import Data.Maybe (fromMaybe,fromJust)
import Data.List (nubBy,elemIndex)

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

toTypeSort :: DataOrNew l -> TypeSort
toTypeSort (DataType _) = Data
toTypeSort (NewType _) = Newtype

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

singleton :: Name l -> Entity -> Environment
singleton name entity = Map.singleton (UnQual () (dropAnn name)) (Set.singleton entity)

fromList :: [(Name l,Entity)] -> Environment
fromList = listToRelation . map (\(n,e) -> (UnQual () (dropAnn n),e))

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
-- Take complete environment of module for lookups (cycle)
moduleDefines :: (SrcInfo l, Eq l) => Environment -> Module l -> Environment
moduleDefines fullEnv (Module _ _ _ _ decls) = declsEnv fullEnv decls

declsEnv :: (SrcInfo l, Eq l) => Environment -> [Decl l] -> Environment
declsEnv fullEnv decls = unionRelationsWith mergeEntities (map (declEnv fullEnv) decls)
  where
  mergeEntities :: Entity -> Entity -> Entity
  mergeEntities (Var name, value1) (Var _, value2) = 
    (Var name, value1 `mergeValues` value2) 
  mergeEntities (Var name, value1) (Con tySort ty _, value2) =
    (Con tySort ty name, value1 `mergeValues` value2)
  mergeEntities (Var name, value1) (Field ty _, value2) =
    (Field ty name, value1 `mergeValues` value2)
  mergeEntities (Var name, value1) (Method cls _, value2) =
    (Method cls name, value1 `mergeValues` value2)
  mergeEntities entity1 entity2@(Var _, _) = mergeEntities entity2 entity1
  mergeEntities _ _ = mergeError
  mergeValues :: AuxiliaryInfo -> AuxiliaryInfo -> AuxiliaryInfo
  mergeValues (TyCls _) _ = mergeError
  mergeValues _ (TyCls _) = mergeError
  mergeValues val1 val2 =
    Value {args = args val1 `mergeArgs` args val2
          ,fixity = fixity val1 `mergeFixities` fixity val2
          ,priority = priority val1 `mergePriorities` priority val2
          ,letBound = letBound val1 && letBound val2
          ,traced = traced val1 && traced val2}
  mergeArgs :: Int -> Int -> Int
  mergeArgs a1 a2 | a1 < 0 = a2
                  | a2 < 0 = a1
                  | otherwise = error "Environment.moduleDefines: conflicting arities."
  mergeFixities :: Environment.Fixity -> Environment.Fixity -> Environment.Fixity
  mergeFixities Def f = f
  mergeFixities f Def = f
  mergeFixities _ _ = error "Environment.moduleDefines: conflicting fixities."
  mergePriorities :: Int -> Int -> Int
  mergePriorities p1 p2 | p1 == 9 = p2
                        | p2 == 9 = p1
                        | otherwise = 
                            error "Environment.moduleDefines: conflicting priorities."
  mergeError = 
    error "Environment.moduleDefines: same name for type/class and other entity."

declEnv :: (SrcInfo l, Eq l) => Environment -> Decl l -> Environment
declEnv fullEnv (TypeDecl _ declHead ty) =
  singleton tyName 
    (TypeClass (getId tyName)
    ,TyCls (splitSynonym fullEnv (map tyVarBind2Name (declHeadTyVarBinds declHead)) ty))
  where
  tyName = declHeadName declHead
declEnv _ (TypeFamDecl l declHead _) = 
  notSupported l "type family declaration"
declEnv _ (DataDecl _ ts _ declHead qualConDecls _) = 
  fromList ((name, (TypeClass (getId name)
                  ,TyCls (Ty (map getId consNames) (map getId fieldNames)))) :
           [(consName
            , (Environment.Con (toTypeSort ts) (getId name) (getId consName)
              ,Value {args = getArityFromConDecl conDecl 
                     ,fixity = Def, priority = 9
                     ,letBound = True, traced = True}))
             | conDecl <- conDecls, let consName = getConstructorFromConDecl conDecl])
  where
  name = declHeadName declHead
  conDecls = map getConDeclFromQualConDecl qualConDecls
  consNames = map getConstructorFromConDecl conDecls
  fieldNames = nubBy eqName (concatMap getFieldNamesFromConDecl conDecls)
declEnv _ (GDataDecl l _ _ declHead _ gadtDecls _) =
  notSupported l "gadt declaration"
declEnv _ (DataFamDecl l declHead _ _) = 
  notSupported l "data family declaration"
declEnv _ (TypeInsDecl _ _ _) = emptyRelation
declEnv _ (DataInsDecl l _ _ _ _) = 
  notSupported l "data family instance declaration"
declEnv _ (GDataInsDecl l _ _ _ _ _) =
  notSupported l "gadt family instance declaration"
declEnv _ (ClassDecl _ _ declHead _ Nothing) = emptyRelation
declEnv finalEnv (ClassDecl _ _ declHead _ (Just classDecls)) =
  varToMethod `mapRng` (declsEnv finalEnv decls)
  where
  classId = getId (declHeadName declHead)
  varToMethod  :: Entity -> Entity
  varToMethod (Var id, value) = (Method classId id, value{args = -1})
  decls = map getDecl classDecls
  getDecl :: SrcInfo l => ClassDecl l -> Decl l
  getDecl (ClsDecl _ decl) = decl
  getDecl (ClsDataFam l _ _ _) = notSupported l "associated data type declaration"
  getDecl (ClsTyFam l _ _) = notSupported l "associated type synonym declaration"
  getDecl (ClsTyDef l _ _) = notSupported l 
                                 "default choice for an associated type synonym"
declEnv _ (InstDecl _ _ _ _) = emptyRelation
declEnv _ (DerivDecl _ _ _) = emptyRelation
declEnv _ (InfixDecl _ assoc maybePri ops) =
  fromList (map opToQNameEntity ops)
  -- Environment only has valid entity information for fixity and priority,
  -- otherwise default values.
  where
  opToQNameEntity :: Op l -> (Name l, Entity)
  opToQNameEntity (VarOp _ name) =
    (name
    ,(Var (getId name)
     ,Value {args = -1,fixity = fixityVal, priority = priorityVal
            ,letBound = True, traced = True}))
  opToQNameEntity (ConOp l name) = opToQNameEntity (VarOp l name)
    -- Use Var here as well, as other arguments for Con are unknown here.
  fixityVal = case assoc of
                AssocNone _ -> None
                AssocLeft _ -> L
                AssocRight _ -> R
  priorityVal = fromMaybe 9 maybePri
declEnv _ (DefaultDecl _ _) = emptyRelation
declEnv _ (SpliceDecl l _) = 
  notSupported l "splice declaration"
declEnv _ (TypeSig _ names _) =
  fromList [(name
           ,(Var (getId name), Value{args = -1, fixity = Def, priority = 9
                                    ,letBound = True, traced = True}))
           | name <- names]
  -- needed for methods in classes
declEnv _ (FunBind _ matches) = matchEnv (head matches)
declEnv _ (PatBind _ pat _ _ _) = patEnv pat
declEnv _ (ForImp l _ _ _ name ty) =
  -- only for NoHat. import
  singleton name
    (Var (getId name), Value{args = length tyArgs, fixity = Def, priority = 9
                            ,letBound = True, traced = True})
  where
  (tyArgs,_) = decomposeFunType ty
declEnv _ (ForExp _ _ _ _ _) = emptyRelation
declEnv _ (RulePragmaDecl _ _) = emptyRelation
declEnv _ (DeprPragmaDecl _ _) = emptyRelation
declEnv _ (WarnPragmaDecl _ _) = emptyRelation
declEnv _ (InlineSig _ _ _ _) = emptyRelation
declEnv _ (InlineConlikeSig _ _ _) = emptyRelation
declEnv _ (SpecSig _ _ _) = emptyRelation
declEnv _ (SpecInlineSig _ _ _ _ _) = emptyRelation
declEnv _ (InstSig _ _ _) = emptyRelation
declEnv _ (AnnPragma _ _) = emptyRelation


matchEnv :: Match l -> Environment
matchEnv (Match l name pats _ _) = 
  singleton name
    (Var (getId name),Value{args = length pats, fixity = Def, priority = 9
                           ,letBound = True, traced = True})
matchEnv (InfixMatch l pat name pats rhs maybeBinds) =
  matchEnv (Match l name (pat:pats) rhs maybeBinds)

patEnv :: SrcInfo l => Pat l -> Environment
patEnv (PVar l name) = 
  singleton name
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
patEnv (PRec _ _ patFields) = unionRelations . map patField $ patFields
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

patField :: SrcInfo l => PatField l -> Environment
patField (PFieldPat _ _ pat) = patEnv pat
patField (PFieldPun l _) = notSupported l "field pun"
patField (PFieldWildcard _) = emptyRelation

-- -----------------------------------------------------------------------------------
-- Determine export and import environments

-- In the hx-file of a module only unqualified names are used 
type HxEnvironment = Relation (Name ()) Entity

-- Determine the exports of a module
exports :: (SrcInfo l, Eq l) => Module l -> Environment -> HxEnvironment
exports mod@(Module l maybeModuleHead _ _ _) env =
  case maybeModuleHead of
    Nothing -> exportList [EVar l (UnQual l (Ident l "main"))]
    Just (ModuleHead _ _ _ Nothing) -> getQualified `mapDom` moduleDefines env mod
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
-- Handling type synonym

-- determines the outer functional or applicative part of a type synonym body
-- this part can then be expanded when transforming types for workers
-- the *final* environment is needed, because in the body of a type synonym
-- a type synonym may appear; because type synonyms shall not be recursive,
-- a blackhole cannot occur for type correct programs.
splitSynonym :: (SrcInfo l,Eq l) => Environment -> [Name l] -> Type l -> TyCls
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
  go (TyVar _ tyVar) tys = Syn 1 (TVar (fromJust (elemIndex tyVar tyVars)))
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
    error "TraceTrans, nameTransTySynHelper: synom name is a symbol"


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
