{- ---------------------------------------------------------------------------
Transform a module for generating a trace.

Names are changed.
Module names are prefixed by 'Hat.'.
Variable names are prefixed to make room for new variable names 
refering to various traces and intermediate expressions.
Details of new name scheme near the end of this module.

No monad is used in the transformation, 
because there is nothing inherently sequential.
Instead, the definitions of the transformation functions `t*' remind of an 
attribut grammar: the arguments are the inherited attributes, the elements
of the result tuples are the synthetic attributes.
---------------------------------------------------------------------------- -}

module TraceTrans (traceTrans, Tracing(..)) where

import Language.Haskell.Exts.Annotated
import System.FilePath (FilePath,takeBaseName)
import Data.Maybe (fromMaybe,isNothing,isJust,fromJust)
import Data.List (stripPrefix,nubBy,partition)
import Data.Char (digitToInt,isAlpha)
import Data.Ratio (numerator,denominator)
import Data.Set (Set)
import qualified Data.Set as Set
import Relation (emptyRelation,unionLocalRelation)
import Environment (Environment,Scope(..),isLocal
                   ,TySynBody(TApp,TFun,THelper,TVar)
                   ,lookupExpEnv, lookupTypeEnv
                   ,eArity, eNo, eBody, eCons, eFields
                   ,Entity,isClass, isSyn, isType
                   ,arity,isLambdaBound,isTracedQName
                   ,fixPriority
                   ,isExpandableTypeSynonym,typeSynonymBody
                   ,nameTransTySynHelper,expandTypeSynonym
                   ,declsEnv,instanceEnv,moduleDefines,maybeBindsEnv,bindsEnv,patsEnv,defineNameEnv
                   ,lambdaVarEnv,makeAllLambdaBound)
import Wired
import SynHelp
import Derive (derive)

import Environment (prettyEnv)
import Debug.Trace

-- ----------------------------------------------------------------------------
-- central types

data Tracing = Traced | Trusted deriving Eq

isTraced :: Tracing -> Bool
isTraced Traced = True
isTraced Trusted = False

-- ----------------------------------------------------------------------------
-- Transform a module

traceTrans :: 
  FilePath -> -- complete filename of the module (essential for module Main)
  Tracing -> -- whether transforming for tracing or trusting
  Environment -> -- contains already info about all imported identifiers
  Module SrcSpanInfo -> 
  Module SrcSpanInfo  -- some srcSpanInfo will be fake
traceTrans moduleFilename tracing env
  mod@(Module span maybeModuleHead modulePragmas impDecls decls) =
  Module span (adaptMain (fmap (tModuleHead env declsExported) maybeModuleHead))
    (map tModulePragma modulePragmas) 
    (tImpDecls env impDecls)
    (declsExported ++
      [PatBind span patParent Nothing 
        (UnGuardedRhs span expRoot) Nothing] ++
      [defNameMod modName moduleFilename tracing] ++ 
      -- map (defNameVar env Global Local modTrace) mvars ++ 
      -- map (defNameVar env Local Local modTrace) vars ++ 
      map (\f -> f modTrace) defFuns ++ 
      (if isTraced tracing then map (defNameSpan modTrace) ls else []))
  where
  modName = getModuleNameFromModule mod
  declsExported = decls' ++ -- conNameDefs ++ globalVarNameDefs ++
                    if isMain modName
                      then [defMain tracing (traceBaseFilename moduleFilename)] 
                      else []
  -- conNameDefs = map (defNameCon env modTrace) cons 
  -- globalVarNameDefs = map (defNameVar env Global Global modTrace) tvars 
  modName' = nameTransModule modName
  modTrace = Var span (UnQual span (nameTraceInfoModule modName))
  -- (poss,tvars,vars,mvars,cons) = getModuleConsts consts
  (decls',consts) = tDecls env Global tracing decls
  definedEnv = moduleDefines (isTraced tracing) env mod
  (ls,defFuns) = moduleConstsGet (moduleConstsEnv Global definedEnv `moduleConstsUnion` consts)
  adaptMain maybeModuleHead = if isMain modName then Nothing else maybeModuleHead
traceTrans _ _ _ (XmlPage span _ _ _ _ _ _) = notSupported span "XmlPage"
traceTrans _ _ _ (XmlHybrid span _ _ _ _ _ _ _ _) = notSupported span "XmlHybrid"

-- For the complete filename of a module yields the base filename for the 
-- trace file.
-- Pre-condition: The module is a Main module
traceBaseFilename :: FilePath -> FilePath
traceBaseFilename = takeBaseName

-- obtain the module identifier
getModuleId :: ModuleHead l -> String
getModuleId (ModuleHead _ (ModuleName _ modId) _ _) = modId

tModuleHead :: Environment -> 
               [Decl SrcSpanInfo] ->  -- transformed declarations of this module
               ModuleHead SrcSpanInfo -> ModuleHead SrcSpanInfo
tModuleHead env decls
  (ModuleHead span moduleName maybeWarningText maybeExportSpecList) =
  ModuleHead span (nameTransModule moduleName) 
    (fmap tWarningText maybeWarningText)
    (tMaybeExportSpecList ((\(ModuleName _ modId) -> modId) moduleName)
      env maybeExportSpecList decls)

-- warnings stay unchanged
tWarningText :: WarningText l -> WarningText l
tWarningText w = w

-- not all module pragmas can be transformed
tModulePragma :: ModulePragma SrcSpanInfo -> ModulePragma SrcSpanInfo
tModulePragma (LanguagePragma l names) = LanguagePragma l names
tModulePragma (OptionsPragma l maybeTool string) = 
  OptionsPragma l maybeTool string
tModulePragma (AnnModulePragma l _) = 
  notSupported l "ANN pragma with module scope"

-- ----------------------------------------------------------------------------
-- construct new main function definition

-- main = T.traceIO "artFilename" gmain
--  equivalent to
-- main = do
--  T.openTrace "artFilename"
--  case omain Prelude.undefined Prelude.undefined of
--    T.R v _ -> v
--  T.closeTrace
defMain :: Tracing -> String -> Decl SrcSpanInfo
defMain tracing artFileName =
  PatBind noSpan (PVar noSpan (nameMain noSpan)) Nothing
    (UnGuardedRhs noSpan (appN
       [expTraceIO
       ,litString noSpan artFileName
       ,appN [Var noSpan qNameGMain, mkExpSR noSpan Trusted, expRoot]]))
    Nothing
  where
  qNameGMain = nameTransLetVar (UnQual noSpan (nameMain noSpan))
  


-- ----------------------------------------------------------------------------
-- Produce export list

tMaybeExportSpecList :: String -> -- name of this module
                        Environment ->
                        Maybe (ExportSpecList SrcSpanInfo) -> -- original list
                        [Decl SrcSpanInfo] -> -- new declarations
                        Maybe (ExportSpecList SrcSpanInfo)
tMaybeExportSpecList _ _ Nothing decls = 
  Just (ExportSpecList noSpan (concatMap makeExport decls))
tMaybeExportSpecList thisModuleId env 
  (Just (ExportSpecList l exportSpecs)) decls =
  Just (ExportSpecList l 
          (concatMap (tExportSpec env decls thisModuleId) exportSpecs))
 
tExportSpec :: Environment -> [Decl SrcSpanInfo] -> 
               String ->  -- name of *this* module, being transformed
               ExportSpec SrcSpanInfo ->
               [ExportSpec SrcSpanInfo]
tExportSpec env _ _ (EVar span qname) = 
  map (EVar span) (tEntityVar env qname)
tExportSpec env _ _ (EAbs span qname) =
  map (EAbs span) qnames'
  where
  qnames' = tEntityAbs env qname
tExportSpec env decls thisModule (EThingAll span qName) 
  | isClass e = [EThingAll span (nameTransCls qName)]
  | isType e = tExportSpec env decls thisModule 
                 (EThingWith span qName (map conName (eCons e) ++ map fieldName (eFields e)))
  | otherwise = error "TraceTrans.tExportSpec: type synonym with (..)."
  where
  e = lookupTypeEnv env qName
  conName str = ConName span (Symbol span str)
  fieldName str = VarName span (Ident span str)
tExportSpec env _ _ (EThingWith span qname cnames) =
  EThingWith span qname' cnames' : map (EVar span) qnames'
  where
  (qname', cnames', qnames') = tEntityThingWith env qname cnames
tExportSpec env decls thisModuleId 
  (EModuleContents span moduleName@(ModuleName _ moduleId)) =
  if thisModuleId == moduleId 
    then concatMap makeExport decls
    else [EModuleContents span (nameTransModule moduleName)]

-- Produce export entities from the declarations generated by the transformation
-- These are all entities defined in this module and meant for export.
makeExport :: Decl SrcSpanInfo -> [ExportSpec SrcSpanInfo]
makeExport (TypeDecl l declHead _) = 
  [EAbs l (UnQual l (getDeclHeadName declHead))]
makeExport (TypeFamDecl l declHead _) = 
  [EAbs l (UnQual l (getDeclHeadName declHead))]
makeExport (DataDecl l _ _ declHead _ _) =
  [EThingAll l (UnQual l (getDeclHeadName declHead))]
makeExport (GDataDecl l _ _ declHead _ _ _) =
  [EThingAll l (UnQual l (getDeclHeadName declHead))]
makeExport (ClassDecl l _ declHead _ _) =
  [EThingAll l (UnQual l (getDeclHeadName declHead))]
makeExport (FunBind l matches) =
  if exportedTransName name then [EVar l (UnQual l name)] else []
  where
  name = getMatchName (head matches)
makeExport (PatBind l (PVar l' name) _ _ _) =
  if exportedTransName name then [EVar l (UnQual l name)] else []
makeExport _ = []

-- Checks whether this is the name of a function that should be exported 
-- by the module.
exportedTransName :: Name l -> Bool
exportedTransName (Ident _ name) = head name `elem` ['g','a','h']
exportedTransName (Symbol _ name) = head name `elem` ['!','+','*']

getMatchName :: Match l -> Name l
getMatchName (Match _ name _ _ _) = name
getMatchName (InfixMatch _ _ name _ _ _) = name

getMatchArity :: Match l -> Arity
getMatchArity (Match _ _ pats _ _) = length pats
getMatchArity (InfixMatch _ _ _ pats _ _) = 1 + length pats

getDeclHeadName :: DeclHead l -> Name l
getDeclHeadName (DHead _ name _) = name
getDeclHeadName (DHInfix _ _ name _) = name
getDeclHeadName (DHParen _ declHead) = getDeclHeadName declHead

-- ----------------------------------------------------------------------------
-- Produce imports

tImpDecls :: Environment -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
tImpDecls env impDecls = 
  ImportDecl {importAnn = noSpan,
              importModule = ModuleName noSpan "Prelude",
              importQualified = True,
              importSrc = False,
              importPkg = Nothing,
              importAs = Nothing,
              importSpecs = Nothing}
    -- Avoid default import of Prelude by importing it qualified.
    -- Transformed program still uses a few (qualified) Prelude
    -- functions and data constructors.
--  : ImportDecl {importAnn = noSpan,
--                importModule = ModuleName noSpan "Hat.Hack",
--                importQualified = False,
--                importSrc = False,
--                importPkg = Nothing,
--                importAs = Nothing,
--                importSpecs = Nothing}
--    -- For list syntax : and [].
--    -- Is that really needed?
  : ImportDecl {importAnn = noSpan,
                importModule = ModuleName noSpan "Hat.Hat",
                importQualified = True,
                importSrc = False,
                importPkg = Nothing,
                importAs = Just (ModuleName noSpan "T"),
                importSpecs = Nothing}
  -- All types and combinators for tracing, inserted by the transformation
  : map (tImportDecl env) impDecls

tImportDecl :: Environment -> ImportDecl SrcSpanInfo -> ImportDecl SrcSpanInfo
tImportDecl env importDecl = 
  if importSrc importDecl 
    then notSupported (importAnn importDecl) "{-# SOURCE #-}"
  else if isJust (importPkg importDecl)
    then notSupported (importAnn importDecl) "explicit package name"
  else 
    importDecl{importModule = nameTransModule (importModule importDecl),
               importAs = fmap nameTransModule (importAs importDecl),
               importSpecs = fmap (tImportSpecList env) (importSpecs importDecl)
              }

tImportSpecList :: Environment -> ImportSpecList SrcSpanInfo -> 
                   ImportSpecList SrcSpanInfo
tImportSpecList env (ImportSpecList l hiding importSpecs) =
  ImportSpecList l hiding (concatMap (tImportSpec env) importSpecs)
      
-- Nearly identical with tExportSpec except for the types.  
tImportSpec :: Environment -> ImportSpec SrcSpanInfo -> [ImportSpec SrcSpanInfo] 
tImportSpec env (IVar span name) = 
  map (IVar span . qName2Name) qnames'
  where
  qnames' = tEntityVar env (UnQual (ann name) name)
tImportSpec env (IAbs span name) =
  map (IAbs span . qName2Name) qnames'
  where
  qnames' = tEntityAbs env (UnQual (ann name) name)
tImportSpec env (IThingAll span name) 
  | isClass e = [IThingAll span (nameTransCls name)]
  | isType e = tImportSpec env (IThingWith span name 
                 (map conName (eCons e) ++ map fieldName (eFields e)))
  | otherwise = error "TraceTrans.tImportSpec: unexpected type synonym with (..)"
  where
  e = lookupTypeEnv env (UnQual (ann name) name)
  conName str = ConName span (mkName span str)
  fieldName str = VarName span (mkName span str)
tImportSpec env (IThingWith span name cnames) =
  IThingWith span (qName2Name qname') cnames' : 
    map (IVar span . qName2Name) qnames'
  where
  (qname', cnames', qnames') = 
    tEntityThingWith env (UnQual (ann name) name) cnames

-- ----------------------------------------------------------------------------
-- Produce entities in either import or export list

tEntityVar :: SrcInfo l => Environment -> QName l -> [QName l]
tEntityVar env qname = 
  nameTransLetVar qname : 
  case arity env qname of
    Just a | a > 0 -> [nameTraceInfoGlobalVar qname, nameWorker qname]            
    Just (-1)      -> [nameShare qname]
    _              -> []

-- a class or datatype ex-/imported abstractly, or a type synonym
tEntityAbs :: SrcInfo l => Environment -> QName l -> [QName l]
tEntityAbs env qname 
  | isClass e = [nameTransCls qname]
  | isType e = [nameTransTy qname]
  | isSyn e = nameTransSyn qname : 
                map (nameTransTySynHelper qname) [1..eNo e]
  where
  e = lookupTypeEnv env qname

-- a class with some methods or a datatype with some constructors/fields
tEntityThingWith :: SrcInfo l => Environment -> QName l -> [CName l] -> 
                    (QName l, [CName l], [QName l])
tEntityThingWith env qname cnames 
  | isClass e = (nameTransCls qname, 
                 map nameTransLetVar cnames ++ 
                 map nameShare cnames,
                 [])
  | isType e =  (nameTransTy qname,
                 map nameTransCon consNames ++ map nameTransField fieldNames,
                 map nameTransLetVar qFieldNames ++
                 map nameWorker qFieldNames ++
                 map nameTraceInfoGlobalVar qFieldNames ++
                 map nameTraceInfoCon qConsNames) 
  | otherwise = error "TraceTrans.tEntityThingWith: unexpected sort of type."   
  where
  e = lookupTypeEnv env qname
  qConsNames = map (cName2QName qname) consNames
  qFieldNames = map (cName2QName qname) fieldNames
  (consNames, fieldNames) = partition isSymbol cnames

-- ----------------------------------------------------------------------------
-- New top-level definitions for generating shared trace info
-- 
-- Trace info for positions and identifier information. They have to be 
-- top-level, so that they (and their side-effect) are only evaluated once.
-- INCOMPLETE: an optimising compiler may need noinline pragma. 
-- The variables referring to variable information need to include the 
-- position in the name, because the same variable name may be used several 
-- times.

defNameMod :: ModuleName SrcSpanInfo -> String -> Tracing -> Decl SrcSpanInfo
defNameMod modName@(ModuleName l modId) filename tracing =
  PatBind l (PVar l (nameTraceInfoModule modName)) Nothing
    (UnGuardedRhs l
      (appN
        [Var l (qNameMkModule l)
        ,litString l modId
        ,litString l filename
        ,Con l (if isTraced tracing then qNamePreludeTrue l 
                                    else qNamePreludeFalse l)]))
    Nothing


-- The span in the name is used in the declared name and its definition.
defNameCon :: Name SrcSpanInfo -> [Name SrcSpanInfo] -> Int -> Int -> Exp SrcSpanInfo -> Decl SrcSpanInfo
defNameCon conName fieldNames fixPri arity moduleTrace =
  PatBind l (PVar l (nameTraceInfoCon conName)) Nothing
    (UnGuardedRhs l
      (appN
        (Var l (qNameMkAtomConstructor l withFields) :
         moduleTrace :
         encodeSpan l ++
         litInt l fixPri :
         Paren l (litInt l arity) :
         litString l ident :
         if withFields
           then (:[]) . mkExpList .
                  map (Var l . UnQual l . nameTraceInfoVar l Global) $ 
                  fieldNames
           else [] )
       ))
    Nothing
  where
  l = ann conName
  ident = getId conName
  withFields = not (null fieldNames)

{-
defNameCon :: Environment -> 
              Exp SrcSpanInfo -> 
              (Name SrcSpanInfo, [Name SrcSpanInfo]) -> 
              Decl SrcSpanInfo
defNameCon env moduleTrace (conName, fieldNames) =
  PatBind l (PVar l (nameTraceInfoCon conName)) Nothing
    (UnGuardedRhs l
      (appN
        (Var l (qNameMkAtomConstructor l withFields) :
         moduleTrace :
         encodeSpan l ++
         litInt l (fixPriority env conName) :
         litInt l (fromJust (arity env (UnQual l conName))) :
         litString l ident :
         if withFields
           then (:[]) . mkExpList .
                  map (Var l . UnQual l . nameTraceInfoVar l Global) $ 
                  fieldNames
           else [] )
       ))
    Nothing
  where
  l = ann conName
  ident = getId conName
  withFields = not (null fieldNames)
-}

-- The span in the name is used in the declared name and its definition.
defNameVar :: Name SrcSpanInfo -> Int -> Int -> Scope -> Scope -> Exp SrcSpanInfo -> Decl SrcSpanInfo
defNameVar varName fixPri arity defScope visScope moduleTrace =
  PatBind l (PVar l (nameTraceInfoVar l visScope varName)) Nothing
    (UnGuardedRhs l
      (appN
        (Var l (qNameMkAtomVariable l) :
         moduleTrace :
         encodeSpan l ++
         [litInt l fixPri,
          Paren l (litInt l arity),
          litString l (getId varName),
          Con l (if isLocal defScope then qNamePreludeTrue l 
                                     else qNamePreludeFalse l)])))
    Nothing
  where
  l = ann varName

{-
defNameVar :: Environment -> Scope -> Scope -> 
              Exp SrcSpanInfo -> Name SrcSpanInfo -> 
              Decl SrcSpanInfo
defNameVar env defScope visScope moduleTrace varName =
  PatBind l (PVar l (nameTraceInfoVar l visScope varName)) Nothing
    (UnGuardedRhs l
      (appN
        (Var l (qNameMkAtomVariable l) :
         moduleTrace :
         encodeSpan l ++
         [litInt l (fixPriority env varName),
           -- all identifiers in definition position are assumed to 
           -- be equipped with an arity; 
           -- only those defined by pattern bindings do not; they have arity 0.
          litInt l (fromMaybe 0 (arity env (UnQual l varName))),
          litString l (getId varName),
          Con l (if isLocal defScope then qNamePreludeTrue l 
                                     else qNamePreludeFalse l)])))
    Nothing
  where
  l = ann varName
-}

defNameSpan :: Exp SrcSpanInfo -> SrcSpan -> Decl SrcSpanInfo
defNameSpan moduleTrace span =
  PatBind l (PVar l (nameTraceInfoSpan l)) Nothing
    (UnGuardedRhs l
      (appN
        (Var l (qNameMkSpan l) :
         moduleTrace :
         encodeSpan l)))
    Nothing
  where
  l = noInfoSpan span

-- Encode a span in the trace file
encodeSpan :: SrcSpanInfo -> [Exp SrcSpanInfo]
encodeSpan span =
  [litInt noSpan (10000*beginRow + beginCol)
  ,litInt noSpan (10000*endRow + endCol)]
  where
  (beginRow,beginCol,endRow,endCol) = getSpan span

-- Obtain start line, column and end line, column
-- Try to get correct end, as parser gives start of next construct instead.
getSpan :: SrcSpanInfo -> (Int,Int,Int,Int)
getSpan SrcSpanInfo{srcInfoSpan=srcSpan,srcInfoPoints=points} =
  (srcSpanStartLine srcSpan, srcSpanStartColumn srcSpan, erow, ecol)
  where
  end = if null points 
          then srcSpanEnd srcSpan
          else srcSpanEnd (last points)  
  correct (row,col) = (row,if col > 0 then col-1 else col) 
  (erow,ecol) = correct end         

-- Compress data.
dropInfo :: SrcSpanInfo -> SrcSpan
dropInfo SrcSpanInfo{srcInfoSpan=srcSpan,srcInfoPoints=points} =
  srcSpan{srcSpanEndLine = line, srcSpanEndColumn = col}
  where
  (line,col) = if null points then srcSpanEnd srcSpan else srcSpanEnd (last points)  
  
                     
-- ----------------------------------------------------------------------------
-- Abstract data type for keeping track of two sorts of constants introduced 
-- by the transformation.
-- 
-- 1. Collects all spans used in source references of transformed code, 
-- so that corresponding variables can be defined at the top-level of the module.
-- The same span can be added repeatedly, but doubtful this ever happens.
-- 2. Collects information about defined expression names 
-- (let-bound variables, fields, methods and data constructors) 
-- from the environment of every scope. From this corresponding variables 
-- describing the namse and definition locations are obtained at the end.
-- Needs to know whether a scope is local or global, to produce appropriate
-- variable names. 
--
-- Have compositional union operator to combine constants.

data ModuleConsts = MC (Set SrcSpan) ([DeclFun] -> [DeclFun])
type DeclFun = Exp SrcSpanInfo -> Decl SrcSpanInfo

moduleConstsEmpty :: ModuleConsts
moduleConstsEmpty = MC Set.empty id

moduleConstsSpan :: SrcSpanInfo -> ModuleConsts
moduleConstsSpan l = MC (Set.singleton (dropInfo l)) id

-- Add a name declaration for every unqualified value name (not type/class) in the environment.
moduleConstsEnv :: Scope -> Environment -> ModuleConsts
moduleConstsEnv scope env = MC Set.empty (\xs -> defineNameEnv scope env defNameVar defNameCon ++ xs)

infixl 5 `moduleConstsUnion`
moduleConstsUnion :: ModuleConsts -> ModuleConsts -> ModuleConsts
moduleConstsUnion (MC spans1 dfs1) (MC spans2 dfs2) = MC (spans1 `Set.union` spans2) (dfs1 . dfs2)

moduleConstsGet :: ModuleConsts -> ([SrcSpan], [DeclFun])
moduleConstsGet (MC spans dfs) = (Set.elems spans, dfs [])


-- ----------------------------------------------------------------------------
-- Transformation of declarations, expressions etc.

-- pre-condition: The environment contains information about all
-- identifiers declared on this level and more global,
-- but not the local scopes inside.
tDecls :: Environment -> Scope -> Tracing -> 
          [Decl SrcSpanInfo] ->
          ([Decl SrcSpanInfo], ModuleConsts)
tDecls env scope tracing decls = 
  foldr combine ([], moduleConstsEmpty) 
    (map (tDecl env scope tracing) decls)
  where
  combine :: ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts)
  combine (ds1, c1) (ds2, c2) = (ds1 ++ ds2, c1 `moduleConstsUnion` c2)


tDecl :: Environment -> Scope -> Tracing ->
         Decl SrcSpanInfo ->
         ([Decl SrcSpanInfo], ModuleConsts)
tDecl env _ _ synDecl@(TypeDecl span declHead ty) =
  (map tTypeSynonym (splitSynonym env synDecl), moduleConstsEmpty)
  where
  tTypeSynonym :: Decl SrcSpanInfo -> Decl SrcSpanInfo
  tTypeSynonym (TypeDecl span declHead ty) =
    TypeDecl span (declHead) (tType ty)
tDecl _ _ _ (TypeFamDecl l _ _) =
  notSupported l "type family declaration"
tDecl env Global tracing d@(DataDecl span dataOrNew maybeContext declHead 
                               qualConDecls maybeDeriving) =
  (DataDecl span dataOrNew (fmap tContext maybeContext) 
     (declHead) (map tQualConDecl qualConDecls) Nothing :
   -- "derive" must be empty, because transformed classes cannot be derived
   instDecl : fieldSelectorDecls ++ deriveDecls'
  ,derivedConsts `moduleConstsUnion` fieldSelectorConsts `moduleConstsUnion` deriveConsts)
  where
  derivedDecls = derive env d
  derivedConsts = moduleConstsEnv Local (declsEnv False env derivedDecls)
  (deriveDecls', deriveConsts) = 
    tDecls env Global Trusted derivedDecls
  instDecl = wrapValInstDecl env tracing maybeContext declHead qualConDecls
  (fieldSelectorDecls, fieldSelectorConsts) = mkFieldSelectors qualConDecls
tDecl _ _ _ (GDataDecl l _ _ _ _ _ _) =
  notSupported l "generalized algebraic data type declaration"
tDecl _ _ _ (DataFamDecl l _ _ _) =
  notSupported l "data family declaration"
tDecl _ _ _ (TypeInsDecl l _ _) =
  notSupported l "type family instance declaration"
tDecl _ _ _ (DataInsDecl l _ _ _ _) =
  notSupported l "data family instance declaration"
tDecl _ _ _ (GDataInsDecl l _ _ _ _ _) =
  notSupported l "GADT family instance declaration"
tDecl env _ tracing  -- class without methods
  (ClassDecl l maybeContext declHead fundeps Nothing) =
  onlyDecl (ClassDecl l (fmap tContext maybeContext) 
     (mapDeclHead (updateId id) declHead) 
     (map tFunDep fundeps) Nothing)
tDecl env _ tracing  -- class with methods
  (ClassDecl l maybeContext declHead fundeps (Just classDecls)) =
  ([ClassDecl l (fmap tContext maybeContext) 
    (mapDeclHead (updateId id) declHead) 
    (map tFunDep fundeps) (Just classDecls')]
  ,declsConsts)
  where
  (classDecls', declsConsts) = tClassDecls env tracing classDecls
tDecl env _ tracing -- class instance without methods
  (InstDecl l maybeContext instHead Nothing) =
  onlyDecl (InstDecl l (fmap tContext maybeContext) (tInstHead instHead) 
    Nothing)
tDecl env _ tracing -- class instance with methods
  inst@(InstDecl l maybeContext instHead (Just instDecls)) =
  ([InstDecl l (fmap tContext maybeContext) (tInstHead instHead) 
     (Just instDecls')]
  ,declsConsts `moduleConstsUnion` moduleConstsEnv Local localEnv)
  where
  (instDecls', declsConsts) = tInstDecls env2 tracing instDecls
  -- Add unqualified method names to environment.
  -- Qualfied ones should already be in environment but unqualfied may not.
  localEnv = instanceEnv (isTraced tracing) env inst
  env2 = env `unionLocalRelation` localEnv

tDecl _ _ _ (DerivDecl l _ _) =
  notSupported l "standalone deriving declaration"
tDecl _ _ _ (InfixDecl l assoc priority ops) =
  onlyDecl (InfixDecl l assoc priority (map nameTransLetVar ops))
tDecl env _ _ (DefaultDecl l tys) =
  ([DefaultDecl l []
   ,WarnPragmaDecl l [([], "Defaulting doesn't work in traced programs. Add type annotations to resolve ambiguities.")]]
  ,moduleConstsEmpty)
tDecl _ _ _ (SpliceDecl l _) =
  notSupported l "Template Haskell splicing declaration"
tDecl env _ _ (TypeSig l names ty) =
  -- Type signatures need to be preserved (i.e. transformed),
  -- because polymorphic recursion needs them or more general
  -- types may later lead to ambiguous types
  -- Also shared constants need to be typed, in case they are overloaded,
  -- so that the monomorphic restriction does not lead to type error
  -- (actually then sharing is unfortunately lost)
  (TypeSig l (map nameTransLetVar names) (tMapType tFunType ty) :
   concatMap mkWorkerTypeSig nonConstNames ++
   if null constNames then []
     else [TypeSig l (map nameShare constNames) (tMapType tConstType ty)]
  ,moduleConstsEmpty)
  where
  (constNames, nonConstNames) = Data.List.partition isNonMethodConstant names
  isNonMethodConstant :: Name SrcSpanInfo -> Bool
  isNonMethodConstant name =
    isLambdaBound env qName || maybe False (==0) (arity env qName)
    where
    qName = UnQual l name
  mkWorkerTypeSig :: Name SrcSpanInfo -> [Decl SrcSpanInfo]
  mkWorkerTypeSig name =
    case arity env (UnQual l name) of
      Just n | n > 0 -> [TypeSig l [nameWorker name] (tMapType (tWorkerType env n) ty)]
      _ -> []
tDecl env scope tracing (FunBind l matches) =
  tFunBind env scope tracing l matches  
  -- a function does not use the static parent
tDecl env scope tracing (PatBind l pat maybeTy rhs maybeBinds) =
  tPatBind env scope tracing l pat maybeTy rhs maybeBinds
tDecl env _ _ (ForImp l callConv maybeSafety maybeString name ty) =
  case maybeString >>= stripPrefix "NotHat." of
    Just origName -> tForeignImp l (mkQName l origName) name ty
    Nothing -> 
      (ForImp l callConv maybeSafety maybeString (nameForeign name) ty :
         -- type is not renamed, original given type left unchanged  
       wrapperDecls
      ,consts)
  where
  (wrapperDecls, consts) = tForeignImp l (UnQual l (nameForeign name)) name ty
tDecl _ _ _ (ForExp l _ _ _ _) =
  notSupported l "foreign export declaration"
tDecl _ _ _ (RulePragmaDecl l _) =
  notSupported l "RULES pragma"
tDecl _ _ _ (DeprPragmaDecl l list) = onlyDecl (DeprPragmaDecl l list)
tDecl _ _ _ (WarnPragmaDecl l list) = onlyDecl (WarnPragmaDecl l list)
tDecl _ _ _ (InlineSig l _ _ _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore INLINE pragma")])
tDecl _ _ _ (InlineConlikeSig l _ _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore INLINE CONLIKE pragma")])
tDecl _ _ _ (SpecSig l _ _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore SPECIALISE pragma")])
tDecl _ _ _ (SpecInlineSig l _ _ _ _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore SPECIALISE INLINE pragma")])
tDecl _ _ _ (InstSig l _ _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore SPECIALISE instance pragma")])
tDecl _ _ _ (AnnPragma l _) =
  onlyDecl (WarnPragmaDecl l [([], "ignore ANN pragma")])

onlyDecl :: Decl l -> ([Decl l], ModuleConsts)
onlyDecl d = ([d], moduleConstsEmpty)


-- Process pattern binding:

tPatBind :: Environment -> Scope -> Tracing -> SrcSpanInfo ->
            Pat SrcSpanInfo -> Maybe (Type SrcSpanInfo) -> Rhs SrcSpanInfo ->
            Maybe (Binds SrcSpanInfo) ->
            ([Decl SrcSpanInfo], ModuleConsts)
tPatBind env scope tracing l (PVar _ name) maybeType rhs maybeBinds =
  -- simple case
  tCaf env scope tracing l name maybeType rhs maybeBinds
tPatBind env scope tracing l (PAsPat _ name pat) maybeType rhs maybeBinds =
  -- can break off simple case
  (cafDecls ++ patDecls, cafConsts `moduleConstsUnion` patConsts)
  where
  (cafDecls, cafConsts) = 
    tCaf env scope tracing l name maybeType rhs maybeBinds
  (patDecls, patConsts) = tDecl env scope tracing
    (PatBind l pat maybeType (UnGuardedRhs l (Var l (UnQual l name))) Nothing)
tPatBind env scope tracing l pat maybeType rhs maybeBinds =
  -- unfortunately we cannot transform a pattern binding into another pattern
  -- binding; we have to introduce an explicit `case' to be able to terminate 
  -- with an appropriate error message when the pattern does not match.
  -- first rewrite as p = e, then
  -- xi sr p = constUse sr p zi
  -- zi = constDef parent 
  --        (\_ -> (case patId of (t,y1,..,yn) -> projection sr t yi))
  -- patId = case e' of 
  --           p' -> (t,y1,..,yn)
  --           _  -> fail noPos parent
  (map (useDef tracing) patNames ++
   map (projDef scope tracing l patTuple 
          (Var l (UnQual l resultTraceName)) patName resultTraceName) patNames ++
   [PatBind l (PVar l patName) Nothing (UnGuardedRhs l (
     (Case l exp'
       [Alt l pat'' (UnGuardedAlt l expTuple) Nothing
       ,Alt l (PWildCard l) (UnGuardedAlt l (mkFailExp expParent)) Nothing])))
     Nothing]
  ,foldr moduleConstsUnion altConsts (map (moduleConstsSpan . ann) patNames))
  where
  firstName = head patNames
  patNames = map (\(PVar _ name) -> name) patVars
  patName = nameTraceShared l firstName
  resultTraceName = nameTrace2 firstName
  expTuple = Tuple l (map (Var l . UnQual l) (resultTraceName : patNames'))
  patTuple = PTuple l (map (PVar l) (resultTraceName : patNames'))
  patNames' = map (\(PVar _ name) -> name) patVars'
  (patVars', Nothing) = tPats patVars
  -- Nothing means that we do not support numeric patterns (k and n+k)
  patVars = getPatVars pat
  pat'' = patAddReference resultTraceName pat'
  (Match _ _ [pat'] (UnGuardedRhs _ exp') maybeBinds', altConsts) =
    tMatch env tracing False undefined failContinuation 
      (Match l firstName [pat] rhs maybeBinds)

patAddReference :: Name l -> Pat l -> Pat l
patAddReference n (PParen l pat) = PParen l (patAddReference n pat)
patAddReference n (PApp l r [v,_]) = PApp l r [v,PVar l n]
  
-- Build the first set of definitions for pattern bindings.
useDef :: Tracing -> Name SrcSpanInfo -> Decl SrcSpanInfo
useDef tracing name =
  FunBind l [Match l (nameTransLetVar name) [PVar l sr, patParent]
               (UnGuardedRhs l (appN
                  [mkExpConstUse tracing 
                  ,Var l (UnQual l sr)
                  ,expParent
                  ,Var l (UnQual l (nameShare name))]))
               Nothing]
  where
  l = ann name
  sr = nameSR name 

-- Caf for one variable in a pattern binding
projDef :: Scope -> Tracing -> SrcSpanInfo -> Pat SrcSpanInfo -> Exp SrcSpanInfo -> 
           Name SrcSpanInfo -> Name SrcSpanInfo -> Name SrcSpanInfo -> Decl SrcSpanInfo
projDef scope tracing l patTuple expVarResultTrace patName resultTraceName name =
  PatBind l (PVar l (nameShare name)) Nothing 
    (UnGuardedRhs l (appN 
      [mkExpConstDef tracing
      ,expParent
      ,Var l (UnQual l (nameTraceInfoVar l scope name))
      ,Lambda l [PWildCard l]
         (Case l (Var l (UnQual l patName))
           [Alt l patTuple (UnGuardedAlt l 
             (if isLocal scope && tracing == Trusted
                then varLambdaName
                else appN
                       [Var l (qNameProjection l)
                       ,mkExpSR (ann name) tracing
                       ,Var l (UnQual l resultTraceName)
                       ,varLambdaName]))
             Nothing])]))
    Nothing
  where
  varLambdaName = Var l (UnQual l (nameTransLambdaVar name))

-- Extract all variables from a pattern, left-to-right
getPatVars :: Pat SrcSpanInfo -> [Pat SrcSpanInfo]
getPatVars p@(PVar _ _) = [p]
getPatVars (PLit _ _) = []
getPatVars (PNeg _ p) = getPatVars p
getPatVars (PNPlusK l _ _) = notSupported l "n+k pattern in pattern binding"
getPatVars (PInfixApp _ pl _ pr) = getPatVars pl ++ getPatVars pr
getPatVars (PApp _ _ ps) = concatMap getPatVars ps
getPatVars (PTuple _ ps) = concatMap getPatVars ps
getPatVars (PList _ ps) = concatMap getPatVars ps
getPatVars (PParen _ p) = getPatVars p
getPatVars (PRec _ _ patFields) = concatMap getPatFieldVars patFields
  where
  getPatFieldVars :: PatField SrcSpanInfo -> [Pat SrcSpanInfo]
  getPatFieldVars (PFieldPat _ _ p) = getPatVars p
  getPatFieldVars (PFieldPun _ _) = []
  getPatFieldVars (PFieldWildcard _) = []
getPatVars (PAsPat _ n p) = PVar (ann n) n : getPatVars p
getPatVars (PWildCard _) = []
getPatVars (PIrrPat _ p) = getPatVars p
getPatVars (PatTypeSig _ p _) = getPatVars p
getPatVars (PViewPat _ _ p) = getPatVars p
getPatVars (PBangPat _ p) = getPatVars p
getPatVars p = notSupported (ann p) "in pattern binding"


-- Binding of a single variable
-- The second binding is similar to what projDef produces
tCaf :: Environment -> Scope -> Tracing -> SrcSpanInfo -> 
        Name SrcSpanInfo ->  -- name of original caf
        Maybe (Type SrcSpanInfo) -> 
        Rhs SrcSpanInfo -> Maybe (Binds SrcSpanInfo) -> 
        ([Decl SrcSpanInfo], ModuleConsts)
tCaf env scope tracing l name maybeType rhs maybeBinds =
  -- Produce a wrapper and a worker, the latter must be a caf
  -- to avoid reevaluation.
  -- name sr p = constUse sr p name'
  -- name' = constDef parent "name" (\p -> [[rhs]]_p)
  ([useDef tracing name
   ,PatBind l (PVar l (nameShare name)) (fmap tType maybeType)
      (UnGuardedRhs l (appN
        [mkExpConstDef tracing
        ,expParent
        ,Var l (UnQual l (nameTraceInfoVar l scope name))
        ,Lambda l [patParent] (smartExpLet maybeBinds' rhs')]))
      Nothing]
  ,moduleConstsEnv Local envLocal `moduleConstsUnion` rhsConsts `moduleConstsUnion` bindsConsts)
  --,addVar l name moduleConstsEmpty `withLocal` 
  --   (rhsConsts `moduleConstsUnion` bindsConsts))
  where
  envLocal = maybeBindsEnv (isTraced tracing) maybeBinds
  env2 = env `unionLocalRelation` envLocal
  (rhs', rhsConsts) = tRhs env2 tracing True failContinuation rhs
  (maybeBinds', bindsConsts) = tMaybeBinds env2 tracing maybeBinds
  smartExpLet :: Maybe (Binds SrcSpanInfo) -> Exp SrcSpanInfo -> Exp SrcSpanInfo
  smartExpLet Nothing exp = exp
  smartExpLet (Just binds) exp = Let noSpan binds exp

tMaybeBinds :: Environment -> Tracing -> Maybe (Binds SrcSpanInfo) ->
               (Maybe (Binds SrcSpanInfo), ModuleConsts)
tMaybeBinds env tracing Nothing =
  (Nothing, moduleConstsEmpty)
tMaybeBinds env tracing (Just binds) =
  (Just binds', bindsConsts)
  where
  (binds', bindsConsts) = tBinds env tracing binds

tBinds :: Environment -> Tracing -> Binds SrcSpanInfo ->
          (Binds SrcSpanInfo, ModuleConsts)
tBinds env tracing (BDecls l decls) =
  (BDecls l decls', declsConsts)
  where
  (decls', declsConsts) = tDecls env Local tracing decls
tBinds env tracing (IPBinds l _) =
  notSupported l "binding group for implicit parameters"


-- Process function binding:

tFunBind :: Environment -> Scope -> Tracing -> SrcSpanInfo -> 
            [Match SrcSpanInfo] ->
            ([Decl SrcSpanInfo], ModuleConsts)
tFunBind env scope tracing l matches =
  -- Define the wrapper and the worker.
  (FunBind l [Match l (nameTransLetVar orgName) 
               [PVar l sr, patParent]
               (UnGuardedRhs l 
                 (mkExpFun tracing (Var l (UnQual l sr)) 
                   (Var l (UnQual l (nameTraceInfoVar l scope orgName)))
                   (Var l (UnQual l wrappedId')) funArity))
               Nothing]
  :(if isLocal scope 
     then (PatBind l (PVar l (nameTraceInfoVar l Global orgName)) Nothing 
            (UnGuardedRhs l
               (Var l (UnQual l (nameTraceInfoVar l Local orgName)))) 
            Nothing :)
     else \x->x)
    (FunBind l matches' : newDecls')
  -- The known-arity application optimisation needs a nameTraceInfoVar of
  -- the global kind (the name does not include the definition position)
  -- Hence for local definitions we need to define the nameTraceInfoVar
  -- in terms of the "local" nameTraceInfoVar that is defined globally.
  -- In same scope as type decl
  ,funConsts)
  -- ,addVar l orgName (moduleConstsEmpty `withLocal` funConsts))
  where
  funArity = getMatchArity (head matches)
  orgName = getMatchName (head matches)
  sr = nameSR orgName
  wrappedId' = nameWorker orgName
  (matches', newDecls', funConsts) =
    tMatches env tracing l (nameFuns orgName)
      (map (PVar l) (nameArgs orgName)) wrappedId'
      (matchArity (head matches)) False 
      (map changeInfixMatch matches)


-- Transformation of matches is complex because guarded equations may fail
-- and thus 'fall' to next equation.
-- This is simulated here by a sequence of functions, each possibly calling
-- the next, i.e., we use continuation passing style.
tMatches :: Environment -> Tracing -> SrcSpanInfo -> 
            [Name SrcSpanInfo] -> -- names for definitions that clauses become
            [Pat SrcSpanInfo] -> -- vars for naming arguments that are not vars
            Name SrcSpanInfo -> -- name of this definition
            Arity -> 
            Bool -> -- preceeding match will never fail
            [Match SrcSpanInfo] ->
            ([Match SrcSpanInfo], [Decl SrcSpanInfo], ModuleConsts)
tMatches _ _ _ _ _ _ _ True [] = 
  -- Preceeding match never fails, so no further match required.
  ([], [], moduleConstsEmpty)
tMatches env tracing l ids pVars funName funArity False [] =
  -- End of matches, but add catch-all match to explicitly raise an error.
  ([Match l funName (replicate funArity (PWildCard l) ++ [patParent])
     (UnGuardedRhs l (continuationToExp failContinuation))
     Nothing]
  ,[]
  ,moduleConstsEmpty)
tMatches env tracing l ids pVars funName funArity _
  (m@(Match _ _ pats _ _) : matches) | not (null matches) && matchCanFail m =
  -- Normal sort of match: further matches afterwards and guard or numeric literals may fail.
  ([Match l funName (pats'' ++ [patParent]) rhs' decls'
   ,Match l funName (vars ++ [patParent]) 
      (UnGuardedRhs l (continuationToExp failCont)) Nothing]
  ,FunBind l matches' : matchesDecls
  ,matchConsts `moduleConstsUnion` matchesConsts)
  where
  contId = head ids
  failCont = functionContinuation (UnQual l contId) (map pVarToVar vars)
  (pats'', vars) = namePats pats' pVars
  (Match _ _ pats' rhs' decls', matchConsts) =
    tMatch env tracing True funName failCont m
  (matches', matchesDecls, matchesConsts) =
    tMatches env tracing l (tail ids) pVars contId funArity 
      (neverFailingPats pats) matches
tMatches env tracing l ids pVars funName funArity _ 
  (m@(Match _ _ pats _ _) : matches) =
  -- Last match or guards and numeric literals cannot fail.
  (Match l funName (pats' ++ [patParent]) rhs' decls' : matches'
  ,matchesDecls
  ,matchConsts `moduleConstsUnion` matchesConsts)
  where
  (Match _ _ pats' rhs' decls', matchConsts) =
    tMatch env tracing True funName failContinuation m
  (matches', matchesDecls, matchesConsts) =
    tMatches env tracing l ids pVars funName funArity 
      (neverFailingPats pats) matches

namePats :: [Pat SrcSpanInfo] -> [Pat SrcSpanInfo] -> 
            ([Pat SrcSpanInfo], [Pat SrcSpanInfo])
namePats pats vars = unzip (zipWith namePat pats vars)

-- Obtain a variable that names the given pattern;
-- straightforward if pattern has variable at top-level;
-- otherwise use provided variable
namePat :: Pat SrcSpanInfo  -- pattern to name
        -> Pat SrcSpanInfo  -- default variable
        -> (Pat SrcSpanInfo,Pat SrcSpanInfo)  -- named pattern, name variable
namePat pat@(PVar _ _) _ = (pat,pat)
namePat pat@(PAsPat l name pat') _ = (pat,PVar l name)
namePat pat var@(PVar l name) = (PAsPat l name pat,var)


-- Numeric literals need to be overloaded with respect to the new
-- transformed numeric classes; hence they cannot just be left wrapped
-- in patterns
-- Transform such literals and n+k patterns into conditions in guards.
-- Have to be careful to preserve left-to-right pattern matching,
-- e.g. f 1 True = ... -> f x True | x == 1 = ... is wrong.
-- Correct: f 1 True = M -> f v1 v2 | v1 == 1 = h v2 where h True = M
-- Assume that ~ has been removed before.
-- Cannot remove numeric patterns in a separate, earlier phase, because
-- need to handle continuation correctly when already a guard exists.
-- Definition similar to tGuardedExps
tMatch :: Environment ->
          Tracing ->
          Bool ->   -- whether this is reduct of the parent
          Name SrcSpanInfo ->   -- name for function this match partially defines
          ContExp SrcSpanInfo -> -- continuation in case of match failure
          Match SrcSpanInfo ->
          (Match SrcSpanInfo, ModuleConsts)
tMatch env tracing cr funName contExp m@(Match l _ pats rhs maybeBinds) =
  if isNothing numericLitInfos
    then (Match l funName pats' (UnGuardedRhs l rhs') maybeBinds'
         ,moduleConstsEnv Local envLocal `moduleConstsUnion` bindsConsts `moduleConstsUnion` rhsConsts)
    else (Match l funName pats' (UnGuardedRhs l (appN (case tracing of
           Traced -> [mkExpGuard Traced
                     ,mkExpSR l tracing
                     ,expParent
                     ,cond'
                     ,Lambda l [patParent]
                       (appN (varFun : argvars ++ [expParent]))
                     ,Lambda l [patParent]
                       (continuationToExp contExp)]
           Trusted -> [mkExpGuard Trusted
                      ,cond'
                      ,appN (varFun : argvars ++ [expParent])
                      ,continuationToExp contExp])))
           (Just (BDecls l (def:decl')))
         ,moduleConstsSpan l `moduleConstsUnion` condConsts `moduleConstsUnion` 
            declConsts `moduleConstsUnion` matchConsts `moduleConstsUnion` rhsConsts)
         -- condConsts contains locations of the Boolean guards
         -- declsConsts contains locations of the bound variables
  where
  envLocal = maybeBindsEnv (isTraced tracing) maybeBinds
  envLambdas = makeAllLambdaBound (patsEnv l (isTraced tracing) pats)
  env2 = (env `unionLocalRelation` envLambdas) `unionLocalRelation` envLocal 
  (pats', numericLitInfos) = tPats pats
  (rhs', rhsConsts) = tRhs env2 tracing cr contExp rhs
  (maybeBinds', bindsConsts) = tMaybeBinds env2 tracing maybeBinds
  -- from here for else branch above:
  nameFun = nameFunFromSpan l
  varFun = Var l (UnQual l nameFun)
  Just (qName, cond, bindings, argvars, argpats) = numericLitInfos
  envLocalExtra = declsEnv (isTraced tracing) env2 bindings
  env3 = (env2 `unionLocalRelation` envLocalExtra) `unionLocalRelation` lambdaVarEnv (isTraced tracing) qName
  (cond', condConsts) = tExp env3 tracing False cond
  (decl', declConsts) = tDecls env3 Local tracing bindings
  def = FunBind l [Match l nameFun (mpats'++[patParent]) mrhs' mmdecls'
                  ,Match l nameFun 
                     (replicate funArity (PWildCard l) ++ [patParent])
                     (UnGuardedRhs l (continuationToExp contExp))
                     Nothing]
  funArity = length argpats
  (Match _ _ mpats' mrhs' mmdecls', matchConsts) =
    tMatch env3 tracing cr funName contExp 
      (Match l nameFun argpats rhs maybeBinds)

-- Here failure means a failed test that can be observed in the trace,
-- not simply non-matching of data constructors.
matchCanFail :: Match l -> Bool
matchCanFail (Match _ _ pats rhs _) =
  any numericLitIn pats || case rhs of
    UnGuardedRhs _ _ -> False
    GuardedRhss _ gdRhss -> gdRhssCanFail gdRhss

numericLitIn :: Pat l -> Bool
numericLitIn (PLit _ (Int _ _ _)) = True
numericLitIn (PLit _ (Frac _ _ _)) = True
numericLitIn (PNeg _ pat) = numericLitIn pat
numericLitIn (PInfixApp _ pL _ pR) = numericLitIn pL || numericLitIn pR
numericLitIn (PApp _ _ pats) = any numericLitIn pats
numericLitIn (PTuple _ pats) = any numericLitIn pats
numericLitIn (PList _ pats) = any numericLitIn pats
numericLitIn (PParen _ p) = numericLitIn p
numericLitIn (PRec _ _ patFields) = any numericLitInField patFields
  where
  numericLitInField (PFieldPat _ _ pat) = numericLitIn pat
  numericLitInField _ = False
numericLitIn (PAsPat _ _ p) = numericLitIn p
numericLitIn (PIrrPat _ p) = numericLitIn p
numericLitIn (PatTypeSig _ p _) = numericLitIn p
numericLitIn (PViewPat _ _ p) = numericLitIn p
numericLitIn (PBangPat _ p) = numericLitIn p 
numericLitIn _ = False

-- Returns False only if one of the guards definitely evaluates to True
-- Note this is an approximation, no guarantee that failure is possible.
gdRhssCanFail :: [GuardedRhs l] -> Bool
gdRhssCanFail = all gdRhsCanFail

gdRhsCanFail :: GuardedRhs l -> Bool
gdRhsCanFail (GuardedRhs _ [Qualifier _ (Var _ name)] _) =
  not (isTrue name || isOtherwise name)
gdRhsCanFail _ = True


tRhs :: Environment ->
        Tracing ->
        Bool ->  -- equal to parent?
        ContExp SrcSpanInfo ->  -- continuation in case of match failure
        Rhs SrcSpanInfo ->
        (Exp SrcSpanInfo, ModuleConsts)
tRhs env tracing cr failCont (UnGuardedRhs _ exp) = 
  tExp env tracing cr exp
tRhs env tracing cr failCont (GuardedRhss _ gdRhss) =
  tGuardedRhss env tracing cr failCont gdRhss

tGuardedRhss :: Environment ->
                Tracing ->
                Bool ->  -- equal to parent?
                ContExp SrcSpanInfo ->  -- continuation in case of match failure
                [GuardedRhs SrcSpanInfo] ->
                (Exp SrcSpanInfo, ModuleConsts)
tGuardedRhss _ _ _ failCont [] =
  (continuationToExp failCont, moduleConstsEmpty)
tGuardedRhss env Traced cr failCont 
  (GuardedRhs _ [Qualifier l guard] exp : gdRhss) =
  (appN
    [mkExpGuard Traced
    ,mkExpSR l Traced
    ,expParent
    ,guard'
    ,Lambda l [patParent] exp'
    ,Lambda l [patParent] gdRhss']
  ,moduleConstsSpan l `moduleConstsUnion` guardConsts `moduleConstsUnion` 
     expConsts `moduleConstsUnion` gdRhssConsts)
  where
  (guard', guardConsts) = tExp env Traced False guard
  (exp', expConsts) = tExp env Traced cr exp
  (gdRhss', gdRhssConsts) = 
    tGuardedRhss env Traced cr failCont gdRhss
tGuardedRhss env Trusted cr failCont 
  (GuardedRhs _ [Qualifier l guard] exp : gdRhss) =
  (appN
    [mkExpGuard Trusted
    ,guard'
    ,exp'
    ,gdRhss']
  ,guardConsts `moduleConstsUnion` expConsts `moduleConstsUnion` gdRhssConsts)
  where
  (guard', guardConsts) = tExp env Trusted False guard
  (exp', expConsts) = tExp env Trusted cr exp
  (gdRhss', gdRhssConsts) = tGuardedRhss env Trusted cr failCont gdRhss
tGuardedRhss _ _ _ _ (GuardedRhs l _ _ : _) =
  notSupported l "statements in pattern guards"
  

-- Process foreign import:

tForeignImp :: SrcSpanInfo -> QName SrcSpanInfo -> Name SrcSpanInfo -> 
               Type SrcSpanInfo -> 
               ([Decl SrcSpanInfo], ModuleConsts)
tForeignImp l foreignName name ty =
  if arity == 0 
    then ([TypeSig l [letVarName] (tFunType ty)
          ,FunBind l [Match l letVarName [PVar l sr, patParent]
            (UnGuardedRhs l (appN 
              [mkExpConstUse Trusted
              ,Var l (UnQual l sr)
              ,expParent
              ,Var l (UnQual l shareName)]))
            Nothing]
          ,PatBind l (PVar l shareName) Nothing        
            (UnGuardedRhs l (appN 
              [mkExpConstDef Trusted
              ,expParent
              ,Var l (UnQual l (nameTraceInfoVar l Global name))
              ,Lambda l [patParent]
                 (appN
                   [expFrom ty, expParent, Var l foreignName])]))
            Nothing]
         ,moduleConstsEmpty)
    else ([TypeSig l [letVarName] (tFunType ty)
          ,FunBind l [Match l letVarName [PVar l sr, patParent]
            (UnGuardedRhs l 
              (mkExpFun Trusted (Var l (UnQual l sr))
                (Var l (UnQual l (nameTraceInfoVar l Global name)))
                (Var l (UnQual l workerName)) arity))
            Nothing]
          ,FunBind l [Match l workerName (map (PVar l) (args++[hidden]))
            (UnGuardedRhs l (appN
              [expFrom tyRes
              ,Var l (UnQual l hidden)
              ,appN (Var l foreignName : zipWith to tyArgs args)]))
            Nothing]]
         ,moduleConstsEmpty)
  where
  sr = nameSR name
  workerName = nameWorker name
  letVarName = nameTransLetVar name
  shareName = nameShare name
  args = take arity (nameArgs name)
  hidden = nameTrace2 name
  to :: Type SrcSpanInfo -> Name SrcSpanInfo -> Exp SrcSpanInfo
  to ty arg = appN [expTo ty, Var l (UnQual l hidden), Var l (UnQual l arg)]
    where
    l = ann ty
  arity = length tyArgs
  (tyArgs, tyRes) = decomposeFunType ty


-- Process class instances:

tInstHead :: InstHead SrcSpanInfo -> InstHead SrcSpanInfo
tInstHead (IHead l qname tys) = IHead l (nameTransCls qname) (map tType tys)
tInstHead (IHInfix l tyL qname tyR) = 
  IHInfix l (tType tyL) (nameTransCls qname) (tType tyR)
tInstHead (IHParen l instHead) = IHParen l (tInstHead instHead)

tInstDecls :: Environment -> Tracing ->
              [InstDecl SrcSpanInfo] ->
              ([InstDecl SrcSpanInfo], ModuleConsts)
tInstDecls env tracing instDecls =
  (concat instDeclss', foldr moduleConstsUnion moduleConstsEmpty declsConsts)
  where
  (instDeclss', declsConsts) = 
    unzip (map (tInstDecl env tracing) instDecls)

tInstDecl :: Environment -> Tracing ->
             InstDecl SrcSpanInfo -> 
             ([InstDecl SrcSpanInfo], ModuleConsts)
tInstDecl env tracing (InsDecl l decl) =
  (map (InsDecl l) decls', moduleConsts) 
  where
  (decls', moduleConsts) = tClassInstDecl env tracing decl
tInstDecl env tracing (InsType l _ _) =
  notSupported l "associated type definition"
tInstDecl env tracing (InsData l _ _ _ _) = 
  notSupported l "associated data type implementation"
tInstDecl env tracing (InsGData l _ _ _ _ _) =
  notSupported l "associated data type implementation using a GADT"
         

-- Process class declarations:

-- Transform any declarations in a class declaration.
tClassDecls :: Environment -> Tracing -> 
               [ClassDecl SrcSpanInfo] ->
               ([ClassDecl SrcSpanInfo], ModuleConsts)
tClassDecls env tracing classDecls =
  (concat classDeclss', foldr moduleConstsUnion moduleConstsEmpty declsConsts)
  where
  (classDeclss', declsConsts) = 
    unzip (map (tClassDecl env tracing) classDecls)

tClassDecl :: Environment -> Tracing -> 
              ClassDecl SrcSpanInfo -> 
              ([ClassDecl SrcSpanInfo], ModuleConsts)
tClassDecl env tracing (ClsDecl l decl) =
  (map (ClsDecl l) decls', moduleConsts) 
  where
  (decls', moduleConsts) = tClassInstDecl env tracing decl
tClassDecl env tracing (ClsDataFam l _ _ _) = 
  notSupported l "declaration of an associated data type"
tClassDecl env tracing (ClsTyFam l _ _) =
  notSupported l "declaration of an associated type synonym"
tClassDecl env tracing (ClsTyDef l _ _) =
  notSupported l "default choice for an associated type synonym"

-- Transform a standard declaration inside a class or instance declaration.
-- Basically patch the result of the standard transformation of such a 
-- declaration.
tClassInstDecl :: Environment -> Tracing ->
                  Decl SrcSpanInfo ->
                  ([Decl SrcSpanInfo], ModuleConsts)
tClassInstDecl env tracing decl@(FunBind _ _) =
  -- Worker needs to be local, because it does not belong to the 
  -- class / instance, nor can it be outside of it.
  -- (Cannot use arity optimisation for a method anyway.)
  ([FunBind l [addToWhere match workerDecls]], moduleConsts)
  where
  (FunBind l [match] : _ : workerDecls, moduleConsts) =
    tDecl env Local tracing decl
tClassInstDecl env tracing decl@(PatBind _ _ _ _ _) =
  -- Currently don't do any patching!
  -- Use of sharing variable needs to be qualified if class name needs to be
  -- qualified (still covers not all necessary cases)
  -- note when declaring instance the class may only be imported qualified
  -- What does the above mean??
  tDecl env Local tracing decl
tClassInstDecl env tracing decl@(TypeSig l names ty) =
  -- For every method type declaration produce an additional type declaration
  -- of the sharing variable.
  ([TypeSig l (map nameShare names) (tMapType tConstType ty), tySig'], moduleConsts)
  where
  ([tySig'], moduleConsts) = tDecl env Local tracing decl
  -- This should cover all declarations that can occur.

-- No changes in functional dependencies; all names are type variables.
tFunDep :: FunDep l -> FunDep l
tFunDep (FunDep l names1 names2) = FunDep l names1 names2

addToWhere :: Match l -> [Decl l] -> Match l
addToWhere (Match l name pats rhs Nothing) decls =
  Match l name pats rhs (Just (BDecls l decls))
addToWhere (Match l name pats rhs (Just (BDecls l' ds))) decls =
  Match l name pats rhs (Just (BDecls l' (ds ++ decls)))
addToWhere (InfixMatch l pl name pr rhs Nothing) decls =
  InfixMatch l pl name pr rhs (Just (BDecls l decls))
addToWhere (InfixMatch l pl name pr rhs (Just (BDecls l' ds))) decls =
  InfixMatch l pl name pr rhs (Just (BDecls l' (ds ++ decls)))

  -- Split a synonym into a core synonym plus several helpers.
  -- pre-condition: the declaration is a type synonym declaration.
  -- post-condition: all resulting declarations are type synonym declarations.
  -- The helper synonyms are necessary for the following reason:
  -- The known-arity optimisation requires that workers of functions with
  -- known arity are defined on the same level as their wrapper, not local
  -- to them. If the original function was recursive, the worker will be
  -- recursive instead of calling the wrapper (as without known-arity opt.).
  -- Hence if the original definition had a type signature, then the worker
  -- needs a type signature as well (the wrapper gets one anyway),
  -- because otherwise its inferred type might not be general enough 
  -- (polymorphic recursion) or too general (type class ambiguities,
  -- problems with existential types).
  -- Transformation of the original type signature into the worker type
  -- signature is not uniform: function types are handled specially.
  -- So if the type includes a type synonym it may not be possible to use
  -- the transformed type synonym, but the original one has to be expanded
  -- and transformed in this non-uniform way. However, in general a type
  -- synonym cannot be expanded, because the rhs might not be in scope at
  -- the synonym use site. Hence a type synonym is split into an outer part
  -- consisting of function types, type applications and type variables, 
  -- which can and may need to be expanded, and several inner type parts,
  -- for which new helper type synonyms are defined. These are always
  -- ex- and imported with the type synonym itself.
  -- A lot of effort, but it does work in the end.
splitSynonym :: Environment -> Decl SrcSpanInfo -> [Decl SrcSpanInfo]
splitSynonym env typeDecl@(TypeDecl span declHead ty) =
  typeDecl : zipWith mkTypeDecl (hrhss ty) [1..]
  where
  mkTypeDecl :: Type SrcSpanInfo -> Int -> Decl SrcSpanInfo
  mkTypeDecl hrhs no =
    TypeDecl span (mapDeclHead (flip nameTransTySynHelper no) declHead) hrhs
  hrhss ty = if tys == [ty] then [] else tys
    where
    tys = go ty []

  -- It is vital that this `go' agrees with the `go' in `splitSynonym' in
  -- AuxFile. Sadly the module structure of Hat is such that the two
  -- functions cannot sit next to each other (or be combined) without
  -- introducing a separate module for them.
  go :: Type SrcSpanInfo -> [Type SrcSpanInfo] -> [Type SrcSpanInfo]
  go ty@(TyForall l _ _ _) [] = [ty]
  go (TyFun l tyL tyR) [] = tyL : go tyR []
  go ty@(TyTuple _ _ _) [] = [ty]
  go ty@(TyList _ _) [] = [ty]
  go (TyApp l tyL tyR) tys = go tyL (tyR:tys)
  go (TyVar _ _) tys = []
  go ty@(TyCon l tyCon) tys 
    | isFunTyCon tyCon = case tys of
                           [] -> []
                           [ty] -> [ty]
                           [ty1,ty2] -> ty1 : (go ty2 [])
    | isExpandableTypeSynonym env tyCon 
    = go (expandTypeSynonym env tyCon tys) []  
    | otherwise = [tyAppN (ty:tys)]
  go (TyParen l ty) tys = map (TyParen l) (go ty tys)  -- a bit dubious
  go (TyInfix l tyL tyCon tyR) tys =
    if isExpandableTypeSynonym env tyCon
      then go (expandTypeSynonym env tyCon (tyL:tyR:tys)) []
      else []
  go (TyKind l ty kind) tys = notSupported l "kind annotation in type synonym"

mapDeclHead :: (Name l -> Name l) -> DeclHead l -> DeclHead l
mapDeclHead f (DHead l name tyVarBinds) = DHead l (f name) tyVarBinds
mapDeclHead f (DHInfix l tyVarBindL name tyVarBindR) = 
  DHInfix l tyVarBindL (f name) tyVarBindR
mapDeclHead f (DHParen l declHead) = DHParen l (mapDeclHead f declHead)

-- Process data type declarations:

{-
addConInfo :: QualConDecl SrcSpanInfo -> ModuleConsts -> ModuleConsts
addConInfo (QualConDecl _ _ _ condecl) =
  addConDeclModuleConsts condecl

addConDeclModuleConsts :: ConDecl SrcSpanInfo -> ModuleConsts -> ModuleConsts
addConDeclModuleConsts (ConDecl l name bangtypes) =
  addCon name [] 
addConDeclModuleConsts (InfixConDecl l btL name btR) =
  addCon name []
addConDeclModuleConsts (RecDecl l name fieldDecls) =
  addCon name (concatMap (\(FieldDecl _ names _) -> names) fieldDecls)
-}

tQualConDecl :: QualConDecl SrcSpanInfo -> QualConDecl SrcSpanInfo
tQualConDecl (QualConDecl l maybeTyVarBinds maybeContext condecl) =
  QualConDecl l (fmap (map tTyVarBind) maybeTyVarBinds)
    (fmap tContext maybeContext) (tConDecl condecl)

-- Just identity, no change.
tTyVarBind :: TyVarBind SrcSpanInfo -> TyVarBind SrcSpanInfo
tTyVarBind (KindedVar l name kind) = KindedVar l name kind
tTyVarBind (UnkindedVar l name) = UnkindedVar l name

tConDecl :: ConDecl SrcSpanInfo -> ConDecl SrcSpanInfo
tConDecl (ConDecl l name bangTys) = 
  ConDecl l (nameTransCon name) (map tBangType bangTys)
tConDecl (InfixConDecl l btL name btR) =
  InfixConDecl l (tBangType btL) (nameTransCon name) (tBangType btR)
tConDecl (RecDecl l name fieldDecls) =
  RecDecl l (nameTransCon name) (map tFieldDecl fieldDecls)

tFieldDecl :: FieldDecl SrcSpanInfo -> FieldDecl SrcSpanInfo
tFieldDecl (FieldDecl l fieldNames bangTy) =
  FieldDecl l (map nameTransField fieldNames) (tBangType bangTy)

tBangType :: BangType SrcSpanInfo -> BangType SrcSpanInfo
tBangType (BangedTy l ty) = BangedTy l (wrapType (tType ty))
tBangType (UnBangedTy l ty) = UnBangedTy l (wrapType (tType ty))
tBangType (UnpackedTy l ty) = UnpackedTy l (wrapType (tType ty))




-- Build field selectors:

mkFieldSelectors :: [QualConDecl SrcSpanInfo] -> 
                    ([Decl SrcSpanInfo], ModuleConsts)
mkFieldSelectors qualConDecls =
  foldr combine ([], moduleConstsEmpty) . map mkFieldSelector . nubBy eqName . 
    concatMap getFieldNamesFromQualConDecl $ qualConDecls
  where
  combine :: ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts)
  combine (decls1, modConsts1) (decls2, modConsts2) = 
    (decls1++decls2, modConsts1 `moduleConstsUnion` modConsts2)

-- construct the traced version of a field selector, using the 
-- normal field selector, i.e. from zname :: T -> R Int construct
-- gname :: SR -> Trace -> R (Fun T Int)
-- gname sr p = fun1 "name" hname sr p
-- hname :: Trace -> R T -> R Int
-- hname p (R v _) = projection mkNoSrcPos p (zname v)
mkFieldSelector :: Name SrcSpanInfo -> ([Decl SrcSpanInfo], ModuleConsts)
mkFieldSelector fieldName =
  ([FunBind l [Match l (nameTransLetVar fieldName) 
                [PVar l (nameSR fieldName), patParent]
                (UnGuardedRhs l 
                   (mkExpFun Trusted (Var l (UnQual l (nameSR fieldName)))
                     (Var l (UnQual l (nameTraceInfoVar l Global fieldName)))
                     (Var l (UnQual l wrappedName)) 1))
                Nothing]
   ,FunBind l [Match l wrappedName
                [wrapPat (PVar l varName) (PWildCard l), patParent]
                (UnGuardedRhs l (appN
                   [Var l (qNameProjection l)
                   ,mkExpSR l Trusted
                   ,expParent
                   ,App l (Var l (UnQual l (nameTransField fieldName))) 
                      (Var l (UnQual l varName))]))
                Nothing]]
  ,moduleConstsEmpty)
  where
  l = ann fieldName
  wrappedName = nameWorker fieldName
  varName:_ = nameArgs fieldName

-- Build the instance of class WrapVal for type with given data constructors.
-- This instance is needed for constructing and updating with labelled fields.
wrapValInstDecl :: Environment -> Tracing ->
                   Maybe (Context SrcSpanInfo) -> 
                   DeclHead SrcSpanInfo ->
                   [QualConDecl SrcSpanInfo] -> 
                   Decl SrcSpanInfo
wrapValInstDecl env tracing maybeContext declHead qualConDecls =
  InstDecl l maybeContext 
    (IHead l (qNameWrapValClass l) [dataDeclHeadToType declHead])
    (Just [InsDecl l (FunBind l (map wrapValMatch conDecls))])
  where
  l = ann declHead
  conDecls = map (\(QualConDecl _ _ _ conDecl) -> conDecl) qualConDecls
  
dataDeclHeadToType :: DeclHead l -> Type l
dataDeclHeadToType (DHead l name tyVarBinds) =
  tyAppN (TyCon l (UnQual l name) : map tyVarBindToType tyVarBinds)


tyVarBindToType :: TyVarBind l -> Type l
tyVarBindToType (KindedVar l name kind) = 
  TyParen l (TyKind l (TyVar l name) kind)
tyVarBindToType (UnkindedVar l name) = TyVar l name

wrapValMatch :: ConDecl SrcSpanInfo -> Match SrcSpanInfo
wrapValMatch conDecl =
  Match l (nameWrapValFun l) 
    [PVar l (nameSR (nameWrapValFun l))
    ,PAsPat l varName patConsApp
    ,patParent]
    (UnGuardedRhs l (wrapExp (Var l (UnQual l varName)) consAppTrace))
    Nothing
  where
  l = ann conDecl
  consAppTrace =
    if numOfArgs == 0
      then appN (Var l (qNameMkExpValueUse l) : expParent :
                   map (Var l . UnQual l) [srName, funAtom])
      else appN (Var l (qNameMkExpValueApp l numOfArgs) : expParent :
                   map (Var l . UnQual l) (srName : funAtom : traces))
  funAtom = nameTraceInfoCon consName
  patConsApp =
    PApp l (UnQual l consName) (map (wrapPat (PWildCard l) . PVar l) traces)
  consName = getConstructorFromConDecl conDecl
  traces = take numOfArgs (nameArgs (nameWrapValFun l)) :: [Name SrcSpanInfo]
  numOfArgs = getArityFromConDecl conDecl
  varName = nameTrace2 (nameWrapValFun l)
  srName = nameSR (nameWrapValFun l)
  
-- ---------------------------------------------------------------------------- -- Transform expressions


-- Boolean argument True iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
tExp :: Environment -> Tracing -> Bool -> Exp SrcSpanInfo ->
        (Exp SrcSpanInfo, ModuleConsts)
tExp env tracing cr exp = tExpA env tracing cr exp [] []

-- Order of both lists is innermost first.
-- All input expressions still need to be transformed.
tExpA :: Environment -> Tracing -> Bool -> Exp SrcSpanInfo -> 
         [SrcSpanInfo] ->  -- locations of applications of arguments in list
         [Exp SrcSpanInfo] ->  -- arguments from surrounding applications
         (Exp SrcSpanInfo, ModuleConsts)
tExpA env tracing cr (Var l qName) ls es 
  | Just a <- ari, a > 0, a <= length es, a <= 5, 
    let lApp = ls!!(a-1) =
  -- Known arity optimisation that calls worker directly
  tExpF env tracing (drop a ls) (drop a es) $
    (if isTraced tracing || isTracedQName env qName
       then mkExpApplyArity (isTracedQName env qName) (mkExpSR lApp tracing) (mkExpSR l tracing)
              a (Var noSpan (nameTraceInfoVar l Global qName)) expWorker es'
       else mkExpUWrapForward (appN (expWorker : es' ++ [expParent]))
    ,moduleConstsSpan l `moduleConstsUnion` moduleConstsSpan lApp `moduleConstsUnion` esConsts)
  where
  ari = arity env qName
  (es', esConsts) = tExps env tracing (take (fromJust ari) es)
  expWorker = Var noSpan (nameWorker qName)
tExpA env tracing cr (Var l qName) ls es =
  -- Unknown arity.
  tExpF env tracing ls es $
    if isLambdaBound env qName
      then 
        let e' = Var l (nameTransLambdaVar qName)
        in if cr
             then (mkExpProjection sr e', moduleConstsSpan l)
             else (e', moduleConstsEmpty)
      else
        (appN [Var l (nameTransLetVar qName), sr, expParent]
        ,moduleConstsSpan l)
  where
  sr = mkExpSR l tracing
tExpA _ _ _ (IPVar l _) _ _ =
  notSupported l "implicit parameter variable"
tExpA env tracing cr (Con l qName) ls es =
  tConApp env tracing qName ls es
tExpA env tracing cr (Lit l literal) [] [] =  -- no arguments possible
  (tLiteral env tracing literal, moduleConstsSpan (ann literal))
tExpA env tracing cr (InfixApp l e1 qop e2) ls es =
  tExpA env tracing cr 
    (App l (App (ann e1 <++> ann qop) (qOp2Exp qop) e1) e2) ls es
tExpA env tracing cr (App l e1 e2) ls es = 
  tExpA env tracing cr e1 (l:ls) (e2:es)
tExpA env tracing cr (NegApp l e) ls es =
  tExpA env tracing cr (App l (Var l (qNameDeriveSymbol "-" l)) e) ls es
tExpA env tracing cr (Lambda l pats body) ls es =
  tExpF env tracing ls es
    (mkExpFun tracing (mkExpSR l tracing) expMkAtomLambda fun funArity
    ,moduleConstsSpan l `moduleConstsUnion` bodyConsts)
  where
  fun = if neverFailingPats pats
          then Lambda l (pats' ++ [patParent]) body'
          else Lambda l (map (PVar l) nameVars ++ [patParent])
                 (Case l (mkExpTuple (map (Var l . UnQual l) nameVars))
                    [Alt l (mkPatTuple pats') (UnGuardedAlt l body') Nothing
                    ,Alt l (PWildCard l) (UnGuardedAlt l expFail) Nothing]
                 )
  (Match _ _ pats' (UnGuardedRhs _ body') _, bodyConsts) =
    tMatch env tracing True undefined failContinuation 
      (Match l undefined pats (UnGuardedRhs l body) Nothing)
  nameVars = take funArity (namesFromSpan l)
  funArity = length pats
tExpA env tracing cr (Let l binds body) ls es =
  tExpF env tracing ls es
    (Let l binds' body'
    ,moduleConstsEnv Local localEnv `moduleConstsUnion` bindsConsts `moduleConstsUnion` bodyConsts)
  where
  localEnv = bindsEnv (isTraced tracing) binds
  env2 = env `unionLocalRelation` localEnv
  (binds', bindsConsts) = tBinds env2 tracing binds
  (body', bodyConsts) = tExp env2 tracing cr body
tExpA env Traced cr (If l cond e1 e2) ls es =
  tExpF env Traced ls es
    (mkExpIf l Traced cond' 
       (Lambda l [patParent] e1') (Lambda l [patParent] e2')
    ,moduleConstsSpan l `moduleConstsUnion` condConsts `moduleConstsUnion` e1Consts `moduleConstsUnion` e2Consts)
  where
  (cond', condConsts) = tExp env Traced False cond
  (e1', e1Consts) = tExp env Traced True e1
  (e2', e2Consts) = tExp env Traced True e2
tExpA env Trusted cr (If l cond e1 e2) ls es =
  tExpF env Trusted ls es
    (mkExpIf l Trusted cond' e1' e2'
    ,condConsts `moduleConstsUnion` e1Consts `moduleConstsUnion` e2Consts)
  where
  (cond', condConsts) = tExp env Trusted False cond
  (e1', e1Consts) = tExp env Trusted True e1
  (e2', e2Consts) = tExp env Trusted True e2
tExpA env tracing cr (Case l exp alts) ls es =
  -- translate case into function f:
  -- case exp of
  --   pat1 -> exp1
  --   pat2 -> exp2
  --   ...
  -- ==>
  -- (let f pat1 = exp1; f pat2 = exp2;... in f) exp
  -- but not fully, as we don't want to trace function f and the application
  tExpF env tracing ls es
    (mkExpCase l tracing 
       (Let l (BDecls l (FunBind l matches' : decls'))
          (Var l (UnQual l funName))) exp'
    ,moduleConstsSpan l `moduleConstsUnion` expConsts `moduleConstsUnion` funConsts)
  where
  (funName : argName : funsNames) = namesFromSpan l
  (exp', expConsts) = tExp env tracing False exp
  (matches', decls', funConsts) =
    tMatches env tracing l funsNames [PVar l argName] 
      funName 1 True (map alt2Match alts)
tExpA env tracing cr (Do l stmts) ls es =
  tExpF env tracing ls es (tExpA env tracing cr (removeDo stmts) [] [])
tExpA env tracing cr (MDo l _) ls es =
  notSupported l "mdo-expression"
tExpA env tracing cr (Tuple l exps) [] [] =
  tConApp env tracing (Special l (TupleCon l Boxed a)) 
    (replicate a l) exps
  where 
  a = length exps
tExpA env tracing cr (TupleSection l maybeExps) ls es =
  -- desugar tuple section into lambda
  tExpA env tracing cr (desugarTupleSection l maybeExps) ls es
tExpA env tracing cr (List l []) [] [] = -- just the empty list, transform efficiently
  (appN [Var noSpan (qNameCon noSpan 0),mkExpSR l tracing,expParent
        ,Con noSpan (qNameConNil noSpan),Var noSpan (qNameTraceInfoConNil noSpan)]
  ,moduleConstsSpan l)
tExpA env tracing cr (List l exps) [] [] =
  -- use special combinator that transforms list at runtime;
  -- desugaring and subsequent transformation would lead to large program.
  (appN [expFromExpList, mkExpSR l tracing, expParent, List l exps']
  ,moduleConstsSpan l `moduleConstsUnion` expsConsts)
  where
  (exps', expsConsts) = tExps env tracing exps
tExpA env tracing cr (Paren l exp) ls es =
  tExpA env tracing cr exp ls es
tExpA env tracing cr (LeftSection l exp qOp) ls es =
  -- desugar into normal function application
  tExpA env tracing cr (App l (qOp2Exp qOp) exp) ls es
tExpA env tracing cr (RightSection l qOp exp) ls es =
  -- desugar into use of flip: (op e) = (flip op e)
  tExpA env tracing cr (App l (App l (mkExpDeriveFlip l) (qOp2Exp qOp)) exp) ls es
  -- -- desugar into a lambda abstraction
  -- tExpA env tracing cr
  --   (Lambda noSpan [PVar noSpan name] 
  --     (App l (App l (qOp2Exp qOp) (Var noSpan (UnQual noSpan name))) exp)) 
  --   ls es
  -- where
  -- name = nameFromSpan l
tExpA env tracing cr (RecConstr l qName fieldUpdates) ls es =
  tExpF env tracing ls es $
    (appN [expWrapValFun, mkExpSR l tracing, 
           RecUpdate l consUndefined fieldUpdates', expParent]
    ,moduleConstsSpan l `moduleConstsUnion` fieldsConsts)
  where
  consUndefined =
    if consArity == 0 
      then Con l (nameTransCon qName)
      else appN (Con l (nameTransCon qName) : replicate consArity
                   (appN [expUndefined, expSR, expParent]))
  Just consArity = arity env qName
  expSR = mkExpSR l Trusted
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env tracing) fieldUpdates
tExpA env Traced cr (RecUpdate l exp fieldUpdates) ls es =
  tExpF env Traced ls es $
    (Let l (BDecls l (zipWith mkFieldVarDecl fieldVarNames fieldExps')) 
       (appN (mkExpUpdate Traced (length labels)
             :mkExpSR l Traced
             :expParent
             :exp'
             :Lambda l [PVar l nameVar] 
               (RecUpdate l (Var l (UnQual l nameVar)) varFields')
             :labels ++ fieldVars))
    ,moduleConstsSpan l `moduleConstsUnion` expConsts `moduleConstsUnion` fieldsConsts)
  where
  mkFieldVarDecl :: Name SrcSpanInfo -> Exp SrcSpanInfo -> Decl SrcSpanInfo
  mkFieldVarDecl name exp =
    PatBind noSpan (PVar noSpan name) Nothing (UnGuardedRhs noSpan exp) Nothing
  (exp', expConsts) = tExp env Traced False exp
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env Traced) fieldUpdates
  labels = map (Var l . UnQual l . nameTraceInfoVar l Global) labelNames
  varFields' = zipWith (FieldUpdate l) (map fieldLabel fieldUpdates') fieldVars
  fieldExps' = map fieldExp fieldUpdates'
  fieldVars = map (Var l . UnQual l) fieldVarNames
  fieldVarNames = map nameShare labelNames
  labelNames = map (qName2Name . fieldLabel) fieldUpdates
  nameVar = nameFromSpan l
  fieldLabel (FieldUpdate _ qNameLabel _) = qNameLabel
  fieldLabel (FieldPun l _) = notSupported l "record field pun"
  fieldLabel (FieldWildcard l) = notSupported l "record field wildcard"
  fieldExp (FieldUpdate _ _ exp) = exp
  fieldExp (FieldPun l _) = notSupported l "record field pun"
  fieldExp (FieldWildcard l) = notSupported l "record field wildcard"
tExpA env Trusted cr (RecUpdate l exp fieldUpdates) ls es =
  tExpF env Trusted ls es $
    (appN [mkExpUpdate Trusted (-1) 
          ,expParent
          ,exp'
          ,Lambda l [PVar l nameVar] 
            (RecUpdate l (Var l (UnQual l nameVar)) fieldUpdates')]
    ,expConsts `moduleConstsUnion` fieldsConsts)
  where
  (exp', expConsts) = tExp env Trusted False exp
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env Trusted) fieldUpdates
  nameVar = nameFromSpan l
tExpA env tracing cr (EnumFrom l exp) ls es =
  -- desugar list enumeration [from ..]
  tExpA env tracing cr (App l (mkExpDeriveEnumFrom l) exp) ls es
tExpA env tracing cr (EnumFromTo l from to) ls es =
  tExpA env tracing cr (App l (App l (mkExpDeriveEnumFromTo l) from) to) ls es
tExpA env tracing cr (EnumFromThen l from the) ls es =
  tExpA env tracing cr 
    (App l (App l (mkExpDeriveEnumFromThen l) from) the) ls es
tExpA env tracing cr (EnumFromThenTo l from the to) ls es =
  tExpA env tracing cr
    (App l (App l (App l (mkExpDeriveEnumFromThenTo l) from) the) to) ls es
tExpA env tracing cr (ListComp l exp qualStmts) ls es =
  tExpF env tracing ls es $
    tExpA env tracing cr (desugarListComprehension l exp qualStmts) ls es
tExpA env tracing cr (ParComp l _ _) ls es =
  notSupported l "parallel list comprehension"
tExpA env tracing cr (ExpTypeSig l exp ty) ls es =
  tExpF env tracing ls es $
    (ExpTypeSig l exp' ty', expConsts)
  where
  (exp', expConsts) = tExp env tracing cr exp
  ty' = tConstType ty
tExpA env tracing cr (VarQuote l _) ls es =
  notSupported l "template Haskell reifying of expressions"
tExpA env tracing cr (TypQuote l _) ls es =
  notSupported l "template Haskell reifying of types"
tExpA env tracing cr (BracketExp l _) ls es =
  notSupported l "template Haskell bracket expression"
tExpA env tracing cr (SpliceExp l _) ls es =
  notSupported l "template Haskell splice expression"
tExpA env tracing cr (QuasiQuote l _ _) ls es =
  notSupported l "template Haskell quasi-quote"
tExpA env tracing cr (XTag l _ _ _ _) ls es =
  notSupported l "xml element with attributes and children"
tExpA env tracing cr (XETag l _ _ _) ls es =
  notSupported l "empty xml element, with attributes"
tExpA env tracing cr (XPcdata l _) ls es =
  notSupported l "PCDATA child element"
tExpA env tracing cr (XExpTag l _) ls es =
  notSupported l "escaped Haskell expression inside XML"
tExpA env tracing cr (XChildTag l _) ls es =
  notSupported l "children of an xml element"
tExpA env tracing cr (CorePragma l _ _) ls es =
  notSupported l "CORE pragma" 
tExpA env tracing cr (SCCPragma _ "" exp@(Lambda _ _ _)) ls es =
  -- This is a hack: desugaring of do-expressions uses empty SCC marker
  -- so that here we can say that the lambda is from a do-expression.
  tExpF env tracing ls es $ (appN [fun,expMkAtomDoLambda,sr,par,lambda], expConsts)
  where
  (App l1 (App l2 (App l3 (App l4 fun _) sr) par) lambda, expConsts) = tExp env tracing cr exp
tExpA env tracing cr (SCCPragma l str exp) ls es =
  tExpF env tracing ls es $
    (SCCPragma l str exp', expConsts)
  where
  (exp', expConsts) = tExp env tracing cr exp
tExpA env tracing cr (GenPragma l _ _ _ _) ls es =
  notSupported l "GENERATED pragma"
tExpA env tracing cr (Proc l _ _) ls es =
  notSupported l "arrows proc"
tExpA env tracing cr (LeftArrApp l _ _) ls es =
  notSupported l "arrow application from left"
tExpA env tracing cr (RightArrApp l _ _) ls es =
  notSupported l "arrow application from right"
tExpA env tracing cr (LeftArrHighApp l _ _) ls es =
  notSupported l "higher-order arrow application from left"
tExpA env tracing cr (RightArrHighApp l _ _) ls es =
  notSupported l "higher-order arrow application from right"

-- At end of transforming expressions possibly add deferred applications.
-- Lists are ordered from innermost to outermost.
-- Pre-condition: expression already transformed,
-- but expressions in list not yet transformed.
tExpF :: Environment -> Tracing ->
         [SrcSpanInfo] ->  -- locations of surrounding applications
         [Exp SrcSpanInfo] ->  -- arguments of surrounding apps
         (Exp SrcSpanInfo, ModuleConsts) -> 
         (Exp SrcSpanInfo, ModuleConsts)
tExpF _ _ [] [] result = result
tExpF env tracing ls es (e, consts) =
  (mkExpApply tracing (mkExpSR l tracing) (e : es'), 
  moduleConstsSpan l `moduleConstsUnion` esConsts `moduleConstsUnion` consts)
  where
  l = last ls
  (es', esConsts) = tExps env tracing es

-- Transform a list of expressions.
-- Mainly have to merge all constants.
tExps :: Environment -> Tracing -> [Exp SrcSpanInfo] ->
         ([Exp SrcSpanInfo], ModuleConsts)
tExps env tracing = mapMerge2 (tExp env tracing False)

mapMerge2 :: (a -> (b, ModuleConsts)) -> [a] -> ([b], ModuleConsts)
mapMerge2 f = mapSnd (foldr moduleConstsUnion moduleConstsEmpty) . unzip . map f

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x, f y)

-- Transform data constructor application.
-- Number of arguments may be smaller than arity of the data constructor.
-- (but never greater.)
tConApp :: Environment -> Tracing -> QName SrcSpanInfo -> 
           [SrcSpanInfo] -> [Exp SrcSpanInfo] -> 
           (Exp SrcSpanInfo, ModuleConsts)
tConApp env tracing qName ls es =
  (if conArity > numberOfArgs  -- undersaturated application
     then mkExpPartial sr conArity numberOfArgs qName es'
   else if conArity == numberOfArgs -- saturated application
     then mkExpCon sr conArity qName es'
   else
     error "TraceTrans.tConApp: data constructor with too many arguments."
  ,moduleConstsSpan lApp `moduleConstsUnion` esModuleConsts)
  where
  Just conArity = arity env qName  -- a constructor always has an arity
  numberOfArgs = length es
  lApp = last (ann qName : ls) -- location of outermost application (or just constructor itself)
  sr = mkExpSR lApp tracing
  (es',esModuleConsts) = tExps env tracing es

tLiteral :: Environment -> Tracing -> Literal SrcSpanInfo ->
            Exp SrcSpanInfo
tLiteral env tracing lit@(Char l c _) =
  mkExpConChar (mkExpSR l tracing) lit
tLiteral env tracing lit@(String l str _) =
  -- Because the result of transforming a list of characters would be very 
  -- large use special combinator that
  -- transforms string in traced string at runtime instead
  mkExpFromLitString (mkExpSR l tracing) lit
tLiteral env tracing lit@(Int l integer _) =
  mkExpApply tracing sr 
    [mkExpFromInteger sr, mkExpConInteger sr lit] 
  where
  sr = mkExpSR l tracing
tLiteral env tracing lit@(Frac l rational _) =
  -- desugar rational constant into explicit use of ":%",
  -- because Rational is not a primitive type but defined in PreludeBasic
  -- however, this way mkNTRational is not used at all
  mkExpApply tracing sr 
    [appN [expFromRational, sr, expParent]
    ,appN [Var l (qNameR l)
          ,appN [expConRational
                ,mkExpConInteger sr litNum
                ,mkExpConInteger sr litDenom] 
          ,appN [expMkAtomRational, sr, expParent, Lit noSpan lit]
    ]]
  where
  sr = mkExpSR l tracing
  Lit _ litNum = litInt noSpan (numerator rational)
  Lit _ litDenom = litInt noSpan (denominator rational)
tLiteral env tracing lit = 
  notSupported (ann lit) "unboxed literal"


-- return False if matching the pattern may fail
-- otherwise try to return True
-- (safe approximation)
neverFailingPat :: Pat l -> Bool
neverFailingPat (PVar _ _) = True
neverFailingPat (PNeg _ p) = neverFailingPat p
neverFailingPat (PTuple _ ps) = neverFailingPats ps
neverFailingPat (PParen _ p) = neverFailingPat p
neverFailingPat (PAsPat _ _ p) = neverFailingPat p
neverFailingPat (PWildCard _) = True
neverFailingPat (PIrrPat _ _) = True
neverFailingPat (PatTypeSig _ p _) = neverFailingPat p
neverFailingPat (PBangPat _ p) = neverFailingPat p
neverFailingPat _ = False

neverFailingPats :: [Pat l] -> Bool
neverFailingPats = all neverFailingPat

-- Minor transformation here.
-- The main step is in the construction or update expression for a record.
tField :: Environment -> Tracing -> FieldUpdate SrcSpanInfo -> 
          (FieldUpdate SrcSpanInfo, ModuleConsts)
tField env tracing (FieldUpdate l qName exp) =
  (FieldUpdate l (nameTransField qName) exp', expConsts)
  where
  (exp', expConsts) = tExp env tracing False exp
tField env tracing (FieldPun l name) =
  notSupported l "record field pun"
tField env tracing (FieldWildcard l) =
  notSupported l "record field wildcard"

-- Desugar do-statements
removeDo :: [Stmt SrcSpanInfo] -> Exp SrcSpanInfo
removeDo [Qualifier l exp] =  -- last stmt in do-expression
  exp
removeDo (Qualifier l exp : stmts) =
  appN [Var l (qNameDeriveGtGt l), exp, removeDo stmts]
removeDo (LetStmt l binds : stmts) =
  Let l binds (removeDo stmts)
removeDo (Generator l pat e : stmts) =
  appN [Var l (qNameDeriveGtGtEq l)
       ,e
       ,SCCPragma l "" $  -- hack to inform this really from a do-stmt
        if neverFailingPat pat
          then Lambda l [pat] (removeDo stmts)
          else Lambda l [PVar l newName]
                 (Case l (Var l (UnQual l newName))
                   [Alt l pat (UnGuardedAlt l (removeDo stmts)) Nothing
                   ,Alt l (PWildCard l)
                     (UnGuardedAlt l 
                       (App l (Var l (qNameDeriveFail l)) 
                              (Lit l (String l msg msg))))
                     Nothing])]
  where
  newName : _ = namesFromSpan l
  msg = "pattern-match failure in do-expression"

-- Desugar tuple section  (,4,,1)
-- Replace by a lambda abstraction  \x1 x2 -> (x1,4,x2,1)
desugarTupleSection :: SrcSpanInfo -> [Maybe (Exp SrcSpanInfo)] -> 
                       Exp SrcSpanInfo
desugarTupleSection l maybeExps =
  Lambda l (map (PVar l) varNames) (Tuple l (mkTuple maybeExps varNames))
  where
  arity = length (filter isNothing maybeExps)
  varNames = take arity (namesFromSpan l)
  mkTuple :: [Maybe (Exp SrcSpanInfo)] -> [Name SrcSpanInfo] -> 
             [Exp SrcSpanInfo]
  mkTuple [] _ = []
  mkTuple (Nothing : mes) (name : names) = 
    Var l (UnQual l name) : mkTuple mes names
  mkTuple (Just e : mes) names = e : mkTuple mes names

desugarListComprehension :: SrcSpanInfo -> Exp SrcSpanInfo -> 
                            [QualStmt SrcSpanInfo] -> 
                            Exp SrcSpanInfo
desugarListComprehension l exp qualStmts =
  App noSpan (trans (map unQual qualStmts)) (List noSpan [])
  where
  unQual :: QualStmt SrcSpanInfo -> Stmt SrcSpanInfo
  unQual (QualStmt _ stmt) = stmt
  unQual qs = notSupported l 
                "qualifier beyond a statement in list comprehension"
  trans [] = App l (mkExpCons l) exp
  trans (LetStmt l binds : stmts) = 
    Let l binds (trans stmts)
  trans (Qualifier l exp : stmts) =
    App l (App l (mkExpDeriveFilter l) exp) (trans stmts)
  trans (Generator l pat expG : stmts) =
    App l (App l (mkExpDeriveFoldr l) 
            (Lambda l [PVar l nameX, PVar l nameY]
                      (Case l (Var l (UnQual l nameX))
                        [Alt l pat (UnGuardedAlt l 
                                      (App l (trans stmts) 
                                         (Var l (UnQual l nameY)))) 
                           Nothing
                        ,Alt l (PWildCard l) (UnGuardedAlt l 
                                                (Var l (UnQual l nameY))) 
                           Nothing])))
       expG
    where
    nameX : nameY : _ = namesFromSpan l

-- ----------------------------------------------------------------------------
-- Transform patterns

tPats :: [Pat SrcSpanInfo] ->
         ([Pat SrcSpanInfo]    -- transformed patterns
         ,Maybe       -- only if there is a numeric pattern (k or n+k)
           (QName SrcSpanInfo   -- new name for numeric pattern
           ,Exp SrcSpanInfo     -- test for the numeric pattern (e.g. x==k)
           ,[Decl SrcSpanInfo]  -- binding for n if there is an n+k pattern
           ,[Exp SrcSpanInfo]   -- variables inserted in pattern after num pat
           ,[Pat SrcSpanInfo])) -- original patterns after num pat
tPats = mapCombinePats tPat

mapCombinePats :: (Pat SrcSpanInfo ->
      (Pat SrcSpanInfo, Maybe (a,c,d,[Exp SrcSpanInfo],[Pat SrcSpanInfo]))) ->
      [Pat SrcSpanInfo] ->
      ([Pat SrcSpanInfo], Maybe (a,c,d,[Exp SrcSpanInfo],[Pat SrcSpanInfo]))
mapCombinePats f [] = ([], Nothing)
mapCombinePats f (x:xs) =
  case f x of
    (pat, Nothing) -> (pat:pats, numPatInfos)
    (pat, Just (i,e,d,vs,ps)) -> (pat:xvps, Just (i,e,d,vs++xves,ps++xs))
  where
  (pats, numPatInfos) = mapCombinePats f xs
  xvps = map patToVar xs
  xves = map pVarToVar xvps

-- Replace every pattern by a variable pattern.
patToVar :: Pat SrcSpanInfo -> Pat SrcSpanInfo
patToVar (PVar l name) = PVar l (nameTransLambdaVar name)
patToVar (PAsPat l name pat) = PVar l (nameTransLambdaVar name)
patToVar pat = PVar l (nameFromSpan l)
  where
  l = ann pat

pVarToVar :: Pat l -> Exp l
pVarToVar (PVar l name) = Var l (UnQual l name)

-- Transform a pattern.
-- In particular handle numeric subpatterns, i.e. k and n+k.
-- Also create environment for occuring variables assuming they are
-- lambda-bound, except where declaration with let-binding created.
tPat :: Pat SrcSpanInfo ->
        (Pat SrcSpanInfo
        ,Maybe (QName SrcSpanInfo, Exp SrcSpanInfo, [Decl SrcSpanInfo]
               ,[Exp SrcSpanInfo], [Pat SrcSpanInfo]))
tPat (PVar l name) = (PVar l (nameTransLambdaVar name), Nothing)
tPat (PLit _ lit@(Char l c str)) = 
  (wrapPat (PLit l lit) (PWildCard l), Nothing)
tPat (PLit _ (String l s str)) =
  tPat . mkPatList l . map mkPatChar $ s
  where
  mkPatChar :: Char -> Pat SrcSpanInfo
  mkPatChar c = PLit l (Char l c (show c)) 
tPat (PLit _ lit@(Int l int str)) =
  (PVar l (nameTransLambdaVar nameNew)
  ,Just (UnQual l nameNew
        ,App l (App l (mkExpDeriveEqualEqual l)
                      (Var l (UnQual l nameNew))) 
               (Lit l lit)
        ,[],[],[]))
  where
  nameNew = nameFromSpan l
tPat (PLit _ lit@(Frac l rat str)) =
  (PVar l (nameTransLambdaVar nameNew)
  ,Just (UnQual l nameNew
        ,App l (App l (mkExpDeriveEqualEqual l) 
                      (Var l (UnQual l nameNew))) 
               (Lit l lit)
        ,[],[],[]))
  where
  nameNew = nameFromSpan l
tPat (PLit l lit) =
  notSupported l "unboxed literal"
tPat (PNeg l (PLit _ (Int _ int str))) = 
  -- a negative integer literal is represented as negate int
  tPat (PLit l (Int l (-int) ('-':str)))
tPat (PNeg l (PLit _ (Frac _ rat str))) =
  -- a negative floating point literal is represented as negate frac
  tPat (PLit l (Frac l (-rat) ('-':str))) 
tPat (PNeg l _) =
  notSupported l "negated pattern"
tPat (PNPlusK l n k) =
  (PVar l (nameTransLambdaVar nameNew)
  ,Just (UnQual l nameNew
        ,App l (App l (mkExpDeriveGreaterEqual l)
                      varNew)
               litK
        ,[PatBind l (PVar l n) Nothing 
           (UnGuardedRhs l (App l (App l (mkExpDeriveMinus l) varNew)
                                  litK)) 
           Nothing]
        ,[],[]))
  where
  nameNew = nameFromSpan l
  varNew = Var l (UnQual l nameNew)
  litK = Lit l (Int l k (show k))
tPat (PInfixApp l pL qName pR) =
  tPat (PApp l qName [pL, pR])
tPat (PApp l qName pats) =
  (wrapPat (PApp l (nameTransCon qName) pats') (PWildCard l)
  ,patsNumInfo)
  where
  (pats', patsNumInfo) = tPats pats
tPat (PTuple l pats) =
  tPat (PApp l (Special l (TupleCon l Boxed arity)) pats)
  where
  arity = length pats
tPat (PList l []) = -- just empty list constructor
  (wrapPat (PApp l (qNameConNil l) []) (PWildCard l), Nothing)
tPat (PList l pats) =
  tPat (mkPatList l pats)
tPat (PParen l pat) = 
  (PParen l pat', patNumInfo) 
  where
  (pat', patNumInfo) = tPat pat
tPat (PRec l qName patFields) =
  (wrapPat (PRec l (nameTransCon qName) patFields') (PWildCard l)
  ,numInfos)
  where
  (patFields', numInfos) = tPatFields patFields
tPat (PAsPat l name pat) =
  (PAsPat l (nameTransLambdaVar name) pat', patNumInfos)
  where
  (pat', patNumInfos) = tPat pat
tPat (PWildCard l) =
  (PWildCard l, Nothing)
tPat (PIrrPat l pat) =
  if isNothing patNumInfos
    then
      (case pat' of
         PApp l' nameR [p',t'] -> PApp l' nameR [PIrrPat l p', t']
         x                     -> x  -- wildcard, ...
      ,Nothing)
    else notSupported l "numeric literal or n+k inside ~ pattern"
  where
  (pat', patNumInfos) = tPat pat
tPat (PatTypeSig l pat ty) =
  (PatTypeSig l pat' (tConstType ty), patNumInfos)
  where
  (pat', patNumInfos) = tPat pat
tPat (PViewPat l _ _) =
  notSupported l "view pattern"
tPat (PRPat l _) =
  notSupported l "regular list pattern"
tPat (PXTag l _ _ _ _) =
  notSupported l "XML element pattern"
tPat (PXETag l _ _ _) =
  notSupported l "XML singleton element pattern"
tPat (PXPcdata l _) =
  notSupported l "XML PCDATA pattern"
tPat (PXPatTag l _) =
  notSupported l "XML embedded pattern"
tPat (PXRPats l _) =
  notSupported l "XML regular list pattern"
tPat (PExplTypeArg l _ _) =
  notSupported l "explicit generics style type argument"
tPat (PQuasiQuote l _ _) =
  notSupported l "quasi quote pattern"
tPat (PBangPat l pat) =
  -- strictness is currently ignored
  tPat pat


-- Rename field labels and process patterns.
tPatFields :: [PatField SrcSpanInfo] -> 
              ([PatField SrcSpanInfo]
              ,Maybe (QName SrcSpanInfo, Exp SrcSpanInfo, [Decl SrcSpanInfo]
                     ,[Exp SrcSpanInfo], [Pat SrcSpanInfo]))
tPatFields patFields =
  (patFields', numInfos)
  where
  (spans, names, pats) = unzipFields patFields
  (pats', numInfos) = mapCombinePats tPat pats
  patFields' = zipFields spans (map nameTransField names) pats'
  unzipFields :: [PatField SrcSpanInfo] ->
                 ([SrcSpanInfo], [QName SrcSpanInfo], [Pat SrcSpanInfo])
  unzipFields [] = ([],[],[])
  unzipFields (PFieldPat l qName pat : fields) = (l:ls, qName:names, pat:pats)
    where
    (ls, names, pats) = unzipFields fields
  unzipFields (PFieldPun l _ : _) = notSupported l "record field pun"
  unzipFields (PFieldWildcard l : _) = notSupported l "record field wildcard"
  zipFields :: [SrcSpanInfo] -> [QName SrcSpanInfo] -> [Pat SrcSpanInfo] ->
               [PatField SrcSpanInfo]
  zipFields = zipWith3 PFieldPat

-- apply data constructor R
wrapPat :: Pat SrcSpanInfo -> Pat SrcSpanInfo -> Pat SrcSpanInfo
wrapPat expVal expTrace = PApp noSpan (qNameR noSpan) [expVal, expTrace]

-- apply data constructor R
wrapExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo
wrapExp expVal expTrace = appN [Con noSpan (qNameR noSpan), expVal, expTrace]

mkPatList :: l -> [Pat l] -> Pat l
mkPatList l =
  foldr (\x xs -> PApp l cons [x,xs]) (PList l [])
  where
  cons = qNameCons l

-- ----------------------------------------------------------------------------
-- Abstract continuation for guards
--
-- To correctly create the trace within guards, a continuation is used.
-- The type ContExp should be abstract. Its implementation is only used in 
-- the following three functions.

data ContExp l = Fail | Function (QName l) [Exp l]

failContinuation :: ContExp l
failContinuation = Fail

functionContinuation :: QName l -> [Exp l] -> ContExp l
functionContinuation = Function

continuationToExp :: ContExp SrcSpanInfo -> Exp SrcSpanInfo
continuationToExp Fail = mkFailExp expParent
continuationToExp (Function fun args) =
  appN (Var l fun : args ++ [expParent])
  where
  l = ann fun

-- ----------------------------------------------------------------------------
-- Transform types

-- ty ==> R [[ty]]
tConstType :: Type SrcSpanInfo -> Type SrcSpanInfo
tConstType ty = wrapType (tType ty)

-- ty ==> RefSrcPos -> Trace -> R [[ty]]
tFunType :: Type SrcSpanInfo -> Type SrcSpanInfo
tFunType ty = 
  TyCon l (qNameRefSrcSpan l) `typeFun` TyCon l (qNameRefExp l) `typeFun` 
  wrapType (tType ty)
  where 
  l = ann ty

-- Leave any outer context unchanged but apply function to rest of type.
tMapType :: (Type SrcSpanInfo -> Type SrcSpanInfo) -> Type SrcSpanInfo -> Type SrcSpanInfo
tMapType f (TyForall l maybeBinds maybeContext ty) = TyForall l maybeBinds maybeContext (f ty)
tMapType f (TyParen l ty) = TyParen l (f ty)
tMapType f ty = f ty

-- Build type of worker from original type
tWorkerType :: Environment -> Arity -> Type SrcSpanInfo -> Type SrcSpanInfo
tWorkerType _ 0 ty =
  TyCon l (qNameRefExp l) `typeFun` wrapType (tType ty)
  where
  l = ann ty
tWorkerType env a ty = tWorkerSynExpand env a ty []

-- Expand a type synonym and then apply worker type transformation.
-- Need to collect type arguments.
tWorkerSynExpand :: Environment -> Arity -> Type SrcSpanInfo -> 
                    [Type SrcSpanInfo] -> 
                    Type SrcSpanInfo
tWorkerSynExpand env a (TyFun l tyL tyR) [] =
  wrapType (tType tyL) `typeFun` tWorkerType env (a-1) tyR
tWorkerSynExpand env a (TyCon l qname) [tyL, tyR] | isFunTyCon qname =
  wrapType (tType tyL) `typeFun` tWorkerType env (a-1) tyR
tWorkerSynExpand env a (TyCon l qname) tys =
  tWorkerType env a (expandTypeSynonym env qname tys)
tWorkerSynExpand env a (TyApp l tyL tyR) tys = 
  tWorkerSynExpand env a tyL (tyR : tys)
tWorkerSynExpand env a (TyParen l ty) tys =
  TyParen l (tWorkerSynExpand env a ty tys)
tWorkerSynExpand _ a ty tys = 
  error ("TraceTrans.tWorkerSynExpand: type must be a function but is not:\ntWorkerSynExpand env " ++ 
         show a ++ prettyPrint ty ++ concatMap ((" & "++) . prettyPrint) tys)


-- Just rewrite built-in type constructors, especially function type.
-- (Otherwise type names stay unchanged, they will refer to different modules.)
-- t1 -> t2 ==> Fun [[t1]] [[t2]]
tType :: Type SrcSpanInfo -> Type SrcSpanInfo
tType (TyForall l maybeTyVarBinds maybeContext ty) =
  TyForall l maybeTyVarBinds maybeContext (tType ty)
tType (TyFun l tyL tyR) = 
  tType (tyAppN [TyCon l (Special l (FunCon l)), tyL, tyR])
tType (TyTuple l boxed tys) =
  tType (tyAppN (TyCon l (Special l (TupleCon l boxed (length tys))) : tys))
tType (TyList l ty) =
  tType (TyApp l (TyCon l (Special l (ListCon l))) ty)
tType (TyApp l tyL tyR) = TyApp l (tType tyL) (tType tyR)
tType (TyVar l name) = TyVar l (nameTransTyVar name)
tType (TyCon l name) = TyCon l (nameTransTy name)
tType (TyParen l ty) = TyParen l (tType ty)
tType (TyInfix l tyL name tyR) = 
  TyInfix l (tType tyL) (nameTransTy name) (tType tyR)
tType (TyKind l ty kind) = TyKind l (tType ty) kind

-- ty ==> R ty
wrapType :: Type SrcSpanInfo -> Type SrcSpanInfo
wrapType ty = TyApp noSpan (TyCon noSpan (qNameR noSpan)) ty

-- Class contexts remain unchanged 
-- (except possibly for identifier qualifications and type renaming)
tContext :: Context SrcSpanInfo -> Context SrcSpanInfo
tContext (CxSingle l asst) = CxSingle l (tAsst asst)
tContext (CxTuple l assts) = CxTuple l (map tAsst assts)
tContext (CxParen l ctx) = CxParen l (tContext ctx)
tContext (CxEmpty l) = CxEmpty l

tAsst :: Asst SrcSpanInfo -> Asst SrcSpanInfo
tAsst (ClassA l qNameClass tys) = 
  ClassA l (nameTransCls qNameClass) (map tType tys)
tAsst (InfixA l tyL qNameClass tyR) =
  InfixA l (tType tyL) (nameTransCls qNameClass) (tType tyR)
tAsst (IParam l _ _) = notSupported l "implicit parameter assertion"
tAsst (EqualP l tyL tyR) = EqualP l (tType tyL) (tType tyR)



-- ----------------------------------------------------------------------------
-- New names
-- Module names and hence all qualifications are prefixed.
-- Names of classes, type constructors and type variables remain unchanged.
-- Names of data constructors remain unchanged.
-- (everything but expression variables)
-- As prefix characters only those characters can be chosen that do
-- not start a reserved identifier or operator. Otherwise the transformation
-- might create a reserved identifier.
-- (uppercase identifiers can be prefixed by such a character, because
-- a reserved identifier will never be created by prefixing)

-- names referring to traces (or parts thereof) of program fragments:

nameTraceInfoModule :: ModuleName l -> Name l
nameTraceInfoModule (ModuleName l ident) = Ident l ('t' : ident)

nameTraceInfoVar :: UpdId i => SrcSpanInfo -> Scope -> i -> i
nameTraceInfoVar span Global = prefixName 'a' '+'
nameTraceInfoVar span Local = prefixSpanName 'a' '+' span

nameTraceInfoGlobalVar :: UpdId i => i -> i
nameTraceInfoGlobalVar = prefixName 'a' '+'

nameTraceInfoCon :: UpdId i => i -> i
nameTraceInfoCon = prefixName 'a' '+'

nameTraceInfoSpan :: SrcSpanInfo -> Name SrcSpanInfo
nameTraceInfoSpan span = Ident span ('p' : showsEncodeSpan span "")

-- names referring to transformed program fragments:

-- The unqualfied names in the namespace of classes, types and type synonyms 
-- are left unchanged, but the module changes.
-- Names of some built-in type constructors (e.g ->, [], (,)) are changed!
-- Similarly type variable names are left unchanged.

nameTransCls :: UpdId i => i -> i
nameTransCls = updateId id 

nameTransTy :: UpdId i => i -> i
nameTransTy = updateId id 

nameTransSyn :: UpdId i => i -> i
nameTransSyn = updateId id 

nameTransTyVar :: Name l -> Name l
nameTransTyVar = id  -- unchanged

nameTransCon :: UpdId i => i -> i
nameTransCon = updateId id  -- unchanged

nameTransField :: UpdId i => i -> i
nameTransField = prefixName 'b' '^'

nameTransLetVar :: UpdId i => i -> i
nameTransLetVar = prefixName 'g' '!'

nameTransLambdaVar :: UpdId i => i -> i
nameTransLambdaVar = prefixName 'f' '&'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: UpdId i => i -> i
nameWorker = prefixName 'h' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: UpdId i => i -> i
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: UpdId i => i -> i
nameShare = prefixName 's' '|'

-- Turn a QName into a Name, assuming it will be prefixed.
-- Uses span to make qualified names unique even without qualification.
qName2Name :: QName SrcSpanInfo -> Name SrcSpanInfo
qName2Name (Qual l' _ name) = updateId update name
  where
  update :: Name l -> Name l
  update (Ident l id) = Ident l (showsEncodeSpan l' id)
  update (Symbol l id) = Symbol l (showsSymEncodeSpan l' id)
qName2Name (UnQual l name) = name
qName2Name (Special l _) = error "hat-trans: special name in binding position"

-- variable for a trace including span
nameTraceShared :: UpdId i => SrcSpanInfo -> i -> i
nameTraceShared = prefixSpanName 'j' '$'

-- variable for parent
nameParent :: Name SrcSpanInfo
nameParent = Ident noSpan "p"

-- variable for a trace
nameTrace :: UpdId i => i -> i
nameTrace = prefixName 'j' '$'

-- second variable for a trace
nameTrace2 :: UpdId i => i -> i
nameTrace2 = prefixName 'k' '@'

-- name for a local variable for a source reference
nameSR :: UpdId i => i -> i
nameSR = prefixName 'p' '%'

-- intermediate function
nameFunFromSpan :: SrcSpanInfo -> Name SrcSpanInfo
nameFunFromSpan span = Ident span ('h' : showsEncodeSpan span "n")

-- infinite list of var ids made from one id (for function clauses)
nameFuns :: UpdId i => i -> [i]
nameFuns = prefixNames 'y' '>'

-- infinite list of var ids made from one id (for naming arguments)
nameArgs :: UpdId i => i -> [i]
nameArgs = prefixNames 'z' '^'

-- a single id made from a span (different from below)
nameFromSpan :: SrcSpanInfo -> Name SrcSpanInfo
nameFromSpan span = Ident span ('v' : showsEncodeSpan span "n")

-- infinite list of ids made from a span
namesFromSpan :: SrcSpanInfo -> [Name SrcSpanInfo]
namesFromSpan span =
  map (Ident span . ('v':) . showsEncodeSpan span . ('v':) . show) 
    [1..]

-- Generation of new variables

showsEncodeSpan :: SrcSpanInfo -> ShowS
showsEncodeSpan span = shows beginRow . ('v':) . shows beginColumn . ('v':) 
  . shows endRow  . ('v':) . shows endColumn
  where
  (beginRow,beginColumn,endRow,endColumn) = getSpan span

showsSymEncodeSpan :: SrcSpanInfo -> ShowS
showsSymEncodeSpan span = 
  \xs -> numToSym (show beginRow) ++ '=' : numToSym (show beginColumn) ++ '=' 
    : numToSym (show endRow) ++ '=' : numToSym (show endColumn) ++ xs 
  where
  (beginRow,beginColumn,endRow,endColumn) = getSpan span

prefixName :: UpdId i => Char -> Char -> i -> i
prefixName c d = updateId update
  where
  update (Ident l name) = Ident l (c:name)
  update (Symbol l name) = Ident l (d:name)

-- really used with that general type?
prefixModName :: UpdId i => Char -> i -> i
prefixModName c = updateId update
  where
  update (Ident l name) = Ident l (c: map (\c->if c=='.' then '_' else c) name)

prefixSpanName :: UpdId i => Char -> Char -> SrcSpanInfo -> i -> i
prefixSpanName c d span = updateId update
  where
  update (Ident l name) = Ident l (c : showsEncodeSpan span name)
  update (Symbol l name) = Symbol l (d : showsSymEncodeSpan span name)

prefixNames :: UpdId i => Char -> Char -> i -> [i]
prefixNames c d name = map (($ name) . updateId . update) [1..]
  where
  update no (Ident l name) = Ident l (c : show no ++ name)
  update no (Symbol l name) = Symbol l (d : numToSym (show no) ++ name)

numToSym :: String -> String
numToSym = map (("!#$%&*+^@>" !!) . digitToInt)


-- ----------------------
-- Hardwired identifiers

expRoot :: Exp SrcSpanInfo
expRoot = expShortIdent "mkRoot"

mkExpSR :: SrcSpanInfo -> Tracing -> Exp SrcSpanInfo
mkExpSR l tracing = 
  Var noSpan (if isTraced tracing then UnQual l (nameTraceInfoSpan l) 
                                  else qNameMkNoSpan l)

mkExpProjection :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo
mkExpProjection sr e =
  appN [expProjection, sr, expParent, e]

mkExpApplyArity :: Bool ->  -- function is traced
                   Exp SrcSpanInfo -> -- location of application
                   Exp SrcSpanInfo -> -- location of applied function
                   Arity -> 
                   Exp SrcSpanInfo -> 
                   Exp SrcSpanInfo -> 
                   [Exp SrcSpanInfo] ->
                   Exp SrcSpanInfo
mkExpApplyArity tracedFun srA srF a info fun args =
  appN (Var noSpan ((if tracedFun then qNameApp else qNameUApp) noSpan a)
       :srA
       :srF
       :expParent
       :info
       :fun
       :args)

mkExpApply :: Tracing -> Exp SrcSpanInfo -> [Exp SrcSpanInfo] ->
              Exp SrcSpanInfo
mkExpApply tracing sr es =
  appN (Var noSpan ((if isTraced tracing then qNameAp else qNameUAp) noSpan a)
       :sr
       :expParent
       :es)
  where
  a = length es - 1  -- function is in list

mkExpPartial :: Exp SrcSpanInfo -> -- location of application
                Arity -> Arity -> 
                QName SrcSpanInfo -> [Exp SrcSpanInfo] ->
                Exp SrcSpanInfo
mkExpPartial sr conArity numberOfArgs qName es =
  appN (Var noSpan (qNamePa noSpan numberOfArgs) 
       :Con noSpan (nameTransCon qName)
       :Var noSpan (qNameCn noSpan (conArity-numberOfArgs))
       :sr
       :expParent
       :Var noSpan (nameTraceInfoCon qName)
       :es)

mkExpCon :: Exp SrcSpanInfo ->  -- location of constructor application
            Arity ->
            QName SrcSpanInfo -> [Exp SrcSpanInfo] -> 
            Exp SrcSpanInfo
mkExpCon sr arity qName es =
  appN (Var noSpan (qNameCon noSpan arity)
       :sr
       :expParent
       :Con noSpan (nameTransCon qName)
       :Var noSpan (nameTraceInfoCon qName)
       :es)

mkExpConChar :: Exp SrcSpanInfo -> Literal SrcSpanInfo -> Exp SrcSpanInfo
mkExpConChar sr lit =
  appN [expConChar, sr, expParent, Lit noSpan lit]

mkExpFromLitString :: Exp SrcSpanInfo -> Literal SrcSpanInfo -> 
                      Exp SrcSpanInfo
mkExpFromLitString sr lit =
  appN [expFromLitString, sr, expParent, Lit noSpan lit]

mkExpFromInteger :: Exp SrcSpanInfo ->
                    Exp SrcSpanInfo
mkExpFromInteger sr =
  appN [expFromInteger, sr, expParent]

mkExpConInteger :: Exp SrcSpanInfo ->
                   Literal SrcSpanInfo ->
                   Exp SrcSpanInfo
mkExpConInteger sr lit =
  appN [expConInteger, sr, expParent, Paren noSpan (Lit noSpan lit)]

mkExpFun :: Tracing -> Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo ->
            Arity ->
            Exp SrcSpanInfo
mkExpFun tracing sr funName fun a =
  appN [Var noSpan ((if isTraced tracing then qNameFun else qNameUFun) noSpan a)
       ,funName
       ,sr
       ,expParent
       ,fun]

mkExpIf :: SrcSpanInfo -> Tracing -> 
           Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo -> 
           Exp SrcSpanInfo
mkExpIf l Traced cond e1 e2 =
  appN [Var noSpan (qNameIf noSpan),mkExpSR l Traced,expParent,cond,e1,e2]
mkExpIf l Trusted cond e1 e2 =
  appN [Var noSpan (qNameUIf noSpan),expParent,cond,e1,e2]

mkExpCase :: SrcSpanInfo -> Tracing -> Exp SrcSpanInfo -> Exp SrcSpanInfo ->
             Exp SrcSpanInfo
mkExpCase l tracing caseFun arg =
  appN [Var noSpan ((if isTraced tracing then qNameCase else qNameUCase) noSpan)
       ,mkExpSR l tracing
       ,expParent
       ,caseFun
       ,arg]

mkExpUWrapForward :: Exp SrcSpanInfo -> Exp SrcSpanInfo
mkExpUWrapForward exp = 
  appN [Var noSpan (qNameUWrapForward noSpan)
       ,expParent
       ,exp]

expShortIdent :: String -> Exp SrcSpanInfo
expShortIdent ident = Var noSpan (qNameShortIdent ident noSpan)



-- ----------------------------------------------------------------------------
-- Wrapping of untransformed code

expTo :: Type SrcSpanInfo -> Exp SrcSpanInfo
expTo = expType True

expFrom :: Type SrcSpanInfo -> Exp SrcSpanInfo
expFrom = expType False

-- The following assumes a limited form of types as they
-- occur in foreign import / export declarations.
-- Variables of kind other than * pose a problem.
expType :: Bool -> Type SrcSpanInfo -> Exp SrcSpanInfo
expType to (TyForall l _ _ _) = notSupported l "local type forall"
expType to (TyFun l tyL tyR) =
  appN
    [Var l (qNameShortIdent (prefix to ++ "Fun") l)
    ,expType (not to) tyL
    ,expType to tyR]
expType to (TyTuple l boxed tys) =
  appN
    (Var l (qNameShortIdent (prefix to ++ "Tuple" ++ show (length tys)) l) :
     map (expType to) tys)
expType to (TyList l ty) =
  appN [Var l (qNameShortIdent (prefix to ++ "List") l), expType to ty]
expType to (TyApp l tyL tyR) = 
  App l (expType to tyL) (expType to tyR)
expType to (TyVar l _) =
  Var l (qNameShortIdent (prefix to ++ "Id") l)
expType to (TyCon l qName) = 
  Var l (qNameShortIdent (prefix to ++ getName qName) l)
  where
  getName :: QName SrcSpanInfo -> String
  getName (Special _ specialCon) = specialToId specialCon
  getName qName = getId qName
expType to (TyParen l ty) = Paren l (expType to ty)
expType to (TyInfix l tyL qName tyR) = expType to (TyApp l (TyApp l (TyCon l qName) tyL) tyR)
expType to (TyKind l ty kind) = notSupported l "type with kind"

prefix :: Bool -> String
prefix True = "to"
prefix False = "from"
  

-- ----------------------------------------------------------------------------
-- Useful stuff


-- Frequently used in transformed code:

mkExpTuple :: [Exp SrcSpanInfo] -> Exp SrcSpanInfo
mkExpTuple es = Tuple noSpan es

mkPatTuple :: [Pat SrcSpanInfo] -> Pat SrcSpanInfo
mkPatTuple ps = PTuple noSpan ps 

mkExpList :: [Exp SrcSpanInfo] -> Exp SrcSpanInfo
mkExpList =
  foldr (\x xs -> appN [mkExpCons noSpan,x,xs]) (List noSpan [])

mkFailExp :: Exp l -> Exp l
mkFailExp parent = App l (Var l (qNameFatal l)) parent
  where
  l = ann parent

expFail :: Exp SrcSpanInfo
expFail = mkFailExp expParent

patParent :: Pat SrcSpanInfo
patParent = PVar noSpan nameParent

expParent :: Exp SrcSpanInfo
expParent = Var noSpan (UnQual noSpan nameParent)


-- Test for specific names

-- Is this the module name "Main"?
isMain :: ModuleName l -> Bool
isMain (ModuleName l name) = name == "Main"

-- This test is an unsafe hack.
-- Even with qualification "Prelude" this may be a different "True"
-- There is no origin-tracking
isTrue :: QName l -> Bool
isTrue (Qual _ (ModuleName _ "Prelude") (Ident _ "True")) = True
isTrue (UnQual _ (Ident _ "True")) = True
isTrue _ = False

isOtherwise :: QName l -> Bool
isOtherwise (Qual _ (ModuleName _ "Prelude") (Ident _ "otherwise")) = True
isOtherwise (UnQual _ (Ident _ "otherwise")) = True
isOtherwise _ = False

-- ----------------------------------------------------------------------------

expMkAtomRational :: Exp SrcSpanInfo
expMkAtomRational = Var noSpan (qNameMkAtomRational noSpan)

expMkAtomLambda :: Exp SrcSpanInfo
expMkAtomLambda = Var noSpan (qNameMkAtomLambda noSpan)

expMkAtomDoLambda :: Exp SrcSpanInfo
expMkAtomDoLambda = Var noSpan (qNameMkAtomDoLambda noSpan)

mkExpConstUse :: Tracing -> Exp SrcSpanInfo
mkExpConstUse tracing =
  Var noSpan 
    (if isTraced tracing then qNameConstUse noSpan else qNameUConstUse noSpan)

mkExpConstDef :: Tracing -> Exp SrcSpanInfo
mkExpConstDef tracing =
  Var noSpan 
    (if isTraced tracing then qNameConstDef noSpan else qNameUConstDef noSpan)

mkExpGuard :: Tracing -> Exp SrcSpanInfo
mkExpGuard tracing =
  Var noSpan 
    (if isTraced tracing then qNameGuard noSpan else qNameUGuard noSpan)

mkExpUpdate :: Tracing -> Arity -> Exp SrcSpanInfo
mkExpUpdate tracing arity =
  Var noSpan 
    (if isTraced tracing then qNameUpdate noSpan arity else qNameUUpdate noSpan)

expProjection :: Exp SrcSpanInfo
expProjection = Var noSpan (qNameProjection noSpan)

expConChar :: Exp SrcSpanInfo
expConChar = Var noSpan (qNameConChar noSpan)

expConInteger :: Exp SrcSpanInfo
expConInteger = Var noSpan (qNameConInteger noSpan)

expFromLitString :: Exp SrcSpanInfo
expFromLitString = Var noSpan (qNameFromLitString noSpan)

expFromExpList :: Exp SrcSpanInfo
expFromExpList = Var noSpan (qNameFromExpList noSpan)

expWrapValFun :: Exp SrcSpanInfo
expWrapValFun = Var noSpan (UnQual noSpan (nameWrapValFun noSpan))

expTraceIO :: Exp SrcSpanInfo
expTraceIO = Var noSpan (qNameShortIdent "traceIO" noSpan)

