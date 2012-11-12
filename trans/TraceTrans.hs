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
import System.FilePath (takeBaseName)
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Data.Set (Set)
import qualified Data.Set as Set

-- ----------------------------------------------------------------------------
-- central types

data Tracing = Traced | Trusted

isTraced :: Tracing -> Bool
isTraced Traced = True
isTraced Traced = False

data Scope = Global | Local

isLocal :: Scope -> Bool
isLocal Local = True
isLocal Global = False

type Arity = Int

-- ----------------------------------------------------------------------------
-- Transform a module

traceTrans :: 
  FilePath -> -- complete filename of the module (essential for module Main)
  Tracing -> -- whether transforming for tracing or trusting
  Environment -> -- contains already info about all imported identifiers
  Module SrcSpanInfo -> 
  Module SrcSpanInfo  -- some srcSpanInfo will be fake
traceTrans moduleFilename tracing env
  (Module span maybeModuleHead modulePragmas impDecls decls) =
  Module span (fmap (tModuleHead env declsExported maybeModuleHead) 
    (map tModulePragma modulePragmas) 
    (tImpDecls tracing impDecls)
    (declsExported ++
      [PatBind span patParent Nothing 
        (UnGuardedRhs span (Var span (qNameMkRoot span))) Nothing] ++
      [defNameMod pos modId filename traced] ++ 
      map (defNameVar Global Local modTrace) mvars ++ 
      map (defNameVar Local Local modTrace) vars ++ 
      (if traced then map (defNameSpan modTrace) poss else []) 
  where
  modId = maybe "Main" getModuleId maybeModuleHead
  declsExported = decls' ++ conNameDefs ++ glabalVarNameDefs ++
                    if isMain modId 
                      then [defMain traced (traceBaseFilename moduleFilename)] 
                      else []
  conNameDefs = map (defNameCon modTrace) cons 
  globalVarNameDefs = map (defNameVar Global Global modTrace) tvars 
  modId' = nameTransModule modId
  modTrace = ExpVar pos (nameTraceInfoModule modId)
  (poss,tvars,vars,mvars,cons) = getModuleConsts consts
  (decls',consts) = tDecls Global tracing decls
traceTrans _ _ (XmlPage span _ _ _ _ _ _) = notSupported span "XmlPage"
traceTrans _ _ (XmlHybrid span _ _ _ _ _ _ _ _) = notSupported span "XmlHybrid"

-- For the complete filename of a module yields the base filename for the 
-- trace file.
-- Pre-condition: The module is a Main module
traceBaseFilename :: FilePath -> FilePath
traceBaseFilename = takeBaseName

-- obtain the module identifier
getModuleId :: ModuleHead l -> String
getModuleId (ModuleHead _ (ModuleName _ modId) _ _) = modId

tModuleHead :: ModuleHead SrcSpanInfo -> ModuleHead SrcSpanInfo
tModuleHead env decls
  (ModuleHead span moduleName maybeWarningText maybeExportSpecList) =
  ModuleHead span (nameTransModule moduleName) 
    (fmap tWarningText maybeWarningText)
    (tMaybeExportSpecList (\(ModuleName _ modId) -> modId) moduleName) 
      env maybeExportSpecList decls)

-- warnings stay unchanged
tWarningText :: WarningText l -> WarningText l
tWarningText w = w

-- not all module pragmas can be transformed
tModulePragma :: ModulePragma l -> ModulePragma l
tModulePragma (LanguagePragma l names) = LanguagePragma l names
tModulePragma (OptionsPragma l maybeTool string) = 
  OptionsPragma l maybeTool string
tModulePragma (AnnModulePragma l _) = 
  notSupported l "ANN pragma with module scope"
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
  (Just (ExportSpecList span hiding exportSpecs)) decls =
  Just (ExportSpecList span (concatMap (tExportSpec env) hiding exportSpecs))
 
tExportSpec :: Environment -> ExportSpec l -> [ExportSpec l]
tExportSpec env (EVar span qname) = 
  map (EVar span) (tEntityVar env qname)
tExportSpec env (EAbs span qname) =
  EAbs span qname' : map (EVar span) qnames'
  where
  (qname', qnames') = tEntityAbs env qname
tExportSpec env (EThingAll span qname) =
  case clsTySynInfo env qname of
    Cls _ -> [EThingAll span (nameTransCls qname)]
    Ty cons fields -> 
      tExportSpec (EThingWith span qname 
                    (map conName cons ++ map fieldName fields))
  where
  conName str = ConName span (Symbol span str)
  fieldName str = VarName span (Identifier span str)
tExportSpec env (EThingWith span qname cnames) =
  EThingWith span qname' cnames' : map (EVar span) qnames'
  where
  (qname', cnames', qnames') = tEntityThingWith env qname cnames
tExportSpec env (EModuleContents span moduleName@(ModuleName _ moduleId)) =
  if thisModuleId == moduleId 
    then concatMap makeExport decls
    else [EModuleContents span (nameTransModule moduleName)]

-- Produce export entities from the declarations generated by the transformation
-- These are all entities defined in this module and meant for export.
makeExport :: Decl SrcSpanInfo -> [ExportSpec SrcPanInfo]
makeExport (TypeDecl l declHead _) = [EAbs l (getDeclHeadName declHead)]
makeExport (TypeFamDecl l declHead _) = [EAbs l (getDeclHeadName declHead)]
makeExport (DataDecl l _ _ declHead _ _) =
  [EThingAll l (getDeclHeadName declHead)]
makeExport (GDataDecl l _ _ declHead _ _ _) =
  [EThingAll l (getDeclHeadName declHead)]
makeExport (ClassDecl l _ declHead _ _) =
  [EThingAll l (getDeclHeadName declHead)]
makeExport (FunBind l match) =
  if exportedTransName name then [EVar l (Unqual l name)] else []
  where
  name = getMatchName match
makeExport (PatBind l (PVar l' name) _ _ _) =
  if exportedTransName name then [EVar l (Unqual l name)] else []
makeExport _ = []

-- Checks whether this is the name of a function that should be exported 
-- by the module.
exportedTransName :: Name l -> Bool
exportedTransName (Ident _ name) = head name `elem` ['g','a','h']
exportedTransName (Symbol _ name) = head name `elem` ['!','+','*']

getMatchName :: Match l -> Name l
getMatchName (Match _ name _ _ _) = name
getMatchName (InfixMatch _ _ name _ _ _) = name

getDeclHeadName :: DeclHead l -> Name l
getDeclHeadName (DHead _ name _) = name
getDeclHeadName (DHInfix _ _ name _) = name
getDeclHeadName (DHParen _ declHead) = getDeclHeadName declHead

-- ----------------------------------------------------------------------------
-- Produce imports

tImpDecls :: Environment -> [ImportDecl l] -> [ImportDecl l]
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
  : ImportDecl {importAnn = noSpan,
                importModule = ModuleName noSpan "Hat.Hack",
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Nothing}
    -- For list syntax : and [].
    -- Is that really needed?
  : ImportDecl {importAnn = noSpan,
                importModule = ModuleName noSpan "Hat.Hat",
                importQualified = True,
                importSrc = False,
                importPkg = Nothing,
                importAs = Just (ModuleName noSpan "T",
                importSpecs = Nothing}
  -- All types and combinators for tracing, inserted by the transformation
  : map (tImportDecl env) impDecls

tImportDecl :: Environment -> ImportDecl l -> ImportDecl l
tImportDecl env importDecl = 
  if isJust (importSrc importDecl) 
    then unsupported (importAnn importDecl) "{-# SOURCE #-}"
  else if isJust (importPkg importDecl)
    then unsupported (importAnn importDecl) "explicit package name"
  else 
    importDecl{importModule = nameTransModule (importModule importDecl),
               importAs = fmap nameTransModule (importAs importDecl),
               importSpecs = fmap (tImportSpecList env) (importSpecs importDecl)
              }

tImportSpecList :: Environment -> ImportSpecList l -> ImportSpecList l
tImportSpecList env (ImportSpecList l hiding importSpecs) =
  ImportSpecList l hiding (concatMap (tImportSpec env) importSpecs)
      
-- Nearly identical with tExportSpec except for the types.  
tImportSpec :: Environment -> ImportSpec l -> [ImportSpec l]     
tImportSpec env (IVar span qname) = 
  map (IVar span) (tEntityVar env qname)
tImportSpec env (IAbs span qname) =
  IAbs span qname' : map (IVar span) qnames'
  where
  (qname', qnames') = tEntityAbs env qname
tImportSpec env (IThingAll span qname) =
  case clsTySynInfo env qname of
    Cls _ -> [IThingAll span (nameTransCls qname)]
    Ty cons fields -> 
      tExportSpec (IThingWith span qname 
                    (map conName cons ++ map fieldName fields))
  where
  conName str = ConName span (Symbol span str)
  fieldName str = VarName span (Identifier span str)
tImportSpec env (IThingWith span qname cnames) =
  IThingWith span qname' cnames' : map (IVar span) qnames'
  where
  (qname', cnames', qnames') = tEntityThingWith env qname cnames
tImportSpec env (IModuleContents span moduleName) =
  [IModuleContents span (nameTransModule moduleName)]

-- ----------------------------------------------------------------------------
-- Produce entities in either import or export list

tEntityVar :: Environment -> QName l -> [QName l]
tEntityVar env qname = 
  qNameLetVar qname : 
  case arity env qname of
    Just a | a > 0 -> [nameTraceInfoGlobalVar qname, nameWorker qname, 
                      nameShare qname]            
    Just (-1)      -> [nameShare qname]
    _              -> []

-- a class or datatype ex-/imported abstractly, or a type synonym
tEntityAbs :: Environment -> QName l -> [QName l]
tEntityAbs env qname =
  case clsTySynInfo env qname of
    Cls _ -> [nameTransCls qname]
    Ty _ _ -> [nameTransTy qname]
    Syn helpNo -> nameTransSyn qname : 
                    map (nameTransSynHelper qname) [1..helpNo]

-- a class with some methods or a datatype with some constructors/fields
tEntityThingWith :: Environment -> QName l -> [CName l] -> 
                    (QName l, [CName l], [QName l])
tEntityThingWith env qname cnames =
  case clsTySynInfo env qname of
    Cls _ -> (nameTransCls qname, 
             map nameTransLetVar cnames ++ map nameShare cnames,
             [])
    Ty _ _ -> (nameTransTy qname,
              map nameTransCon consNames ++ map nameTransField fieldNames,
              map nameTransLetVar fieldNames ++
              map nameWorker fieldNames ++
              map nameTraceInfoGlobalVar fieldNames ++
              map nameTraceInfoCon consNames)     
  where
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

defNameMod :: ModuleName -> String -> Bool -> Decl l
defNameMod modName@(ModuleName l modId) filename traced =
  PatBind l (PVar l (nameTraceInfoModule modName)) Nothing
    (UnGuardedRhs l
      (appN l
        [Var l nameMkModule
        ,litString l modId
        ,litString l filename
        ,Con l (if traced then qNamePreludeTrue l else qNamePreludeFalse l)]))
    Nothing

defNameCon :: Environment -> 
              Exp SrcSpanInfo -> 
              (Name SrcSpanInfo, [Name SrcSpanInfo]) -> 
              Decl SrcSpanInfo
defNameCon env moduleTrace (conName, fieldNames) =
  PatBind l (PVar l (nameTraceInfoCon conName)) Nothing
    (UnGuardedRhs l
      (appN l
        (Var l (qNameHatMkAtomConstructor l withLabels) :
         moduleTrace :
         encodeSpan l ++
         litInt l (fixPriority env conName) :
         litInt l (fromJust (arity env conName)) :
         litString l ident) :
         if withFields
           then (:[]) . mkList l .
                  map (Var l . UnQual l . nameTraceInfoVar l Global) $ 
                  fieldNames
           else []
       )))
    Nothing
  where
  l = ann conName
  ident = getId conName
  withFields = not (null fieldNames)

defNameVar :: Scope -> Scope -> Exp SrcSpanInfo -> Name SrcSpanInfo -> 
              Decl SrcSpanInfo
defNameVar defScope visScope moduleTrace varName =
  PatBind l (PVar l (nameTraceInfoVar visScope varName)) Nothing
    (UnGuardedRhs l
      (appN l
        (Var l (qNameHatMkAtomVariable l)) :
         moduleTrace :
         encodeSpan l ++
         [litInt l (fixPriority env varName),
           -- all identifiers in definition position are assumed to 
           -- be equipped with an arity; 
           -- only those defined by pattern bindings do not; they have arity 0.
          litInt l (maybe 0 (arity env varName)),
          litString l (getId varName),
          Con l (if isLocal defScope then qNamePreludeTrue l 
                                     else qNamePreludeFalse l)])) 
    Nothing
  where
  l = ann varName

defNameSpan :: Exp SrcSpanInfo -> SrcSpanInfo -> Decl SrcSpanInfo
defNameSpan moduleTrace span =
  PatBind l (PVar l (nameTraceInfoSpan span)) Nothing
    (UnGuardedRhs l
      (appN l
        (Var l (qNameHatMkSpan l) :
         moduleTrace :
         encodeSpan span)))
    Nothing
  where
  l = span

-- Encode a span in the trace file
encodeSpan :: SrcSpanInfo -> [Exp SrcSpanInfo]
encodeSpan SrcSpanInfo{srcInfoSpan=
             SrcSpan{srcSpanStartLine=beginRow
                    ,srcSpanStartColumn=beginCol
                    ,srcSpanEndLine=endRow
                    ,srcSpanEndColumn=endCol}} =
  [litInt (10000*beginRow + beginCol)
  ,litInt (10000*endRow + endCol)]
-- ----------------------------------------------------------------------------
-- Abstract data type for keeping track of constants introduced by the
-- transformation.
-- Implements sets of spans, defined this-level and local variables, 
-- defined methods and defined constructors (no duplicates)
-- this-level means defined on the currently considered declaration level,
-- local means defined in some declaration local to the current declaration.
-- Variables and constructors come with the span at which they are defined.
-- Pre-condition: a constructor is only added once.
-- A variable with span may be added several times, because
-- the span may be zero. (really?) 
-- Because same span may be used for a variable, an application etc,
-- a position may be added several times. (really?)
-- Maybe could use lists instead of sets, because no duplicates occur anyway?
-- The scope states if the variable is defined globally or locally.

data ModuleConsts = 
  MC (Set SrcSpan)  -- spans used in traces
    Set (Name SrcSpanInfo)  -- this-level variable ids for traces
    Set (Name SrcSpanInfo)  -- local variable ids for use in traces
    Set (Name SrcSpanInfo)  -- ids for methods for use in trace
    [(Name SrcSpanInfo,[Name SrcSpanInfo])]  
                            -- constructor ids for use in traces
                            -- together with field labels (global)

emptyModuleConsts :: ModuleConsts
emptyModuleConsts = MC Set.empty Set.empty Set.empty Set.empty Set.empty

addSpan :: SrcSpanInfo -> ModuleConsts -> ModuleConsts
addSpan ssi (MC poss tids ids mids cons) = 
  MC (Set.insert (srcInfoSpan ssi) poss) tids ids mids cons

-- pre-condition: name is a variable
addVar :: Name SrcSpanInfo -> ModuleConsts -> ModuleConsts
addVar name (MC poss tids ids mids cons) = 
  MC (Set.insert (srcInfoSpan (ann name)) poss) 
    (Set.insert name tids) ids mids cons

-- pre-condition: name is a data constructor
addCon :: Name SrcSpanInfo -> [Name SrcSpanInfo] -> ModuleConsts -> 
          ModuleConsts
addCon name fields (MC poss tids ids mids cons) =
  MC (Set.insert (srcInfoSpan (ann name)) poss) tids ids mids 
    ((name,fields) : cons)

-- reclassify this-level variables as methods
classifyMethods :: ModuleConsts -> ModuleConsts
classifyMethods (MC poss tids ids [] cons) = MC poss [] ids tids cons

-- both from the same declaration level
merge :: ModuleConsts -> ModuleConsts -> ModuleConsts
merge (MC poss1 tids1 ids1 mids1 cons1) (MC poss2 tids2 ids2 mids2 cons2) = 
  MC (poss1 `Set.union` poss2) (tids1 `Set.union` tids2) 
    (ids1 `Set.union` ids2) (mids1 `Set.union` mids2) (cons1 ++ cons2)

-- Combine this declaration level with a local declaration level
-- The second collection is the local one.
withLocal :: ModuleConsts -> ModuleConsts -> ModuleConsts
withLocal (MC poss1 tids1 ids1 mids1 cons1) (MC poss2 tids2 ids2 [] []) =
  MC (poss1 `Set.union` poss2) tids1 
    (ids1 `Set.union` tids2 `Set.union` ids2) mids1 cons1
withLocal _ _ = 
  error "TraceTrans.withLocal: locally defined data constructors or method"

getModuleConsts :: ModuleConsts 
                -> ([SrcSpan],[Name SrcSpanInfo],[Name SrcSpanInfo]
                   ,[Name SrcSpanInfo],[(Name SrcSpanInfo,[Name SrcSpanInfo])])
getModuleConsts (MC pos tids ids mids cons) =
  (elems pos,elems tids,elems ids,elems mids,cons)

-- ----------------------------------------------------------------------------
-- Transformation of declarations, expressions etc.

-- pre-condition: The environment contains information about all
-- identifiers declared on this level and more global,
-- but not the local scopes inside.
tDecls :: Environment -> Scope -> Tracing -> 
          [Decl SrcSpanInfo] ->
          ([Decl SrcSpanInfo], ModuleConsts)
tDecls env scope tracing decls = 
  foldr combine ([], emptyModuleConsts) 
    (map (tDecl env scope traced) decls)
  where
  combine :: ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts)
  combine (ds1, c1) (ds2, c2) = (ds1 ++ ds2, c1 `merge` c2)


tDecl :: Environment -> Scope -> Tracing ->
         Decl SrcSpanInfo ->
         ([Decl SrcSpanInfo], ModuleConsts)
tDecl env _ _ synDecl@(TypeDecl span declHead ty) =
  (map tTypeSynonym (splitSynonym span declHead ty), emptyModuleConsts)
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
   instDecl : filedSelectorDecls ++ deriveDecls
  ,foldr addConInfo (fieldSelectorConsts `merge` deriveConsts) qualConDecls)
  where
  (deriveDecls, deriveConsts) = 
    tDecls env Global Trusted (derive d)
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
  onlyDecl (ClassDecl l (fmap tContext maybeContext) (declHead) 
     (map tFunDep fundeps) Nothing)
tDecl env _ tracing  -- class with methods
  (ClassDecl l maybeContext declHead fundeps (Just classDecls)) =
  (ClassDecl l (fmap tContext maybeContext) (declHead) 
    (map tFunDep fundeps) (Just classDecls') :
   auxDecls
  ,classifyMethods declsConsts)
  where
  (classDecls', auxDecls, declsConsts) = 
    tClassDecls env tracing classDecls
tDecl env _ tracing -- class instance without methods
  (InstDecl l maybeContext instHead Nothing) =
  onlyDecl (InstDecl l (fmap tContext maybeContext) (tInstHead instHead) 
    Nothing)
tDecl env _ tracing -- class instance with methods
  (InstDecl l maybeContext instHead (instDecls)) =
  ([InstDecl l (fmap tContext maybeContext) (tInstHead instHead) 
     (Just instDecls')] :
   auxDecls
  ,classifyMethods declsConsts)
  where
  (instDecls', auxDecls, declsConsts) =
    tInstDecls env tracing instDecls
tDecl _ _ _ (DeriveDecl l _ _) =
  notSupported l "standalone deriving declaration"
tDecl _ _ _ (InfixDecl l assoc priority ops) =
  onlyDecl (InfixDecl l assoc priority (map nameTransLetVar ops))
tDecl env _ _ (DefaultDecl l tys) =
  ([DefaultDecl l []
   ,WarnPragmaDecl l [([], "Defaulting doesn't work in traced programs. Add type annotations to resolve ambiguities.")]]
  ,emptyModuleDecls)
tDecl _ _ _ (SpliceDecl l _) =
  notSupported l "Template Haskell splicing declaration"
tDecl env _ _ (TypeSig l names ty) =
  -- Type signatures need to be preserved (i.e. transformed),
  -- because polymorphic recursion needs them or more general
  -- types may later lead to ambiguous types
  -- Also shared constants need to be typed, in case they are overloaded,
  -- so that the monomorphic restriction does not lead to type error
  -- (actually then sharing is unfortunately lost)
  (TypeSig l (map nameTransLetVar names) (tFunType ty) :
   concapMap mkWorkerTypeSig nonConstVars ++
   if null constVars then []
     else [TypeSig (map nameTransShare constVars) (tConstType ty)]
  ,emptyModuleConsts)
  where
  (constVars, nonConstVars) = partition isNonMethodConstant vars
  isNonMethodConstant :: Name l -> Bool
  isNonMethodConstant name =
    isLambdaBound name || maybe False (==0) (arity env name)
  mkWorkerTypeSig :: Name SrcSpanInfo -> [Decl SrcSpanInfo]
  mkWorkerTypeSig name =
    case arity env name of
      Just n | n > 0 -> [TypeSig l [nameWorker name] (tWorkerType n ty)]
      _ -> []
tDecl env scope tracing (FunBind l matches) =
  tFunBind env scope tracing l matches  
  -- a function does not use the static parent
tDecl env scope tracing (PatBind l pat maybeTy rhs maybeBinds) =
  tPatBind env scope tracing l pat maybeTy rhs maybeBinds
tDecl env _ _ (ForImp l callConv maybeSafety maybeString name ty) =
  case maybeString >>= stripPrefix "NotHat." of
    Just origName -> tForeignImp l (qName origName) name ty
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
onlyDecl d = ([d], emptyModuleConsts)


-- Process pattern binding:

tPatBind :: Environment -> Scope -> Tracing -> SrcSpanInfo ->
            Pat SrcSpanInfo -> Maybe (Type SrcSpanInfo) -> Rhs SrcSpanInfo ->
            Maybe (Binds SrcSpanInf) ->
            ([Decl SrcSpanInfo], ModuleConsts)
tPatBind env scope tracing l (PVar _ name) maybeType rhs maybeBinds =
  -- simple case
  tCaf env scope tracing l name maybeType rhs maybeBinds
tPatBind env scope tracing l (PAsPat _ name pat) rhs maybeBinds =
  -- can break off simple case
  (cafDecls ++ patDecls, cafConsts `merge` patConsts)
  where
  envLet = mutateLetBound env name
  (cafDecls, cafConsts) = 
    tCaf envLet scope tracing l name maybeType rhs maybeBinds
  (patDecls, patConsts) = tDecl envLet scope tracing
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
   map (projDef patTuple (Var l (UnQual l resultTraceName))) patNames ++
   [PatBind l (PVar l patName) Nothing (UnGuardedRhs l (
     (Case l exp'
       [Alt l pat'' (UnGuardedAlt l tuple) Nothing
       ,Alt (PWildcard l) (UnGuardedAlt l (mkFailExp expParent)) Nothing])))
     Nothing]
  ,foldr addVar (emptyModuleConsts `withLocal` altConsts) patNames)
  where
  firstName = head (patNames)
  patName = nameTraceShared l firstName
  resultTraceName = nameTrace2 firstName
  expTuple = Tuple l (map (Var l . UnQual l) (resultTraceName : patNames'))
  patTuple = PTuple l (map (PVar l) (resultTraceName : patNames'))
  patNames = map (\(PVar _ name) -> name) patVars
  (patVars', Nothing) = tPats patVars
  -- Nothing means that we do not supported numeric patterns (k and n+k)
  patVars = getPatVars pat
  pat'' = case pat' of
            PApp l r [v, _] ->
              PApp l r [v, PVar l resultTraceName]
  (Match _ _ [pat'] (UnGuardedRhs _ exp') maybeBinds', altConsts) =
    tMatch env tracing False failContinuation 
      (Match l firstName [pat] rhs maybeBinds)
  
-- Build the first set of definitions for pattern bindings.
useDef :: Tracing -> Name SrcSpanInfo -> Decl SrcSpanInfo
useDef tracing name =
  FunBind l [Match l (nameTransLetVar name) [PVar l sr, patParent]
               (UnGuardedRhs l (appN l
                  [combConstUse l tracing 
                  ,Var l (UnQual l sr)
                  ,expParent
                  ,Var l (UnQual l (nameShare name))]))
               Nothing]
  where
  l = ann name
  sr = nameSR name 

-- Caf for one variable in a pattern binding
projDef :: Scope -> Tracing -> Pat SrcSpanInfo -> Exp SrcSpanInfo -> 
           Name SrcSpanInfo -> Decl SrcSpanInfo
projDef scope tracing patTuple expVarResultTrace name =
  PatBind l (PVar l (nameShare name)) Nothing 
    (UnGuardedRhs l (appN l 
      [combConstDef l tracing
      ,expParent
      ,Var l (nameTraceInfoVar l scope name)
      ,Lambda l (PWildcard l)
         Case l (Var l patName)
           [Alt l patTuple (UnGuardedAlt l 
             (if isLocal scope && tracing == Trusted
                then varLambdaName
                else appN l
                       [Var l qNameProjection
                       ,mkSRExp l tracing
                       ,Var l resultTraceName
                       ,varLambdaName]))
             Nothing]]
    Nothing
  where
  l = ann name
  varLambdaName = Var l (UnQual l (nameTransLambdaVar name))

-- Extract all variables from a pattern, left-to-right
getPatVars :: Pat l -> [Pat l]
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
  getPatFieldVars :: PatField l -> [Pat l]
  getPatFieldVars (PFieldPat _ _ p) = getPatVars p
  getPatFieldVars (PFieldPun _ _) = []
  getPatFieldVars (PFieldWildcard _) = []
getPatVars (PAsPat _ p) = getPatVars p
getPatVars (PWildCard _) = []
getPatVars (PIrrPat _ p) = getPatVars p
getPatVars (PatTypeSig _ p _) = getPatVars p
getPatVars (PViewPat _ _ p) = getPatVars p
getPatVars (PBangPat _ p) = getPatVars p
getPatVars p = notSupported (ann p) "in pattern binding"


-- Process function binding:

tFunBind :: Environment -> Scope -> Tracing -> SrcSpanInfo -> 
            [Match SrcSpanInfo] ->
            ([Decl SrcSpanInfo], ModuleConsts)
tFunBind env scope tracing l matches =
  (FunBind l [Match l (nameTransLetVar orgName) 
               [PVar l sr, patParent]
               (UnGuardedRhs l 
                 (mkExpFun tracing (Var l (UnQual sr)) 
                   (Var l (nameTraceInfoVar l scope orgName)) 
                   (Var l wrappedId') funArity))
               Nothing]
  :if isLocal scope 
     then (PatBind l (PVar l (nameTraceInfoVar l Global orgName)) Nothing 
            (UnGuardedRhs l (Var l (nameTraceInfoVar l Local orgName))) 
            Nothing :)
     else (\x->x)
   (FunBind l matches' : newDecls')
  -- The known-arity application optimisation needs a nameTraceInfoVar of
  -- the global kind (the name does not include the definition position)
  -- Hence for local definitions we need to define the nameTraceInfoVar
  -- in terms of the "local" nameTraceInfoVar that is defined globally.
  -- In same scope as type decl
  ,addVar l orgName (emptyModuleConsts `withLocal` funConsts))
  where
  orgName = getMatchName (head matches)
  sr = nameSR orgName
  wrappedId' = nameWorker id
  (matches', newDecls', funConsts) =
    tMatches env tracing l (nameFuns orgName)
      (map (Var l) (nameArgs orgName)) (matchArity (head matches)) False 
      (map (changeInfixMatch matches))


-- Transformation of matches is complex because guarded equations may fail
-- and thus 'fall' to next equation.
-- This is simulated here by a sequence of functions, each possibly calling
-- the next.
tMatches :: Environment -> Tracing -> SrcSpanInfo -> 
            Name SrcSpanInfo -> -- names for definitions that clauses become
            Exp SrcSpanInfo -> -- vars for naming arguments that are not vars
            Name SrcSpanInfo -> -- name of this definition
            Arity -> 
            Bool -> -- preceeding match will never fail
            [Match SrcSpanInfo] ->
            ([Match SrcSpanInfo], [Decl SrcSpanInfo], ModuleConsts)
tMatches _ _ _ _ _ _ _ True [] = ([], [], emptyModuleConsts)
tMatches env tracing l ids pVars funName funArity False [] =
  ([Match l funName (replicate funArity (PWildcard l) ++ [patParent])
     (UnGuardedRhs l (continuationToExp failContinuation))
     Nothing]
  ,[]
  ,emptyModuleConsts)
tMatches env tracing l ids pVars funName funArity _
  (m@(Match _ _ pats _ _) : matches) | not (null matches) && matchCanFail m =
  ([Match l funName (pats'' ++ [patParent]) rhs' decls'
   ,Match (vars ++ [patParent]) 
      (UnGuardedRhs (continuationToExp failCont)) Nothing]
  ,FunBind l matches' : matchesDecls
  ,matchConsts `merge` matchesConsts)
  where
  contId = head ids
  failCont = functionContinuation contId vars
  (pats'', vars) = namePats pats' pVars
  (Match _ _ pats' rhs' decls', matchConsts) =
    tMatch env tracing True funName failCont m
  (matches', matchesDecls, matchesConsts) =
    tMatches env tracing l (tail ids) pVars funName funArity 
      (neverFailingPats pats) matches
tMatches env tracing l ides pVars funName funArity _ 
  (m@(Match _ _ pats _ _) : matches) =
  -- last match or this match cannot fail (others are dead code)
  (Match l funName (pats' ++ [patParent]) rhs' decls' : matches
  ,matchesDecls
  ,matchConsts `merge` matchesConsts)
  where
  (Match _ _ pats' rhs' decls', matchConsts) =
    tMatch env tracing True funName failContinuation m
  (matches', matchesDecls, matchesConsts) =
    tMatches env tracing l ids pVars funName funArity 
      (neverFailingPats pats) matches

-- Numeric literals need to be overloaded with respect to the new
-- transformed numeric classes; hence they cannot just be left wrapped
-- in patterns
-- Transform such literals and n+k patterns into conditions in guards.
-- Have to be careful to preserve left-to-right pattern matching,
-- e.g. f 1 True = ... -> f x True | x == 1 = ... is wrong.
-- Assume that ~ has been removed before.
-- Definition similar to tGuardedExps
tMatch :: Environment ->
          Tracing ->
          Bool -> -- whether this is reduct of the parent
          Name l -> -- name for function this match partially defines
          ContExp l -> continuation in case of match failure
          Match l ->
          (Match l, ModuleConsts)
tMatch env tracing cr funName contExp (Match l _ pats rhs maybeDecls) =
  if isNothing numericLitInfos
    then (Match l funName pats' (UnGuardedRhs l rhs') maybeDecls'
         ,declsConsts `withLocal` rhsConsts)
    else (Match l funName pats' (UnGuardedRhs l (appN l (case tracing of
           Traced -> [combGuard l True
                     ,mkSRExp l tracing
                     ,expParent
                     ,cond'
                     ,Lambda l [patParent]
                       (appN l (Var l nameFun : argvars ++ [expParent]))
                     ,Lambda l [patParent]
                       (continuationToExp contExp)]
           Trusted -> [combGuard l False
                      ,cond'
                      ,appN l (Var l nameFun : argvars ++ [expParent])
                      ,continuationToExp contExp])))
           (Just (def:decl'))
         ,l `addSpan` condConsts `merge` declConsts `merge` rhsConsts)
         -- condConsts contains locations of the Boolean guards
         -- declsConsts contains locations of the bound variables
  where
  (pats', numericLitInfos) = tPats env pats
  (rhs', rhsConsts) = tRhs env tracing cr contExp rhs
  (decls', declsConsts) = tDecls Local tracing decls
  Just (cond, bindings, argvars, argpats) = numericLitInfos
  (cond', condConsts) = tExp env tracing False cond
  (decl', declConsts) = tDecls env Local tracing bindings
  def = FunBind l [Match l nameFun (fpats'++patParent) mrhs' mmdecls'
                  ,Match l nameFun 
                     (replicate funArity (PWildcard l) ++ [patParent])
                     (UnGuardedRhs l (continuationToExp contExp))
                     Nothing]
  funArity = length argpats
  (Match _ _ mpats' mrhs' mmdecls', matchConsts) =
    tMatch tracing cr contExp (Match l nameFun argpats rhs maybeDecls)

getMatchName :: Match l -> Name l
getMatchName (Match _ name _ _ _) = name
getMatchName (InfixMatch _ _ name _ _ _) = name

-- Here failure means a failed test that can be observed in the trace,
-- not simply non-matching of data constructors.
matchCanFail :: Match l -> Bool
matchCanFail (Match _ _ pats rhs _) =
  any numericLitIn pats || case rhs of
    UnGuardedRhs _ _ -> False
    GuardedRhs _ gdRhss -> gdRhssCanFail gdRhss

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
  (continuationToExp failCont, emptyModuleConsts)
tGuardedRhss env Traced cr failCont 
  (GuardedRhs _ (Qualifier l guard) exp : gdRhss) =
  (appN l
    [combGuard l True
    ,mkSRExp l True
    ,expParent
    ,guard'
    ,Lambda l [patParent] exp'
    ,Lambda l [patParent] gdRhss']
  ,l `addSpan` guardConsts `merge` expConsts `merge` gdRhssConsts)
  where
  (guard', guardConsts) = tExp env Traced False guard
  (exp', expConsts) = tExp Traced cr exp
  (gdRhss', gdRhssConsts) = 
    tGuardedRhss env Traced cr failCont gdRhss
tGuardedRhss env Trusted cr failCont 
  (GuardedRhs _ (Qualifier l guard) exp : gdRhss) =
  (appN l
    [combGuard l False
    ,guard'
    ,exp'
    ,gdRhss']
  ,guardConsts `merge` expConsts `merge` gdRhssConsts)
  where
  (guard', guardConsts) = tExp env Trusted False guard
  (exp', expConsts) = tExp Trusted cr exp
  (gdRhss', gdRhssConsts) = tGuardedRhss env Trusted cr failCont gdRhss
tGuardedRhss _ _ _ _ _ (GuardedRhs l _ _ : _) =
  notSupported l "statements in pattern guards"
  

-- Process foreign import:

tForeignImp :: SrcSpanInfo -> QName SrcSpanInfo -> Name SrcSpanInfo -> 
               Type SrcSpanInfo -> 
               ([Decls SrcSpanInfo], ModuleConsts)
tForeignImp l foreignName name ty =
  if arity == 0 
    then ([TypeSig l letVarName] (tFunType ty)
          ,FunBind l [Match l letVarName [PVar l sr, patParent]
            (UnGuardedRhs l (appN l 
              [combConstUse l False
              ,Var l (UnQual l sr)
              ,expParent
              ,Var l (Unqual lshareName)]))
            Nothing]
          ,PatBind l (PVar l shareName) Nothing        
            (UnGuardedRhs l (appN l 
              [combConstDef l False
              ,expParent
              ,Var l (nameTraceInfoVar l Global name) 
              ,Lambda l [patParent]
                 (appN l
                   [expFrom l ty, expParent, Var l foreignName])]))
            Nothing
         ,addVar l name emptyModuleConsts)
    else ([TypeSig l letVarName] (tFunType ty)
          ,FunBind l [Match l letVarName [PVar l sr, patParent]
            (UnGuardedRhs l 
              (mkExpFun Trusted (Var l (UnQual l sr))
                (Var l (nameTraceInfoVar l Global name))
                (Var l (UnQual l workerName)) arity))
            Nothing]
          ,FunBind l [Match l workerName (map (PVar l) (args++[hidden]))
            (UnGuardedRhs l (appN l
              [expFrom l tyRes
              ,ExpVar l hidden
              ,appN l (Var l foreignName : zipWith to tyArgs args)]))
            Nothing]
         ,addVar l name emptyModuleConsts)
  where
  sr = nameSR name
  workerName = nameWorker name
  letVarName = nameTransLetVar name
  shareName = nameShare name
  args = take arity (nameArgs name)
  hidden = nameTrace2 name
  to :: Type l -> QName l -> Exp l
  to ty arg = appN l [expTo l ty, Var l hidden, Var l arg]
    where
    l = ann ty
  arity = length tyArgs
  (tyArgs, tyRes) = decomposeFunType ty
  -- pre-condition: no type synonym appearing in type
  decomposeFunType :: Type l -> ([Type l], Type l)
  decomposeFunType (TyFun _ tyL tyR) = (tyL:tyArgs, tyRes)
    where
    (tyArgs, tyRes) = decomposeFunType tyR
  decomposeFunType ty = ([], ty)


-- Process class instances:

tInstHead :: InstHead l -> InstHead l
tInstHead (IHead l qname tys) = IHead l (nameTransCls qname) (map (tType tys))
tInstHead (IHInfix l tyL qname tyR) = 
  IHInfix l (tType tyL) (nameTransCls qname) (tType tyR)
tInstHead (IHParen l instHead) = IHParen l (tInstHead instHead)

tInstDecls :: Environment -> Tracing ->
              [InstDecl SrcSpanInfo] ->
              ([InstDecl SrcSpanInfo], ModuleConsts)
tInstDecls env tracing classDecls =
  (concat instDeclss', foldr merge emptyModuleConsts declsConsts)
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
  (concat classDeclss', foldr merge emptyModuleConsts declsConsts)
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
  ([TypeSig l (tSpanShares names) (tConstType ty), tySig'], moduleConsts)
  where
  ([tySig'], moduleConsts) = tDecl env Local tracing decl
  -- This should cover all declarations that can occur.


addToWhere :: Match l -> [Decl l] -> Match l
addToWhere (Match l name pats rhs Nothing) decls =
  Match l name pats rhs (Just BDecls l decls)
addToWhere (Match l name pats rhs (Just (BDecls l' ds))) decls =
  Match l name pats rhs (Just (BDecls l' (ds ++ decls)))
addToWhere (InfixMatch l pl name pr rhs Nothing) decls =
  InfixMatch l pl name pr rhs (Just BDecls l decls)
addToWhere (InfixMatch l pl name pr rhs maybeBinds) decls =
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
splitSynonym :: Decl SrcSpanInfo -> [Decl SrcSpanInfo]
splitSynonym typeDecl@(TypeDecl span declHead ty) =
  typeDecl : zipWith mkTypeDecl (hrhss ty) [1..]
  where
  mkTypeDecl :: Type SrcSpanInfo -> Int -> Decl SrcSpanInfo
  mkTypeDecl hrhs no =
    TypeDecl span (mapDeclHead (nameTransTySynHelper no) declHead) hrhs
  hrhss ty = case ty of
    (TyParen l ty') -> hrhss ty'

  -- It is vital that this `go' agrees with the `go' in `splitSynonym' in
  -- AuxFile. Sadly the module structure of Hat is such that the two
  -- functions cannot sit next to each other (or be combined) without
  -- introducing a separate module for them.
  go :: Type SrcSpanInfo -> [Type SrcSpanInfo] -> [Type SrcSpanInfo]
  go (TyForall l _ _) tys = notSupported l "forall in type synonym"
  go (TyFun l tyL tyR) [] = tyL : go tyR
  go ty@(TyTuple _ _ _) [] = [ty]
  go ty@(TyList _ _) [] = [ty]
  go (TyApp l tyL tyR) tys = go tyL (tyR:tys)
  go (TyVar _ _) tys = []
  go (TyCon l tyCon) tys = 
    if isExpandableTypeSynonym env tyCon 
      then expandTypeSynonym env tyCon tys go  -- continuation
      else []
  go (TyParen l ty) = map (TyParen l) (go ty)
  go (TyInfix l tyL tyCon tyR) tys =
    if isExpandableTypeSynonym env tyCon
      then expandTypeSynonym env tyCon (tyL:tyR:tys) go
      else []
  go (TyKind l ty kind) tys = notSupported l "kind annotation in type syonym"

mapDeclHead :: (Name l -> Name l) -> DeclHead l -> DeclHead l
mapDeclHead f (DHead l name tyVarBinds) = DHead l (f name) tyVarBinds
mapDeclHead f (DHInfix l tyVarBindL name tyVarBindR) = 
  DHInfix l tyVarBindL (f name) tyVarBindR
mapDeclHead f (DHParen l declHead) = DHParen l (mapDeclHead f declHead)

-- Process data type declarations:

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

tQualConDecl :: QualConDecl SrcSpanInfo -> QualConDecl SrcSpanInfo
tQualConDecl (QualConDecl l maybeTyVarBinds maybeContext condecl) =
  QualConDecl l (fmap (map tTyVarBind) maybeTyVarBinds)
    (fmap tContext maybeContext) (tConDecl condecl)

tConDecl :: ConDecl SrcSpanInfo -> ConDecl SrcSpanInfo
tConDecl (ConDecl l name bangTys) = 
  ConDecl l (nameTransCon name) (map tBangType bangTys)
tConDecl (InfixConDecl l btL name btR) =
  InfixConDecl l (tBangType btL) (nameTransCon name) (tBangType btR)
tConDecl (RecDecl l name fieldDecls) =
  RecDecl l (nameTransCon name) (map fieldDecl fieldDecls)

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
  foldr combine ([], emptyModuleConsts) . map mkFieldSelector . nubBy eqName . 
    concatMap getFieldNamesFromQualConDecl $ qualConDecls
  where
  combine :: ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts)
  combine (decls1, modConsts1) (decls2, modConsts2) = 
    (decls1++decls2, modConsts1 `merge` modConsts2)

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
                     (Var l (UnQual l wrappedName)) 1)
                Nothing]
   ,FunBind l [Match l wrappedName
                [wrapPat (PVar l varName) (PWildcard l), patParent]
                (UnGuardedRhs l (appN l
                   [Var l qNameProjection
                   ,mkExpSR l False
                   ,expParent
                   ,App l (Var l (UnQual l (nameTransField fieldName))) 
                      (Var l (UnQual l varName))]))
                Nothing]]
  ,addVar fieldName emptyModuleConsts)
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
    (IHead l (qNameWrapValClass l) [dataDeclHeadToTy declHead])
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

wrapValMatch :: ConDecl SrcSpanInfo :: Match SrcSpanInfo
wrapValMatch conDecl =
  Match l (nameWrapValFun l) 
    [PVar l (nameSR (nameWrapValFun l))
    ,PAsPat l varName patConsApp
    ,patParent]
    (UnGuardedRhs (wrapExp l var consAppTrace))
    Nothing
  where
  l = ann constr
  consAppTrace =
    if numOfArgs == 0
      then appN l (Var l qNameMkExpValueUse : expParent :
                   map (Var l . UnQual l) [srName, funAtom])
      else appN l (Var l (qNameMkExpValueApp numOfArgs) : expParent :
                   map (Var l . UnQual l) (srName : funAtom : traces))
  funAtom = nameTraceInfoCon consName
  patConsApp =
    PApp l consName (map (wrapPat l (PWildcard l) . PVar l) traces)
  consName = getConstructorFromConDecl conDecl
  traces = take numOfArgs (nameArgs (nameWrapValFun l)) :: Name SrcSpanInfo
  numOfArgs = getArityFromConDecl conDecl
  varName = nameTrace2 (nameWrapValFun l)
  
-- ---------------------------------------------------------------------------- -- Transform expressions


-- Boolean argument True iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
tExp :: Environment -> Tracing -> Bool -> Exp SrcSpanInfo ->
        (Exp SrcSpanInfo, ModuleConsts)
tExp env tracing cr exp = tExpA env tracing cr exp []

-- Order of both lists is innermost first.
tExpA :: Environment -> Tracing -> Bool -> Exp SrcSpanInfo -> 
         [SrcSpanInfo] ->  -- locations of applications of arguments in list
         [Exp SrcSpanInfo] ->  -- arguments from surrounding applications
         (Exp SrcSpanInfo, ModuleConsts)
tExpA env tracing cr (Var l qName) ls es 
  | Just a <- arity env qName, a > 0, a <= length es, a <= 5 =
  -- known arity optimisation that calls worker directly
  tExpF tracing (drop a ls) (drop a es) $
    (if isTraced tracing || isTracedQName env qName
       then mkExpApplyArity tracing (mkExpSR lApp tracing) (mkExpSR l tracing)
              a (expTraceInfoVar Global qName) (expWorker qName) es'
       else mkExpUWrapForward (appN (expWorker qName : es' ++ [expParent]))
    ,l `addSpan` (lApp `addSpan` esConsts))
  where
  (es', esConsts) = tExps env tracing es
  lApp = ls!!a-1
tExpA env tracing cr (Var l qName) ls es =
  tExpF tracing ls es $
    if isLambdaBound env qName
      then 
        let e' = Var l (nameTransLambdaVar qName)
        in if cr
             then (mkExpProjection sr e', l `addSpan` emptyModuleConsts)
             else (e', emptyModuleConsts)
      else
        (appN l [Var l (nameTransLetVar qName), sr, expParent]
        ,l `addSpan` emptyModuleConsts)
  where
  sr = mkExpSR l tracing
tExpA _ _ _ (IPVar l _) _ _ =
  notSupported l "implicit parameter variable"
tExpA env tracing cr (Con l qName) ls es =
  tConApp env tracing qName ls es
tExpA env tracing cr (Lit l literal) [] [] =  -- no arguments possible
  (tLiteral env tracing literal, ann literal `addSpan` emptyModuleConsts)
tExpA env tracing cr (InfixApp l e1 qop e2) ls es =
  tExpA env tracing cr 
    (App l (App (ann e1 <++> ann qop) (qOp2Exp qop) e1) e2) ls es
tExpA env tracing cr (NegApp l e) ls es =
  tExpA env tracing cr (App l (Var l (qNamePreludeSymbol "-" l)) e) ls es
tExpA env tracing cr (Lambda l pats exp) ls es =
  tExpF tracing ls es
    (mkExpFun tracing sr expMkAtomLambda fun funArity
    ,l `addSpan` bodyConsts)
  where
  fun = if neverFailingPats pats
          then Lambda l (pats' ++ [patParent]) body'
          else Lambda l (map (PVar l) nameVars ++ [patParent])
                 (Case l (mkExpTuple (map (Var l . UnQual l) nameVars))
                    [Alt l (mkPatTuple pats') (UnGuardedAlt l body') Nothing
                    ,Alt l (PWildcard l) (UnGuardedAlt l expFail) Nothing]
                 )
  (Match _ _ pats' (UnGuardedRhs _ body') _, bodyConsts) =
    tMatch tracing True failContinuation 
      (Match l undefined (UnGuardedRhs l body) Nothing)
  nameVars = take funArity (namesFromSpan l)
  funArity = length pats
tExpA env tracing cr (Let l binds body) ls es =
  tExpF tracing ls es
    (Let l binds' exp', bindsConsts `withLocal` bodyConsts)
  where
  (binds', bindsConsts) = tBinds env tracing binds
  (body', bodyConsts) = tExp env tracing cr body
tExpA env Traced cr (If l cond e1 e2) ls es =
  tExpF tracing ls es
    (mkExpIf l Tracing cond' 
       (Lambda l [patParent] e1') (Lambda l [patParent] e2')
    ,l `addSpan` condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond', condConsts) = tExp env Tracing False cond
  (e1', e1Consts) = tExp env Tracing True e1
  (e2', e2Consts) = tExp env Tracing True e2
tExpA env Trusted cr (If l cond e1 e2) ls es =
  tExpF tracing ls es
    (mkExpIf l Trusted cond' e1' e2'
    ,condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond', condConsts) = tExp env Trusted False cond
  (e1', e1Consts) = tExp env Trusted True e1
  (e2', e2Consts) = tExp env Trusted True e2
tExpA env tracing cr (Case l exp alts) ls es =
  -- translate case into if:
  -- case exp of
       pat1 -> exp1
       pat2 -> exp2
       ...
  -- ==>
     (let f pat1 = exp1; f pat2 = exp2;... in f) exp
  -- but not fully, as we don't want to trace function f and the application
  tExpF tracing ls es
    (mkExpCase l tracing 
       (Let l (BDecls l (Fun l matches' : decls))
          (Var l (UnQual funName))) exp'
    ,l `addSpan` expConsts `merge` funConsts)
  where
  (funName : argName : funsNames) = namesFromSpan l
  (exp', expConsts) = tExp env tracing False exp
  (matches', decls', funConsts) =
    tMatches env tracing l funsNames [ExpVar l (UnQual l argName)] 
      funName 1 True (map alt2Match alts)
tExpA env tracing cr (Do l stmts) ls es =
  tExpF tracing ls es (tExpA env tracing cr (removeDo stmts))
tExpA env tracing cr (MDo l _) ls es =
  notSupported l "mdo-expression"
tExpA env tracing cr (Tuple l exps) [] [] =
  tConApp env tracing (Special l (TupleCon l True arity)) 
    (replicate arity l) exps
tExpA env tracing cr (TupleSection l maybeExps) ls es =
  -- desugar tuple section into lambda
  tExpA env tracing cr (desugarTupleSection l maybeExps) ls es
tExpA env tracing cr (List l exps) [] [] =
  -- use special combinator that transforms list at runtime;
  -- desugaring and subsequent transformation would lead to large program.
  (appN [expFromExpList, mkExpSR l tracing, expParent, List l exps']
  ,l `addSpan` expsConsts)
  where
  (exps', expsConsts) = tExps env tracing exps
tExpA env tracing cr (Paren l exp) ls es =
  tExpA env tracing cr exp ls es
tExpA env tracing cr (LeftSection l exp qOp) ls es =
  -- desugar into normal function application
  tExpA env tracing cr (App l (qOp2Exp qOp) exp) ls es
tExpA env tracing cr (RightSection l exp qOp) ls es
  -- desugar into a lambda abstraction
  tExpA env tracing cr
    (Lambda noSpan [EVar noSpan name] 
      (App l (App l (qOp2Exp qOp) (Var noSpan (UnQual noSpan name))) exp)) 
    ls es
  where
  name : _ = namesFromSpan l
tExpA env tracing cr (RecConstr l qName fieldUpdates) ls es
  tExpF tracing ls es $
    (appN [expWrapValFun, mkExpSR l tracing, 
           RecUpdate l consUndefined fieldUpdates', expParent]
    ,l `addSpan` fieldsConsts)
  where
  consUndefined =
    if consArity == 0 
      then Con l (nameTransCon qName)
      else appN (Con l (nameTransCon qName) : replicate consArity
                   (appN [expUndefined, expSR, expParent]))
  Just consArity = arity env qName
  expSR = mkExpSR l False
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env tracing) fieldUpdates
tExpA env Traced cr (RecUpdate l exp fieldUpdates) ls es
  tExpF Traced ls es $
    (Let l (BDecls l fieldVarDecls) 
       (appN (mkExpUpdate Traced (length labels)
             :mkExpSR l Traced
             :expParent
             :exp'
             :Lambda l [PVar l nameVar] 
               (RecUpdate l (Var l (UnQual l nameVar)) varFields')
             :labels ++ fieldVars))
    ,l `addSpan` expConsts `merge` fieldsConsts)
  where
  (exp', expConsts) = tExp env Traced False exp
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env Tracing) fieldUpdates
  labels = map (Var l . nameTraceInfoVar l Global) labelIds
  varFields' = zipWith (FieldUpdate l) (map fieldLabel fieldUpdates') fieldVars
  fieldExps' = map fieldExp fields'
  fieldVars = map (Var l) fieldVarQNames
  fieldVarQNames = map nameShare labelQNames
  labelQNames = map fieldLabel fieldUpdates
  nameVar = nameFromSpan l
  fieldLabel (FieldUpdate _ qNameLabel _) = qNameLabel
  fieldLabel (FieldPun l) = notSupported l "record field pun"
  fieldLabel (FieldWildcard l) = notSupported l "record field wildcard"
  fieldExp (FieldUpdate _ _ exp) = exp
  fieldExp (FieldPun l) = notSupported l "record field pun"
  fieldExp (FieldWildcard l) = notSupported l "record field wildcard"
tExp env Trusted cr (RecUpdate l exp fieldUpdates) ls es
  tExpF Trusted ls es $
    (appN [mkExpUpdate Trusted (-1) 
          ,expParent
          ,exp'
          ,Lambda l [PVar l nameVar] 
            (RecUpdate l (Var l (UnQual l nameVar)) fieldUpdates')]
    ,expConsts `merge` fieldsConsts)
  where
  (exp', expConsts) = tExp env Trusted False exp
  (fieldUpdates', fieldsConsts) = mapMerge2 (tField env Trusted) fieldUpdates
  nameVar = nameFromSpan l
tExp env tracing cr (EnumFrom l exp) ls es =
  -- desugar list comprehension [from ..]
  tExp env tracing cr (App l (mkExpPreludeEnumFrom l) exp) ls es
tExp env tracing cr (EnumFromTo l from to) ls es =
  tExp env tracing cr (App l (App l (mkExpPreludeEnumFromTo l) from) to) ls es
tExp env tracing cr (EnumFromThen l from the) ls es =
  tExp env tracing cr 
    (App l (App l (mkExpPreludeEnumFromThen l) from) the) ls es
tExp env tracing cr (EnumFromThenTo l from the to) ls es =
  tExp env tracing cr
    (App l (App l (App l (mkExpPreludeEnumFromThenTo l) from) the) to) ls es

-- At end of transforming expressions possibly add deferred applications.
-- Lists are ordered from innermost to outermost.
tExpF :: Tracing ->
         [SrcSpanInfo] ->  -- locations of surrounding applications
         [Exp SrcSpanInfo] ->  -- transformed arguments of surrounding apps
         (Exp SrcSpanInfo, ModuleConsts) -> 
         (Exp SrcSpanInfo, ModuleConsts)
tExpF _ [] [] result = result
tExpF tracing ls es (e, consts) =
  (mkExpApply tracing (mkExpSR l tracing) (e : es), l `addSpan` appConsts)
  where
  l = last ls

-- Transform a list of expressions.
-- Mainly have to merge all constants.
tExps :: Environment -> Tracing -> [Exp SrcSpanInfo] ->
         ([Exp SrcSpanInfo], ModuleConsts)
tExps env tracing = mapMerge2 (tExp tracing False)

mapMerge2 :: (a -> (b, ModuleConsts)) -> [a] -> ([b], ModuleConsts)
mapMerge2 f = mapSnd (foldr merge emptyModuleConsts) . unzip . map f

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x, f y)

-- Transform data constructor application.
-- Number of arguments may be smaller than arity of the data constructor.
-- (but never bigger.)
tConApp :: Environment -> Tracing -> QName SrcSpanInfo -> 
           [SrcSpanInfo] -> [Exp SrcSpanInfo] -> 
           (Exp SrcSpanInfo, ModuleConsts)
tConApp env tracing qName ls es =
  (if conArity > numberOfArgs  -- undersaturated application
     then mkExpPartial sr conArity numberOfArgs qName es
   else if conArity == numberOfArgs -- saturated application
     then mkExpCon sr conArity qName es
   else
     error "TraceTrans.tConApp: data constructor with too many arguments."
  ,lApp `addSpan` emptyModuleConsts)
  where
  Just conArity = arity env qName  -- a constructor always has an arity
  numberOfArgs = length es
  lApp = ls !! conArity-1
  sr = mkExpSR lApp tracing

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
    ,appN [expR
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
neverFailingPat (PTuple _ ps) = all neverFailingPat p
neverFailingPat (PParen _ p) = neverFailingPat p
neverFailingPat (PAsPat _ _ p) = neverFailingPat p
neverFailingPat (PWildcard _) = True
neverFailingPat (PIrrPat _ _) = True
neverFailingPat (PatTypeSig _ p _) = neverFailingPat p
neverFailingPat (PBangPat _ p) = neverFailingPat p
neverFailingPat _ = False

tBinds :: Environment -> Tracing -> Binds SrcSpanInfo ->
          (Binds SrcSpanInfo, ModuleConsts)
tBinds env tracing (BDecls l decls) = tDecls env Local tracing decls
tBinds env tracing (IPBinds l _) =
  notSupported l "binding group for implicit parameters"

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
  appN [Var l (qNamePreludeGtGt l), e, removeDo stmts]
removeDo (LetStmt l binds : stmts) =
  Let l binds (removeDo stmts)
removeDo (Generator l pat e : stmts) =
  appN [Var l (qNamePreludeGtGtEq l)
       ,e
       ,SCCPragma l "" $  -- hack to inform this really from a do-stmt
        if neverFailingPat pat
          then Lambda l [pat] (removeDo stmts)
          else Lambda l [PVar l newName]
                 (Case l (Var l (UnQual l newName))
                   [Alt l pat (UnGuardedAlt l (removeDo stmts)) Nothing
                   ,Alt l (PWildcard l)
                     (UnGuardedAlt l 
                       (App l (Var l (qNamePreludeFail l)) 
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

continuationToExp :: ContExp -> Exp l
continuationToExp Fail = mkFailExp expParent
continuationToExp (Function fun args) =
  appN l (Var l fun : args ++ [expParent])

-- ----------------------------------------------------------------------------
-- Transform types

-- ty ==> R [[ty]]
tConstType :: Type l -> Type l
tConstType ty = wrapType (tType ty)

-- ty ==> RefSrcPos -> Trace -> R [[ty]]
tFunType :: Type l -> Type l
tFunType ty = TyCon l qNameRefSrcPos `typeFun` TyCon l qNameRefExp `typeFun` 
  wrapType (tType ty)
  where 
  l = ann ty

-- Build type of worker from original type
tWorkerType :: Environment -> Arity -> Type l -> Type l
tWorkerType _ 0 ty
  TyCon (ann ty) qNameRefExp `typeFun` wrapType (tType ty)
tWorkerType env a ty = tWorkerSynExpand env a ty []

-- Expand a type synonym and then apply worker type transformation.
-- Need to collect type arguments.
tWorkerSynExpand :: Environment -> Arity -> Type l -> [Type l] -> 
                    Type l
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
tWorkerSynExpand _ _ _ _ = 
  error "TraceTrans.tWorkerSynExpand: type must be a function but is not"

-- Expand only to expose function type constructors.
-- Uses the helper type synonyms introduced by the transformation.
expandTypeSynonym :: Environment -> QName l -> [Type l] -> Type l
expandTypeSynonym env tySyn tys =
  case typeSynonymBody env tySyn of
    Nothing -> error ("TraceTrans.expandTypeSynonym: " ++ show tySyn ++
                      " is not a type synonym.")
    Just body = fst (go body 1)
  where
  l = ann tySyn
  go :: TySynBody -> Int -> (Type l, Int)
  go THelper n = tyAppN (TyCon l (nameTransTySynHelper tySyn n) : tys)
  go (TVar v) n = (tys!!v, n)
  go TFun n = (TyCon l (Special l (FunCon l)), n)
  go (TApp tyL tyR) n = (TyApp l ty1' ty2', n2)
    where
    (ty1', n1) = go ty1 n
    (ty2', n2) = go ty2 n1

-- Just rewrite built-in type constructors, especially function type.
-- (Otherwise type names stay unchanged, they will refer to different modules.)
-- t1 -> t2 ==> Fun [[t1]] [[t2]]
tType :: Type l -> Type l
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
wrapType :: Type l -> Type l
wrapType ty = TyApp l (TyCon l (qNameR l)) ty
  where
  l = ann ty

-- Class contexts remain unchanged
tContext :: Context SrcSpanInfo -> Context SrcSpanInfo
tContext cx = cx


-- ----------------------------------------------------------------------------
-- Error for non-supported language features

notSupported :: SrcSpanInfo -> String -> a
notSupported span construct = 
  "hat-trans: unsupported language construct \"" ++ construct ++ "\" at " ++ 
    show span


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

nameTraceInfoModule :: ModuleName l => Name l
nameTraceInfoModule (ModuleName l ident) = Ident l ('t' : ident)

nameTraceInfoVar :: Id i => SrcSpanInfo -> Scope -> i -> i
nameTraceInfoVar span Global = prefixName 'a' '+'
nameTraceInfoVar span Local = prefixSpanName 'a' '+' span

nameTraceInfoGlobalVar :: Id i => i -> i
nameTraceInfoGlobalVar = prefixName 'a' '+'

nameTraceInfoCon :: Id i => i -> i
nameTraceInfoCon = prefixName 'a' '+'

nameTraceInfoSpan :: SrcSpanInfo -> Name SrcSpanInfo
nameTraceInfoSpan span = Ident span ('p' : showsEncodePos span "")

-- names referring to transformed program fragments:

nameTransModule :: ModuleName l -> ModuleName l
nameTransModule (ModuleName l name) = ModuleName l 
  (fromMaybe (if name == "Main" then name else "Hat." ++ name) 
    (stripPrefix "NotHat." name)) 

-- The unqualfied names in the namespace of classes, types and type synonyms 
-- are left unchanged, but the module changes.
-- Names of some built-in type constructors (e.g ->, [], (,)) are changed!
-- Similarly type variable names are left unchanged.

nameTransCls :: Id i => i -> i
nameTransCls = updateId id  -- unchanged

nameTransTy :: Id i => i -> i
nameTransTy = updateId id  -- unchanged

nameTransSyn :: Id i => i -> i
nameTransSyn = updateId id  -- unchanged

-- Names of helper synonyms are a bit of a hack; a name conflict is possible.
-- We just do not want to prefix all names in the namespace.
nameTransSynHelper :: Id i => i -> Int -> i
nameTransSynHelper syn no = updateToken (++ ("___" ++ show no)) syn
  where 
  update (Ident l name) = Ident l (name ++ "___" ++ show no)
  update (Symbol _ _) = 
    error "TraceTrans, nameTransSynHelper: synom name is a symbol"

nameTransTyVar :: Name l -> Name l
nameTransTyVar = id  -- unchanged

nameTransCon :: Id i => i -> i
nameTransCon = updateId id  -- unchanged

nameTransField :: Id i => i -> i
nameTransField = prefixName 'b' '^'

nameTransLetVar :: Id i => i -> i
nameTransLetVar = prefixName 'g' '!'

nameTransLambdaVar :: Id i => i -> i
nameTransLambdaVar = prefixName 'f' '&'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: Id i => i -> i
nameWorker = prefixName 'h' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: Id i => i -> i
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: Id i => i -> i
nameShare = prefixName 's' '|'

-- variable for a trace including span
nameTraceShared :: Id i => SrcSpanInfo -> i -> i
nameTraceShared = prefixSpanName 'j' '$'

-- variable for parent
nameParent :: Name SrcSpanInfo
nameParent = Ident noSpan "p"

-- variable for a trace
nameTrace :: Id i => i -> i
nameTrace = prefixName 'j' '$'

-- second variable for a trace
nameTrace2 :: Id i => i -> i
nameTrace2 = prefixName 'k' '@'

-- name for a local variable for a source reference
nameSR :: Id i => i -> i
nameSR = prefixName 'p' '%'

-- intermediate function
nameFun :: Name SrcSpanInfo
nameFun = Ident noSpan "h"

-- infinite list of var ids made from one id (for function clauses)
nameFuns :: Id i => i -> [i]
nameFuns = prefixNames 'y' '>'

-- infinite list of var ids made from one id (for naming arguments)
nameArgs :: Id i => i -> [i]
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
  beginRow = srcSpanStartLine srcSpan
  beginColumn = srcSpanStartColumn srcSpan
  endRow = srcSpanEndLine srcSpan
  endColumn = srcSpanEndColumn srcSpan
  srcSpan = srcInfoSpan span

showsSymEncodeSpan :: SrcSpanInfo -> ShowS
showsSymEncodeSpan span = 
  \xs -> numToSym (show beginRow) ++ '=' : numToSym (show beginColumn) ++ '=' 
    : numToSym (show endRow) ++ '=' : numToSym (show endColumn) ++ xs 
  where
  beginRow = srcSpanStartLine srcSpan
  beginColumn = srcSpanStartColumn srcSpan
  endRow = srcSpanEndLine srcSpan
  endColumn = srcSpanEndColumn srcSpan
  srcSpan = srcInfoSpan span

prefixName :: Id i => Char -> Char -> i -> i
prefixName c d = updateId update
  where
  update (Ident l name) = Ident l (c:name)
  update (Symbol l name) = Ident l (d:name)

-- really used with that general type?
prefixModName :: Id i => Char -> i -> i
prefixModName c = updateToken update
  where
  update (Ident l name) = Ident l (c: map (\c->if c=='.' then '_' else c) name)

prefixSpanName :: Id i => Char -> Char -> SrcSpanInfo -> i -> i
prefixSpanName c d span = updateId update
  where
  update (Ident l name) = Ident l (c : showsEncodeSpan span name)
  update (Symbol l name) = Symbol l (d : showsSymEncodeSpan span name)

prefixNames :: Id i => Char -> Char -> i -> [i]
prefixNames c d name = map (($ name) . updateId . update) [1..]
  where
  update no (Ident l name) = Ident l (c : show no ++ name)
  update no (Symbol l name) = Symbol l (d : numToSym (show no) ++ name)

numToSym :: String -> String
numToSym = map (("!#$%&*+^@>" !!) . digitToInt)

-- Actual identifier modification

class Id a where
  -- apply function to unqualified name part 
  -- and prefix module name (if qualified)
  updateId :: (Name l -> Name l) -> a -> a
  -- whether a symbol (operator) or a normal identifier
  isSymbol :: a -> Bool
  getId :: a -> String

instance Id (QName SrcSpanInfo) where
  updateId f (Qual l moduleName name) = 
    Qual l (tModuleName moduleName) (updateId f name)
  updateId f (UnQual l name) = UnQual l (updateId f name)
  updateId f (Special l specialCon) =
    case specialCon of
      UnitCon l' -> newName "Tuple0"
      ListCon l' -> newName "List"
      FunCon l' -> newName "Fun"
      TupleCon l' Boxed arity -> newName ("Tuple" ++ show arity)
      TupleCon l' Unboxed _ -> notSupported l "unboxed tuple"
      Cons l' -> newName "List"
      UnboxedSingleCon l' -> 
        notSupported l "unboxed singleton tuple constructor"
    where
    newName :: String -> QName l
    newName id = Qual l tracingModuleNameShort (Ident l id) 
  isSymbol (Qual _ _ name) = isSymbol name
  isSymbol (UnQual _ name) = isSymbol name
  isSymbol (Special _ _) = True
  getId (Qual _ _ name) = getId name
  getId (UnQual _ name) = getId name

instance Id (Name l) where
  updateId f name = f name  
  isSymbol (Identifier _ _) = False
  isSymbol (Symbol _ _) = True
  getId (Identifier _ ident) = ident
  getId (Symbol _ ident) = ident

instance Id (QOp l) where
  updateId f (QVarOp l qname) = QVarOp l (updateId f qname)
  updateId f (QConOp l qname) = QConOp l (updateId f qname)
  isSymbol (QVarOp _ _) = False
  isSymbol (QConOp _ _) = True
  getId (QVarOp _ qname) = getId qname
  getId (QConOp _ qname) = getId qname

instance Id (Op l) where
  updateId f (VarOp l name) = VarOp l (updateId f name)
  updateId f (ConOp l name) = ConOp l (updateId f name)
  isSymbol (VarOp _ _) = False
  isSymbol (ConOp _ _) = True
  getId (VarOp _ name) = getId name
  getId (ConOp _ name) = getId name

instance Id (CName l) where
  updateId f (VarName l name) = VarName l (updateId f name)
  updateId f (ConName l name) = ConName l (updateId f name)
  isSymbol (VarName _ _) = False
  isSymbol (ConName _ _) = True
  getId (VarOp _ name) = getId name
  getId (ConOp _ name) = getId name

-- ----------------------
-- Hardwired identifiers

expRoot :: Exp SrcSpanInfo
expRoot = expShortIdent "mkRoot"

mkExpSR :: SrcSpanInfo -> Tracing -> Exp SrcSpanInfo
mkExpSR l tracing = 
  Var noSpan (if isTraced tracing then qNameTraceInfoSpan l else qNameMkNoSpan)

mkExpProjection :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo
mkExpProjection sr e =
  appN [expProjection, expParent, e]

mkExpApplyArity :: Tracing ->
                   Exp SrcSpanInfo -> -- location of application
                   Exp SrcSpanInfo -> -- location of applied function
                   Arity -> 
                   Exp SrcSpanInfo -> 
                   Exp SrcSpanInfo -> 
                   [Exp SrcSpanInfo] ->
                   Exp SrcSpanInfo
mkExpApplyArity tracing srA srF a info fun args =
  appN (Var noSpan ((if isTraced tracing then qNameApp else qNameUApp) a)
       :srA
       :srF
       :expParent
       :info
       :fun
       :args)

mkExpApply :: Tracing -> Exp SrcSpanInfo -> [Exp SrcSpanInfo] ->
              Exp SrcSpanInfo
mkExpApply tracing sr es =
  appN (Var noSpan ((if isTraced tracing then qNameAp else qNameUAp) a)
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
  appN (Var noSpan (qNamePa numberOfArgs) 
       :Con noSpan (nameTransCon qName)
       :Var noSpan (qNameCn (conArity-numberOfArgs))
       :sr
       :expParent
       :Var noSpan (nameTraceInfoCon qName)
       :es)

mkExpCon :: Exp SrcSpanInfo ->  -- location of constructor application
            Arity ->
            QName SrcSpanInfo -> [Exp SrcSpanInfo] -> 
            Exp SrcSpanInfo
mkExpCon sr arity qName es =
  appN (Var noSpan (qNameCon arity)
       ,sr
       ,expParent
       ,Con noSpan (nameTransCon qName)
       ,Var noSpan (nameTraceInfoCon qName)
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
  appN [expConInteger, sr, expParent, Lit noSpan lit]

mkExpFun :: Tracing -> Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo ->
            Arity
            Exp SrcSpanInfo
mkExpFun tracing sr funName fun a =
  appN [Var noSpan ((if tracing then qNameFun else qNameUFun) noSpan a)
       ,funName
       ,sr
       ,expParent
       ,fun]

mkExpIf :: SrcSpanInfo -> Tracing -> 
           Exp SrcSpanInfo -> Exp SrcSpanInfo -> Exp SrcSpanInfo -> 
           Exp SrcSpanInfo
mkExpIf l tracing cond e1 e2 =
  appN [Var noSpan ((if isTraced tracing then qNameIf else qNameUIf) noSpan)
       ,mkExpSR l Tracing
       ,expParent
       ,cond
       ,e1
       ,e2]

mkExpCase :: SrcSpanInfo -> Tracing -> Exp SrcSpanInfo -> Exp SrcSpanInfo ->
             Exp SrcSpanInfo
mkExpCase l tracing caseFun arg =
  appN [Var noSpan ((if isTraced tracing then qNameCase else qNameUCase) noSpan)
       ,mkExpSR l tracing
       ,expParent
       ,caseFun
       ,arg]

expShortIdent :: String -> Exp SrcSpanInfo
expShortIdent ident = Var noSpan (qNameShortIdent ident)

-- -----------------------------------------------------------------------------

tracingModuleNameShort :: ModuleName SrcSpanInfo
tracingModuleNameShort = ModuleName noSpan "T"

mkTypeToken :: l -> String -> QName l 
mkTypeToken l id = 
  if id `elem` (map ("from"++) preIds ++ map ("to"++) preIds)
    then Qual l tracingModuleNameShort (Ident l id)
    else UnQual l (Ident l id)
  where
  -- list should include all types allowed in foreign imports
  preIds = ["Id", "IO", "Tuple0", "Tuple2", "Char", "Int", "Integer", 
            "Float", "Double"]

-- names for trace constructors

qNameMkRoot :: l -> QName l
qNameMkRoot = qNameShortIdent "mkRoot"

qNameR :: l -> QName l
qNameR = qNameShortIdent "R"

qNameMkModule :: l -> QName l
qNameMkModule = qNameShortIdent "mkModule"

qNameMkAtomConstructor :: l -> Bool -> QName l
qNameMkAtomConstructor l withFields =
  qNameShortIdent  
    (if withFields then "mkConstructorWFields" else "mkConstructor") l

qNameMkAtomVariable :: l -> QName l
qNameMkAtomVariable = qNameShortIdent "mkVariable"

qNameMkSpan :: l -> QName l
qNameMkSpan = qNameShortIdent "mkSrcPos"

qNameMkNoSpan :: L -> QName l
qNameMkNoSpan = qNameShortIdent "mkNoSrcPos"

qNameMkAtomVariable :: l -> QName l
qNameMkAtomVariable = qNameShortIdent "mkVariable"

qNameMkExpValueApp :: l -> Arity -> QName l
qNameMkExpValueApp = qNameShortArity "mkValueApp"

qNameMkExpValueUse :: l -> QName l
qNameMkExpValueUse = qNameShortIdent "mkValueUse"

qNameMkAtomRational :: l -> QName l
qNameMkAtomRationl = qNameShortIdent "mkAtomRational"
expMkAtomRational :: Exp SrcSpanInfo
expMkAtomRational = Var noSpan (qNameMkAtomRational noSpan)

qNameMkAtomLambda :: l -> QName l
qNameMkAtomLambda = qNameShortIdent "mkLambda"

qNameMkAtomDoLambda :: l -> QName l
qNameMkAtomDoLambda = qNameShortIdent "mkDoLambda"

-- tokens for expression combinators

qNameAp :: l -> Arity -> QName l
qNameAp = qNameShortArity "ap"
qNameUAp :: l -> Arity -> QName l
qNameUAp = qNameShortArity "uap"

qNameApp :: l -> Arity -> QName l
qNameApp = qNameShortArity "app"
qNameUApp :: l -> Arity -> QName l
qNameUApp = qNameShortArity "uapp"

qNameFun :: l -> Arity -> QName l
qNameFun = qNameShortArity "fun"
qNameUFun :: l -> Arity -> Qname l
qNameUFun = qNameShortArity "ufun"

qNameCon :: l -> Arity -> QName l
qNameCon = qNameShortArity "con"

qNamePa :: l -> Arity -> QName l
qNamePa = qNameShortArity "pa"

qNameCn :: l -> Arity -> QName l
qNameCn = qNameShortArity "cn"

qNameConstUse :: l -> QName l
qNameConstUse = qNameShortIdent "constUse"
qNameUConstUse :: l -> QName l
qNameUConstUse = qNameShortIdent "uconstUse"

qNameConstDef :: l -> QName l
qNameConstDef = qNameShortIdent "constDef"
qNameUConstDef :: l -> QName l
qNameUConstDef = qNameShortIdent "uconstDef"

qNameGuard :: l -> QName l
qNameGuard = qNameShortIdent "cguard"
qNameUGuard :: l -> QName l
qNameUGuard = qNameShortIdent "ucguard"

qNameIf :: l -> QName l
qNameIf = qNameShortIdent "cif"
qNameUIf :: l -> QName l
qNameUIf = qNameShortIdent "ucif"

qNameCase :: l -> QName l
qNameCase = qNameShortCase "ccase"
qNameUCase :: l -> QName l
qNameUCase = qNameShortCase "uccase"

qNameUpdate :: l -> Arity -> QName l
qNameUpdate = qNameShortArity "update"
qNameUUpdate :: l -> Arity -> QName l
qNameUUpdate = qNameShortArity "uupdate"

mkExpUpdate :: Tracing -> Arity -> Exp SrcSpanInfo
mkExpUpdate tracing arity =
  Var noSpan 
    (if isTraced tracing then qNameUpdate noSpan arity else qNameUUpdate)

qNameProjection :: l -> QName l
qNameProjection = qNameShortIdent "projection"
expProjection :: Exp SrcSpanInfo
expProjection = Var noSpan (qNameProjection noSpan)

qNameConChar :: l -> QName l
qNameConChar = qNameShortIdent "conChar"
expConChar :: Exp SrcSpanInfo
expConChar = Var noSpan (qNameConChar noSpan)

qNameConInteger :: l -> QName l
qNameConInteger = qNameShortIdent "conInteger"
expConInteger :: Exp SrcSpanInfo
expConInteger = Var noSpan (qNameConInteger noSpan)

qNameFromLitString :: l -> QName l
qNameFromLitString = qNameShortIdent "fromLitString"
expFromLitString :: Exp SrcSpanInfo
expFromLitString = Var noSpan (qNameFromLitString noSpan)

qNameFromExpList :: l -> QName l
qNameFromExpList = qNameShortIdent "fromExpList"
expFromExpList :: Exp SrcSpanInfo
expFromExpList = Var noSpan (qNameFromExpList noSpan)

qNameWrapValClass :: l -> QName l
qNameWrapValClass = qNameShortIdent "WrapVal"

nameWrapValFun :: l -> Name l
nameWrapValFun l = Ident l "wrapVal"

expWrapValFun :: Exp SrcSpanInfo
expWrapValFun = Var noSpan (Var noSpan (UnQual noSpan (nameWrapValFun noSpan)))

qNameUWrapForward :: l -> QName l
qNameUWrapForward = qNameShortIdent "uwrapForward"

-- names from the Prelude:

expUndefined :: Exp SrcSpanInfo
expUndefined = Var noSpan (qNameHatPreludeIdent "gundefined" noSpan)

-- for integer literals
expFromInteger :: Exp SrcSpanInfo
expFromInteger = Var noSpan (qNameHatPreludeIdent "gfromInteger" noSpan)

-- for rational literals:
expConRational :: Exp SrcSpanInfo
expConRational = Var noSpan (qNameHatPreludeSymbol ":%" noSpan)

expFromRational :: Exp SrcSpanInfo
expFromRational = Var noSpan (qNameHatPreludeIdent "gfromRational" noSpan)

-- function for pattern-match failure error message
qNameFatal :: l -> QName l
qNameFatal = qNameShortIdent "fatal"


qNamePreludeTrue :: l -> QName l
qNamePreludeTrue = qNamePreludeIdent "True"

qNamePreludeFalse :: l -> QName l
qNamePreludeFalse = qNamePreludeIdent "False"

-- Names from original (NotHat) prelude:

qNamePreludeGtGt :: l -> QName l
qNamePreludeGtGt = qNamePreludeSymbol ">>"

qNamePreludeGtGtEq :: l -> QName l
qNamePreludeGtGtEq = qNamePreludeSymbol ">>="

qNamePreludeFail :: l -> QName l
qNamePreludeFail = qNamePreludeIdent "fail"

mkExpPreludeEnumFrom :: l -> Exp l
mkExpPreludeEnumFrom l = Var l (qNamePreludeIdent "enumFrom")

mkExpPreludeEnumFromTo :: l -> Exp l
mkExpPreludeEnumFromTo l = Var l (qNamePreludeIdent "enumFromTo")

mkExpPreludeEnumFromThen :: l -> Exp l
mkExpPreludeEnumFromThen l = Var l (qNamePreludeIdent "enumFromThen")

mkExpPreludeEnumFromThenTo :: l -> Exp l
mkExpPreludeEnumFromThenTo l = Var l (qNamePreludeIdent "enumFromThenTo")

-- Building name qualifiers

qNameHatPreludeIdent :: String -> l -> QName l
qNameHatPreludeIdent ident l = 
  Qual l (ModuleName l "Hat.Prelude") (Ident l ident)

qNameHatPreludeSymbol :: String -> l -> QName l
qNameHatPreludeSymbol ident l = 
  Qual l (ModuleName l "Hat.Prelude") (Symbol l ident)

qNamePreludeIdent :: String -> l -> QName l
qNamePreludeIdent ident l = 
  Qual l (ModuleName l "Prelude") (Ident l ident)

qNamePreludeSymbol :: String -> l -> QName l
qNamePreludeSymbol ident l = 
  Qual l (ModuleName l "Prelude") (Symbol l ident)

qNameHatIdent :: String -> l -> QName l
qNameHatIdent ident l = Qual l (ModuleName l "Hat") (Ident l ident)

qNameShortIdent :: String -> l -> QName l
qNameShortIdent ident l = Qual l (ModuleName l "T") (Ident l ident)

qNameShortArity :: String -> l -> Arity -> QName l
qNameShortArity ident l a = qNameShortIdent (ident ++ show a) l


-- ----------------------------------------------------------------------------
-- Wrapping of untransformed code

expTo :: l -> Type l -> Exp l
expTo = expType True

expFrom :: l -> Type l -> Exp l
expFrom = expType False

-- The following assumes a limited form of types as they
-- occur in foreign import / export declarations.
-- Variables of kind other than * pose a problem.
expType :: Bool -> l -> Type l -> Exp l
expType to l (TyForall l _ _ _) = notSupported undefined "local type forall"
expType to l (TyFun l tyL tyR) =
  appN l 
    [Var l (mkTypeToken (prefix to ++ "Fun"))
    ,expType (not to) l tyL
    ,expType to pos tyR]
expType to l (TyTuple l boxed tys) =
  appN l
    (Var l (mkTypeToken (prefix to ++ "Tuple" ++ show (length tys))) :
     map (expType to l) tys)
expType to l (TyList l ty) =
  appN l [Var l (mkTypeToken (prefix to ++ "List")), expType to l ty]
expType to l (TyApp l tyL tyR) = 
  App l (expType to l tyL) (expType to l tyR)
expType to l (TyVar l _) =
  Var l (mkTypeToken (prefix to ++ "Id"))
expType to l (TyCon l qName) = 
  Var l (mkTypeToken (prefix to ++ getId qName))

prefix :: Bool -> String
prefix True = "to"
prefix False = "from"
  

-- ----------------------------------------------------------------------------
-- Useful stuff

-- Test for specific names:

isFunTyCon :: QName l -> Bool
isFunTyCon (Special _ (FunCon _)) = True
isFunTyCon _ = False


-- Frequently used in transformed code:

mkExpTuple :: [Exp SrcSpanInfo] -> Exp SrcSpanInfo
mkExpTuple es = Tuple noSpan es

mkPatTuple :: [Pat SrcSpanInfo] -> Pat SrcSpanInfo
mkPatTuple ps = PTuple noSpan ps 

mkFailExp :: Exp l -> Exp l
mkFailExp parent = App l (Var l (qNameFatal l)) parent
  where
  l = ann parent

expFail :: Exp SrcSpanInfo
expFail = mkFailExp expParent

patParent :: Pat SrcSpanInfo
patParent = PVar noSpan nameParent

expParent :: Exp SrcSpanInfo
expParent = Var noSpan (UnQualified noSpan nameParent)

-- Build parts of syntax tree:

-- Build n-ary application
-- pre-condition: list is non-empty
appN :: l -> [Exp l] -> Exp l
appN _ [e] = e
appN l (e:es) = App l e (appN l es)

-- Build n-ary type application
-- pre-condiiton: list is non-empy
tyAppN :: [Type l] -> Type l
tyAppN [t] = t
tyAppN (t:ts) = TyApp (ann t) t (tyAppN ts)

litInt :: Integral i => l -> i -> Lit l
litInt l i = Lit l (Int l (fromIntegral i) (show i))

litString :: l -> String -> Lit l
litString l str = Lit l (Str l str str) 
  
matchArity :: Match l -> Arity 
matchArity (Match _ _ pats _ _) = length pats
matchArity (InfixMatch _ p _ pats _ _) = 1 + length pats

changeInfixMatch :: Match l -> Match l
changeInfixMatch m@(Match _ _ _ _ _) = m
changeInfixMatch (InfixMatch l p n pats rhs maybeBinds) = 
  Match l n (p:pats) rhs maybeBinds

-- General working on AST:

-- function type constructor
infixr 6 `typeFun`
typeFun :: Type l -> Type l -> Type l
typeFun ty1 ty2 = TyFun (ann ty1) ty1 ty2

-- Compare two names ignoring location
eqName :: Name l -> Name l -> Bool
eqName (Ident _ n1) (Ident _ n2) = n1==n2
eqName (Symbol _ n1) (Symbol _ n2) = n1==n2
eqName _ _ = False 

getFieldNamesFromQualConDecl :: QualConDecl l -> [Name l]
getFieldNamesFromQualConDecl (QualConDecl _ _ _ conDecl) =
  getFieldNamesFromConDecl conDecl

getFieldNamesFromConDecl :: ConDecl l -> [Name l]
getFieldNamesFromConDecl (ConDecl _ _ _) = []
getFieldNamesFromConDecl (InfixConDecl _ _ _ _) = []
getFieldNamesFromConDecl (RecDecl _ _ fieldDecls) = 
  map getFieldNamesFromFieldDecl fieldDecls

getFieldNameFromFieldDecl :: FieldDecl l -> [Name l]
getFieldNameFromFieldDecl (FieldDecl _ names _) = names

getConstructorFromConDecl :: ConDecl l -> Name l
getConstructorFromConDecl (ConDecl _ c _) = c
getConstructorFromConDecl (InfixConDecl _ _ c _) = c
getConstructorFromConDecl (RecDecl _ c _) = c

getArityFromConDecl :: ConDecl l -> Int
getArityFromConDecl (ConDecl _ _ bTys) = length bTys
getArityFromConDecl (InfixConDecl _ _ _ _) = 2
getArityFromConDecl (RecDecl _ c fieldDecls) = 
  sum (map getArityFromFieldDecl fieldDecls)

getArityFromFieldDecl :: FieldDecl l -> Int
getArityFromFieldDecl (FieldDecl _ names _) = length names

alt2Match :: Alt l -> Match l
alt2Match (Alt l pat guardedAlts maybeBinds) = 
  Match l undefined [pat] (guardedAlts2Rhs guardedAlts) maybeBinds

guardedAlts2Rhs :: GuardedAlts l -> Rhs l
guardedAlts2Rhs (UnGuardedAlt l exp) = UnGuardedRhs l exp
guardedAlts2Rhs (GuardedAlts l gdAlts) = 
  GuardedRhs l (map guardedAlt2GuardedRhs gdAlts)

guardedAlt2GuardedRhs :: GuardedAlt l -> GuardedRhs l
guardedAlt2 (GuardedAlt l stmts exp) = GuardedRhs l stmts exp

qOp2Exp :: QOp l -> Exp l
qOp2Exp (QVarOp l qName) = Var l qName
qOp2Exp (QConOp l qName) = Con l qName

-- bogus span, does not appear in the source
noSpan :: SrcSpanInfo
noSpan = noInfoSpan (SrcSpan "" 0 0 0 0)

-- Test for specific names

-- Is this the module name "Main"?
isMain :: String -> Bool
isMain = (== "Main")

-- This test is an unsafe hack.
-- Even with qualification "Prelude" this may be a different "True"
-- There is no origin-tracking
isTrue :: QName l -> Bool
isTrue (Qual _ (ModuleName _ "Prelude") (Identifier "True")) = True
isTrue (UnQual _ (Identifier "True")) = True
isTrue _ = False

isOtherwise :: QName l -> Bool
isOtherwise (Qual _ (ModuleName _ "Prelude") (Identifier "otherwise")) = True
isOtherwise (UnQual _ (Identifier "otherwise")) = True
isOtherwise _ = False