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

data Tracing = Traced | Trusted deriving Eq

data Scope = Global | Local deriving Eq

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
      [defNameMod pos modId filename traced] ++ 
      map (defNameVar Global Local modTrace) mvars ++ 
      map (defNameVar Local Local modTrace) vars ++ 
      (if traced then map (defNamePos modTrace) poss else []) 
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
  (decls',consts) = tDecls Global tracing (mkRoot span) decls
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
tDecls :: Environment -> Scope -> Tracing -> Exp SrcSpanInfo -> 
          [Decl SrcSpanInfo] ->
          ([Decl SrcSpanInfo], ModuleConsts)
tDecls env scope tracing parent decls = 
  foldr combine ([], emptyModuleConsts) 
    (map (tDecl env scope traced parent) decls)
  where
  combine :: ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts) -> 
             ([Decl SrcSpanInfo], ModuleConsts)
  combine (ds1, c1) (ds2, c2) = (ds1 ++ ds2, c1 `merge` c2)


tDecl :: Environment -> Scope -> Tracing -> Exp SrcSpanInfo ->
         Decl SrcSpanInfo ->
         ([Decl SrcSpanInfo], ModuleConsts)
tDecl env _ _ _ synDecl@(TypeDecl span declHead ty) =
  (map tTypeSynonym (splitSynonym span declHead ty), emptyModuleConsts)
  where
  tTypeSynonym :: Decl SrcSpanInfo -> Decl SrcSpanInfo
  tTypeSynonym (TypeDecl span declHead ty) =
    TypeDecl span (tDeclHead declHead) (tType ty)
tDecl env Global tracing _ d@(DataDecl span dataOrNew maybeContext declHead 
                               qualConDecls maybeDeriving) =
  (DataDecl span dataOrNew (fmap tContext maybeContext) (tDeclHead declHead)
     (map tQualConDecl qualConDecls) Nothing :
   -- "derive" must be empty, because transformed classes cannot be derived
   instDecl : filedSelectorDecls ++ deriveDecls
  ,foldr addConInfo (fieldSelectorConsts `merge` deriveConsts) qualConDecls)
  where
  (deriveDecls, deriveConsts) = 
    tDecls env Global Trusted (mkRoot noSpan) (derive d)
  instDecl = wrapValInstDecl env tracing maybeContext declHead qualConDecls
  (fieldSelectorDecls, fieldSelectorConsts) = mkFieldSelectors qualConDecls
tDecl env _ tracing parent  -- class without methods
  (ClassDecl l maybeContext declHead fundeps Nothing) =
  ([ClassDecl l (fmap tContext maybeContext) (tDeclHead declHead) 
     (map tFunDep fundeps) Nothing]
  ,emptyModuleConsts)
tDecl env _ tracing parent  -- class with methods
  (ClassDecl l maybeContext declHead fundeps (Just classDecls)) =
  (ClassDecl l (fmap tContext maybeContext) (tDeclHead declHead) 
    (map tFunDep fundeps) (Just classDecls') :
   auxDecls
  ,classifyMehtods declsConsts)
  where
  (classDecls', auxDecls, declsConsts) = 
    tClassDecls env tracing parent classDecls



-- Process class declarations:

-- Transform any declarations in a class declaration.
tClassDecls :: Environment -> Tracing -> Exp SrcSpanInfo -> 
               [ClassDecl SrcSpanInfo] ->
               ([ClassDecl SrcSpanInfo], ModuleConsts)
tClassDecls env tracing parent classDecls =
  (concat classDeclss', foldr merge emptyModuleConsts declsConsts)
  where
  (classDeclss', declsConsts) = 
    unzip (map (tClassDecl env tracing parent) classDecls)

tClassDecl :: Environment -> Tracing -> Exp SrcSpanInfo -> 
              ClassDecl SrcSpanInfo -> 
              ([ClassDecl SrcSpanInfo], ModuleConsts)
tClassDecl env tracing parent (ClsDecl l decl) =
  (map (ClsDecl l) decls', moduleConsts) 
  where
  (decls', moduleConsts) = tClassInstDecl env tracing parent decl
tClassDecl env tracing parent (ClsDataFam l _ _ _) = 
  notSupported l "declaration of an associated data type"
tClassDecl env tracing parent (ClsTyFam l _ _) =
  notSupported l "declaration of an associated type synonym"
tClassDecl env tracing parent (ClsTyDef l _ _) =
  notSupported l "default choice for an associated type synonym"

-- Transform a standard declaration inside a class or instance declaration.
-- Basically patch the result of the standard transformation of such a 
-- declaration.
tClassInstDecl :: Environment -> Tracing -> Exp SrcSpanInfo ->
                  Decl SrcSpanInfo ->
                  ([Decl SrcSpanInfo], ModuleConsts)
tClassInstDecl env tracing parent decl@(FunBind _ _) =
  -- Worker needs to be local, because it does not belong to the 
  -- class / instance, nor can it be outside of it.
  -- (Cannot use arity optimisation for a method anyway.)
  ([FunBind l [addToWhere match workerDecls]], moduleConsts)
  where
  (FunBind l [match] : _ : workerDecls, moduleConsts) =
    tDecl env Local tracing parent decl
tClassInstDecl env tracing parent decl@(PatBind _ _ _ _ _) =
  -- Currently don't do any patching!
  -- Use of sharing variable needs to be qualified if class name needs to be
  -- qualified (still covers not all necessary cases)
  -- note when declaring instance the class may only be imported qualified
  -- ??
  tDecl env Local tracing parent decl
tClassInstDecl env tracing parent decl@(TypeSig l names ty) =
  -- For every method type declaration produce an additional type declaration
  -- of the sharing variable.
  ([TypeSig l (tSpanShares names) (tConstType ty), tySig'], moduleConsts)
  where
  ([tySig'], moduleConsts) = tDecl env Local tracing parent decl
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
addConInfo (QualConDecl _ Nothing Nothing condecl) =
  addConDeclModuleConsts condecl
addConInfo (QualConDecl l _ _ _) = 
  notSupported l "existential quantification with data constructor"

addConDeclModuleConsts :: ConDecl SrcSpanInfo -> ModuleConsts -> ModuleConsts
addConDeclModuleConsts (ConDecl l name bangtypes) =
  addCon name [] 
addConDeclModuleConsts (InfixConDecl l btL name btR) =
  addCon name []
addConDeclModuleConsts (RecDecl l name fieldDecls) =
  addCon name (concatMap (\(FieldDecl _ names _) -> names) fieldDecls)


tDecl _ traced parent (DeclInstance pos contexts clsId insts decls) = 
  ([DeclInstance pos (tContexts contexts) clsId'
     (map tType insts) decls1]
  ,decls2  -- auxiliary definitions have to be outside the instance definition
  ,classifyMethods declsConsts)
  where
  clsId' = nameTransTyConCls clsId
  qualify = case clsId' of
              Qualified modrps _ -> forceM modrps
              _ -> id
  (decls1,decls2,declsConsts) = tDecls2 qualify traced parent decls
tDecl _ _ _ (DeclDefault tys) = ([],[],emptyModuleConsts) 
  -- defaulting does not work anyway, maybe warn about nonempty one?
tDecl _ _ _ d@(DeclPrimitive pos fnId arity ty) =
  error "TraceTrans:tDecl _ _ _ (DeclPrimitive _ _ _ _) should not occur"
tDecl _ _ _ (DeclForeignImp pos Haskell hasName fnId arity _ ty _) =
  tHaskellPrimitive pos 
    (if null revHasModNameP 
       then visible revHasUnqualName 
       else (qualify (tail revHasModNameP) revHasUnqualName))
    fnId arity ty
  where
  (revHasUnqualName,revHasModNameP) = span (/= '.') . reverse $ hasName 
tDecl _ _ _ 
      (DeclForeignImp pos callConv cname fnId arity fspec ty duplicateId) =
  (funDecls
  ,DeclForeignImp pos callConv
    (if null cname then getUnqualified fnId else cname) 
    (nameForeign fnId) arity fspec (typePlain ty) (nameForeign fnId)
   :wrapperDecls
  ,consts)
  where
  (funDecls,wrapperDecls,consts) = 
    tHaskellPrimitive pos (nameForeign fnId) fnId arity ty
tDecl _ _ _ (DeclForeignExp pos callConv str fnId _) =
  error ("Cannot trace foreign export (used at " ++ strPos pos ++ ")")
tDecl _ _ _ (DeclVarsType vars contexts ty) =
  -- type signatures need to be preserved (i.e. transformed),
  -- because e.g. polymorphic recursion needs them, more general
  -- types may later lead to ambiguous types
  ([DeclVarsType (tPosExps vars) (tContexts contexts) (tFunType ty)]
   ++ concatMap mkWorkerVarsType nonConstVars
  -- shared constants need to be typed, in case they are overloaded,
  -- so that monomorphic restriction does not lead to type error
  -- (actually then sharing is unfortunately lost)
   ++ if null constVars then [] 
        else [DeclVarsType (tPosShares constVars) 
               (tContexts contexts) (tConstType ty)]
  ,[],emptyModuleConsts)
  where
  (constVars,nonConstVars) = partition (isNonMethodConstant . snd) vars
  isNonMethodConstant :: TraceId -> Bool
  isNonMethodConstant id = 
    isLambdaBound id || -- variables in pattern bindings are lambda bound
      (case arity id of
        Just n  -> n == 0
        Nothing -> False)
  mkWorkerVarsType :: (Pos,TraceId) -> [Decl TokenId]
  mkWorkerVarsType (pos,id) =
    case arity id of
      Just n | n > 0 -> [DeclVarsType [(pos,nameWorker id)] 
                          (tContexts contexts) (tWorkerType n ty)]
      _ -> []

{-
  -- Variables of arity 0 do not take SR and Trace argument, so that
  -- their values are shared. Note that type signatures for class methods
  -- are handled differently by tDecl2
  ((if null constVars then [] 
      else [DeclVarsType (tPosExps constVars) 
             (tContexts contexts) (tConstType ty)])
   ++
   (if null nonConstVars then [] else [DeclVarsType (tPosExps nonConstVars) 
                                        (tContexts contexts) (tFunType ty)])
  ,[],emptyModuleConsts)
  where
  (constVars,nonConstVars) = partition (isNonMethodConstant . snd) vars
  isNonMethodConstant :: TraceId -> Bool
  isNonMethodConstant id = 
    isLambdaBound id || -- variables in pattern bindings are lambda bound
      (case arity id of
        Just n  -> n == 0
        Nothing -> False)
-}
tDecl scope traced parent (DeclPat (Alt (ExpVar pos id) rhs decls)) = 
  -- this case may occur because of the next equation
  tCaf scope traced parent pos id rhs decls
tDecl scope traced parent (DeclPat (Alt (PatAs pos id pat) rhs decls)) = 
  (dFun1++dPat1,dFun2++dPat2,funConsts `merge` patConsts)
  where
  id' = modLetBound id
  (dFun1,dFun2,funConsts) = tCaf scope traced parent pos id' rhs decls
  (dPat1,dPat2,patConsts) = 
    tDecl scope traced parent 
      (DeclPat (Alt pat (Unguarded (ExpVar pos id')) noDecls))
tDecl scope traced parent (DeclPat (Alt pat rhs decls)) =
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
  (map useDef patPosIds
  ,DeclFun noPos patId 
    [Fun [] 
      (Unguarded 
        (ExpCase noPos exp'
          [Alt pat'' (Unguarded tuple) noDecls
          ,Alt (PatWildcard noPos) (Unguarded (mkFailExp noPos parent)) noDecls
          ]))
      decls']
   : map projDef patPosIds
  ,foldr (\(pos,id) -> addVar pos id) 
    (emptyModuleConsts `withLocal` altConsts) patPosIds)
  where
  pos = getPos pat
  firstId = snd . head $ patPosIds
  patId = nameTraceShared pos firstId
  resultTraceId = nameTrace2 firstId
  tuple = mkTupleExp noPos (ExpVar noPos resultTraceId : patVars')
  patPosIds = map (\(ExpVar pos id) -> (pos,id)) patVars
  (patVars',Nothing) = tPats patVars 
  patVars = getPatVars pat
  pat'' = case pat' of
           ExpApplication p [r,v,_] -> 
             ExpApplication p [r,v,ExpVar noPos resultTraceId]
  (Fun [pat'] (Unguarded exp') decls',altConsts) = 
     tFun traced False parent failContinuation (Fun [pat] rhs decls)
  useSR = ExpVar pos (nameSR firstId)
  useParent = mkParentVar pos

  useDef :: (Pos,TraceId) -> Decl TokenId
  useDef (pos,id) =
    DeclFun pos (nameTransLetVar id)
      [Fun [useSR,useParent]
        (Unguarded (ExpApplication pos
          [combConstUse pos traced,useSR,useParent,ExpVar pos (nameShare id)]))
        noDecls]

  projDef :: (Pos,TraceId) -> Decl TokenId
  projDef (pos,id) =
    DeclFun pos (nameShare id) 
      [Fun []
        (Unguarded (ExpApplication pos 
          [combConstDef pos traced 
          ,parent
          ,ExpVar pos (nameTraceInfoVar pos scope id)
          ,ExpLambda pos [PatWildcard pos]
            (ExpCase pos (ExpVar pos patId)
              [Alt tuple
                (Unguarded 
                  (if isLocal scope && not traced
                     then ExpVar pos (nameTransLambdaVar id)
                     else
                       ExpApplication pos 
                         [ExpVar pos tokenProjection
                         ,mkSRExp pos traced
                         ,ExpVar pos resultTraceId
                         ,ExpVar pos (nameTransLambdaVar id)]))
                noDecls])]))
         noDecls]

  getPatVars :: Pat id -> [Pat id]
  getPatVars (ExpRecord pat fields) =
    getPatVars pat ++ concatMap getFieldVars fields
    where
    getFieldVars (FieldExp _ _ pat) = getPatVars pat
  getPatVars (ExpApplication _ pats) = concatMap getPatVars pats
  getPatVars pat@(ExpVar pos id) = [pat]
  getPatVars (ExpCon _ _) = []
  getPatVars (ExpLit _ _) = []
  getPatVars (ExpList _ pats) = concatMap getPatVars pats
  getPatVars (PatAs pos id pat) = ExpVar pos id : getPatVars pat
  getPatVars (PatWildcard _) = []
  getPatVars (PatIrrefutable _ pat) = getPatVars pat
tDecl scope traced parent (DeclFun pos id [Fun [] rhs localDecls]) = 
  tCaf scope traced parent pos id rhs localDecls
    -- a caf has many dynamic parents and hence uses the static parent
tDecl _ _ parent (DeclFun pos id (Fun [] _ _ : _)) =
  error ("Variable multiple defined: " ++ show (tokenId id))
tDecl scope traced parent (DeclFun pos id funs) = 
  tFuns scope traced pos id funs  -- a function does not use the static parent
tDecl _ _ _ (DeclFixity _) = ([],[],emptyModuleConsts) 
  -- fixity declarations have been processed before 
  -- not needed in output, because pretty printer produces unambiguous output
tDecl _ _ _ (DeclIgnore s) = ([DeclIgnore s],[],emptyModuleConsts)
tDecl _ _ _ _ = error "tDecl: unknown sort of declaration"


-- for declarations in class and instance definitions:
-- (considered local, because they have their own scope)
tDecls2 :: (TokenId -> TokenId) -> Bool -> Exp TokenId -> Decls TraceId 
        -> (Decls TokenId,[Decl TokenId],ModuleConsts)
tDecls2 qualify traced parent (DeclsParse decls) = 
  (DeclsParse (concat declss1 ++ catMaybes (map declSharedVar decls))
  ,concat declss2
  ,foldr merge emptyModuleConsts declsConstss)
  where
  (declss1,declss2,declsConstss) = 
    unzip3 (map (tDecl2 qualify traced parent) . combineFuns $ decls)

-- for a method type declaration produce type declaration of sharing var
declSharedVar :: Decl TraceId -> Maybe (Decl TokenId)
declSharedVar (DeclVarsType vars contexts ty) =
  Just (DeclVarsType (tPosShares vars) (tContexts contexts) (tConstType ty))
declSharedVar _ = Nothing

tDecl2 :: (TokenId -> TokenId) -> Bool -> Exp TokenId -> Decl TraceId 
       -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tDecl2 _ traced parent decl@(DeclFun pos id (Fun (x:xs) rhs localDecls : funs)) =
  -- patch result of tFuns:
  -- worker needs to be local, because it does not belong to the 
  -- class/instance nor can it be outside of it
  --  (no known arity optimisation anyway)
  ([DeclFun pos id' 
     [Fun args' rhs' (DeclsParse workerDecls')]]
  ,[]
  ,declConsts')
  where
  ((DeclFun _ id' [Fun args' rhs' _]:_:workerDecls'),[],declConsts') = 
    tDecl Local traced parent decl
tDecl2 qualify traced parent decl@(DeclFun pos id ([Fun [] rhs localDecls])) =
  -- patch result of constant transformation
  -- use of sharing variable needs to be qualified if class name needs to be
  -- qualified (still covers not all necessary cases)
  -- note when declaring instance the class may only be imported qualified
  ([DeclFun pos id'
     [Fun args' (Unguarded (ExpApplication pos
       [a1,a2,a3,ExpVar pos (qualify id'')]))
     noDecls]
   ,shared'],[],declConsts')
  where
  ([DeclFun _ id'
     [Fun args' 
       (Unguarded (ExpApplication _
         [a1,a2,a3,ExpVar _ id''])) 
       _]
   ,shared'],_,declConsts')
    = tCaf Local traced parent pos id rhs localDecls
tDecl2 _ traced parent decl = 
  -- type signature only possible remaining case
  tDecl Local traced parent decl


singleDecl :: Decl id -> ([Decl id],[a],ModuleConsts)
singleDecl decl = ([decl],[],emptyModuleConsts)

-- Sharing of constants in classes/instances
-- may be lost if class/instance has a context,
-- because then the shareId also has this context and is no longer a constant.



-- constructor definition in type definition
tConstr :: Constr TraceId -> Constr TokenId
tConstr (Constr pos conId tyArgs) =
  Constr pos (nameTransCon conId) (tTyArgs tyArgs)
tConstr (ConstrCtx tyVars contexts pos conId tyArgs) =
  ConstrCtx (tPosTyVars tyVars) (tContexts contexts) 
    pos (nameTransCon conId) (tTyArgs tyArgs)

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

nameTransCls :: Id i = i -> i
nameTransCls = id  -- unchanged

nameTransTy :: Id i => i -> i
nameTransTy = id  -- unchanged

nameTransSyn :: Id i => i -> i
nameTransSyn = id  -- unchanged

-- names of helper synonyms are a bit of a hack; a name conflict is possible
nameTransSynHelper :: Id i => i -> Int -> i
nameTransSynHelper syn no = updateToken (++ ("___" ++ show no)) syn
  where 
  update (Ident l name) = Ident l (name ++ "___" ++ show no)
  update (Symbol _ _) = 
    error "TraceTrans, nameTransSynHelper: synom name is a symbol"

nameTransTyVar :: Id i => i -> i
nameTransTyVar = id  -- unchanged

nameTransCon :: Id i => i -> i
nameTransCon = id  -- unchanged

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

-- Hardwired identifiers

tracingModuleNameShort :: ModuleName SrcSpanInfo
tracingModuleNameShort = ModuleName noSpan "T"

-- names for trace constructors

qNameHatMkModule :: l -> QName l
qNameHatMkModule l = qNameHatIdent l "mkModule"

qNameHatMkAtomConstructor :: l -> Bool -> QName l
qNameHatMkAtomConstructor l withFields =
  qNameHatIdent l 
    (if withFields then "mkConstructorWFields" else "mkConstructor")

qNameHatMkAtomVariable :: l -> QName l
qNameHatMkAtomVariable = qNameHatIdent l "mkVariable"

qNameHatMkSpan :: l -> QName l
qNameHatMkSpan l = qNameHatIdent l "mkSrcPos"

qNameHatMkNoSpan :: L -> QName l
qNameHatMkNoSpan l = qNameHatIdent l "mkNoSrcPos"

qNamePreludeTrue :: l -> QName l
qNamePreludeTrue l = qNamePreludeIdent l "True"

qNamePreludeFalse :: l -> QName l
qNamePreludeFalse l = qNamePreludeIdent l "False"

qNamePreludeIdent :: l -> String -> QName l
qNamePreludeIdent l ident = Qual l (ModuleName l "Prelude") (Ident l ident)

qNameHatIdent :: l -> String -> QName l
qNameHatIdent l ident = Qual l (ModuleName l "Hat") (Ident l ident)

-- Useful stuff

-- Build parts of syntax tree:

-- Build n-ary application
-- pre-condition: list is non-empty
appN :: l -> [Exp l] -> Exp l
appN _ [e] = e
appN l (e:es) = App l e (appN l es)

litInt :: Integral i => l -> i -> Lit l
litInt l i = Lit l (Int l (fromIntegral i) (show i))

litString :: l -> String -> Lit l
litString l str = Lit l (Str l str str) 
  

-- bogus span, does not appear in the source
noSpan :: SrcSpanInfo
noSpan = noInfoSpan (SrcSpan "" 0 0 0 0)