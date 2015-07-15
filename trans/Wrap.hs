{- --------------------------------------------------------------------------- 
Wrapping is still experimental and I don't think the current implementation
does anything useful.

 Prepare a parsed module ready for the tracing transformation, when
 the module is not to be traced at all, but rather the original
 functions will be called and wrapped.  Essentially, we need to strip
 the module down to reveal just its interface.

 We assume that any function exported from the module must have a
 top-level type signature.  (If not, then it cannot be wrapped, so emit
 an error message.)

 Preparation of the syntax tree consists of:
   * traverse the export list
   * for each datatype exported abstractly, generate type-wrapping functions
   * for each datatype exported concretely, *** TO DO ***
   * for each function exported, grab its type signature and generate a
     "foreign import haskell" decl from it
   * in each class instance, generate "f.i.h" functions for each method - ToDo!
   * delete all imports, except those that bring a needed type into scope
   * add a new import of the original untraced version of this module
   * delete all other decls

Would need proper handling of environment to determine exactly
what is exported and to know about types that have to be imported
because they appear in foreign Haskell declarations. 
Currently very rough approximation.

 After preparation, the module can be passed through the normal tracing
 transformation, which already knows how to handle "foreign import haskell".
 (Except for the new type-wrapping functions, which get added without
 transformation; does this actually work?)

Currently this is a phase before the normal transformation.
Should also consider it as an alternative, writing the wapper .hs file.

--------------------------------------------------------------------------- -}
module Wrap (wrap) where

import Language.Haskell.Exts.Annotated
import System.FilePath(FilePath)
import SynHelp (nameFromOp,mkQual,declHeadName,declHeadTyVarBinds,getId
               ,isUnQual,getModuleNameFromModule,tyVarBind2Type,notSupported)

wrap :: FilePath -> Module SrcSpanInfo -> Module SrcSpanInfo
wrap filename mod@(Module l maybeModuleHead modulePragmas importDecls decls) =
  Module l maybeModuleHead modulePragmas importDecls' decls' 
  where
  importDecls' = mkImportOriginal modName : importDecls
    -- Only need to keep imports of type constructors used in foreign imports,
    -- but keeping all imports is a save over-approximation.
  decls' = concatMap (trans (getExported maybeModuleHead) modName) decls
  modName = getModuleNameFromModule mod

mkImportOriginal :: ModuleName l -> ImportDecl l
mkImportOriginal modName = 
  ImportDecl {importAnn = ann modName, importModule = modName, importQualified = True, importSrc = False,
    importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}

type Exported l = Name l -> Bool

getExported :: SrcInfo l => Maybe (ModuleHead l) -> Exported l
getExported Nothing = const True  -- everything exported
getExported (Just (ModuleHead l modName maybeWarningText Nothing)) = const True  -- everything exported
getExported (Just (ModuleHead l modName maybeWarningText (Just (ExportSpecList _ exportSpecs)))) = 
  \name -> (name `within`) `any` exportSpecs
  where
  within :: SrcInfo l2 => Name l1 -> ExportSpec l2 -> Bool
  name `within` (EVar l (NoNamespace _) qname) = same modName name qname
  name `within` (EVar l (TypeNamespace _) _) = notSupported l "type namespace in export specification"
  name `within` (EAbs l qname) = same modName name qname
  name `within` (EThingAll l qname) = same modName name qname
  name `within` (EThingWith l qname cnames) = same modName name qname
  name `within` (EModuleContents l eModName) = getId modName == getId eModName
  same :: ModuleName l1 -> Name l2 -> QName l3 -> Bool
  same mod n q = getId n == getId q && (isUnQual q || (\(Qual _ qm qn) -> getId qm == getId mod) q)

trans :: Exported l -> ModuleName l -> Decl l -> [Decl l]
trans exported modName (TypeDecl l head ty) = []  -- ToDo: keep if exported
trans exported modName (ClassDecl l maybeContext declHead funDeps maybeClassDecls) = []  -- ToDo
trans exported modName (InstDecl l maybeContext instHead maybeInstDecls) = []  -- ToDo
trans exported modName (DataDecl l dataOrNew maybeContext declHead qualConDecls maybeDeriving) | exported name =
  -- Assume data constructors are not exported, only produce wrapper data type. 
  -- Use type name also as data constructor name for the wrapper type.
  -- Doesn't handle deriving.
  -- ToDo: general case.
  [DataDecl l (NewType l) Nothing declHead 
    [QualConDecl l Nothing Nothing (ConDecl l name [newType])] Nothing]
  where
  -- need to get type variable parameters
  name = declHeadName declHead
  tyVars = map tyVarBind2Type (declHeadTyVarBinds declHead)
  newType = foldr1 (TyApp l) (TyCon l (mkQual modName name) : tyVars)
trans exported modName (TypeSig l names ty) | not (null expNames) =
  map (mkForeignHaskell modName ty) expNames
  where
  expNames = filter exported names
trans exported modName (InfixDecl l assoc maybeInt ops) | not (null expOps) =
  [InfixDecl l assoc maybeInt expOps]
  where
  expOps = filter (exported . nameFromOp) ops
trans exported modName (FunBind l matches) = []  -- remove function declaration
trans exported modName (PatBind l pat rhs maybeBinds) = []  -- remove pattern declaration
trans exported modName (ForImp l callConv maybeSafety maybeString name ty) | exported name =
  [mkForeignHaskell modName ty name]
trans _ _ _ = []  -- ToDo: check which other declarations do need special treatment


mkForeignHaskell :: ModuleName l -> Type l -> Name l -> Decl l
mkForeignHaskell mod ty name =
  ForImp l (StdCall l) Nothing (Just ("NotHat." ++ getId mod ++ '.' : getId name)) name ty
  where
  l = ann name