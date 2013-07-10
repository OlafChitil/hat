-- Useful functions on annotated Haskell syntax
-- All functions are polymorphic in the annotation type.
-- (They may loose annotations or add them rather arbitrarily.)

module SynHelp where

import Language.Haskell.Exts.Annotated 
import Wired (nameTransModule,tracingModuleNameShort,Arity(..))
import Data.Char (isAlpha)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Debug.Trace

-- Common features of all identifiers (names)

class Id a where
  -- whether a symbol (operator) or a normal identifier
  isSymbol :: a -> Bool
  getId :: a -> String

instance Id (QName l) where
  isSymbol (Qual _ _ name) = isSymbol name
  isSymbol (UnQual _ name) = isSymbol name
  isSymbol (Special _ _) = True
  getId (Qual _ _ name) = getId name
  getId (UnQual _ name) = getId name

instance Id (Name l) where
  isSymbol (Ident _ _) = False
  isSymbol (Symbol _ _) = True
  getId (Ident _ ident) = ident
  getId (Symbol _ ident) = ident

instance Id (QOp l) where
  isSymbol (QVarOp _ _) = False
  isSymbol (QConOp _ _) = True
  getId (QVarOp _ qname) = getId qname
  getId (QConOp _ qname) = getId qname

instance Id (Op l) where
  isSymbol (VarOp _ _) = False
  isSymbol (ConOp _ _) = True
  getId (VarOp _ name) = getId name
  getId (ConOp _ name) = getId name

instance Id (CName l) where
  isSymbol (VarName _ _) = False
  isSymbol (ConName _ _) = True
  getId (VarName _ name) = getId name
  getId (ConName _ name) = getId name

instance Id (ModuleName l) where
  isSymbol _ = False
  getId (ModuleName _ ident) = ident

-- General functions on syntax tree:

conDeclName :: ConDecl l -> Name l
conDeclName (ConDecl _ name _) = name
conDeclName (InfixConDecl _ _ name _) = name
conDeclName (RecDecl _ name _) = name

conDeclArity :: ConDecl l -> Arity
conDeclArity (ConDecl _ _ tys) = length tys
conDeclArity (InfixConDecl _ _ _ _) = 2
conDeclArity (RecDecl _ _ fieldDecls) = 
  sum (map (length . fieldDeclNames) fieldDecls)

fieldDeclNames :: FieldDecl l -> [Name l]
fieldDeclNames (FieldDecl _ names _) = names


-- Build n-ary application
-- pre-condition: list is non-empty
appN :: [Exp l] -> Exp l
appN = foldl1 (\e1 e2 -> App (ann e1) e1 e2)

-- Build n-ary type application
-- pre-condiiton: list is non-empy
tyAppN :: [Type l] -> Type l
tyAppN = foldl1 (\t1 t2 -> TyApp (ann t1) t1 t2)

litInt :: (Show i, Integral i) => l -> i -> Exp l
litInt l i = Lit l (Int l (fromIntegral i) (show i))

litString :: l -> String -> Exp l
litString l str = Lit l (String l str str) 

litChar :: l -> Char -> Exp l
litChar l c = Lit l (Char l c (show c))
  
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

getModuleNameFromModule :: Module l -> ModuleName l
getModuleNameFromModule (Module l Nothing _ _ _) = ModuleName l "Main"
getModuleNameFromModule (Module l (Just (ModuleHead _ modName _ _)) _ _ _) = modName

getConDeclFromQualConDecl :: QualConDecl l -> ConDecl l
getConDeclFromQualConDecl (QualConDecl _ _ _ conDecl) = conDecl

getFieldNamesFromQualConDecl :: QualConDecl l -> [Name l]
getFieldNamesFromQualConDecl (QualConDecl _ _ _ conDecl) =
  getFieldNamesFromConDecl conDecl

getFieldNamesFromConDecl :: ConDecl l -> [Name l]
getFieldNamesFromConDecl (ConDecl _ _ _) = []
getFieldNamesFromConDecl (InfixConDecl _ _ _ _) = []
getFieldNamesFromConDecl (RecDecl _ _ fieldDecls) = 
  concatMap getFieldNamesFromFieldDecl fieldDecls

getFieldNamesFromFieldDecl :: FieldDecl l -> [Name l]
getFieldNamesFromFieldDecl (FieldDecl _ names _) = names

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
  GuardedRhss l (map guardedAlt2GuardedRhs gdAlts)

guardedAlt2GuardedRhs :: GuardedAlt l -> GuardedRhs l
guardedAlt2GuardedRhs (GuardedAlt l stmts exp) = GuardedRhs l stmts exp

qOp2Exp :: QOp l -> Exp l
qOp2Exp (QVarOp l qName) = Var l qName
qOp2Exp (QConOp l qName) = Con l qName

getQualified :: SrcInfo l => QName l -> Name l
getQualified (UnQual _ name) = name
getQualified (Qual _ _ name) = name
getQualified (Special _ specialCon) = 
  error ("SynHelp.getQualified: special QName " ++ specialToId specialCon ++ ".")

mkQual :: ModuleName l -> Name l -> QName l
mkQual modName name = Qual (ann name) modName name

qual ::SrcInfo l => ModuleName l -> QName l -> QName l
qual modName qName = mkQual modName (getQualified qName)

isQual :: QName l -> Bool
isQual (Qual _ _ _) = True
isQual (UnQual _ _) = False
isQual (Special _ _) = False

isUnQual :: QName l -> Bool
isUnQual (UnQual _ _) = True
isUnQual (Qual _ _ _) = False
isUnQual (Special _ _) = False

-- The first name possibly provides a module qualifier
cName2QName :: QName l -> CName l -> QName l
cName2QName (UnQual _ _) (VarName l name) = UnQual l name
cName2QName (UnQual _ _) (ConName l name) = UnQual l name
cName2QName (Qual _ mod _) (VarName l name) = Qual l mod name
cName2QName (Qual _ mod _) (ConName l name) = Qual l mod name
cName2QName (Special _ _) (VarName l name) = UnQual l name
cName2QName (Special _ _) (ConName l name) = UnQual l name

-- Build parts of syntax tree:

-- String may contain . with module name before
-- Assume that this is a normal identifier or symbol, not
-- any special name.
mkQName :: l -> String -> QName l
-- mkQName l "()" = Special l (UnitCon l)
-- mkQName l "[]" = Special l (ListCon l)  -- list type constructor
-- mkQName l "->" = Special l (FunCon l) -- function type constructor
-- mkQName l i@(',':_) = Special l (TupleCon l Boxed (length i))
-- mkQName l ":" = Special l (Cons l) -- list data constructor
-- mkQName l "##" = Special l (UnboxedSingleCon l) -- unboxed singleton tuple constructor
mkQName l str = 
  if length revQual <= 1  -- include function composition "."
    then UnQual l (mkName l str) 
    else Qual l (ModuleName l (reverse (tail revQual))) 
           (mkName l (reverse revName))
  where
  (revName,revQual) = break (== '.') (reverse str)

mkName :: l -> String -> Name l
mkName _ "" = error "SynHelp.mkName: empty String."
mkName l id@(x:_) =
  if isAlpha x || x == '_'
    then Ident l id
    else Symbol l id

-- Test for specific names:

isFunTyCon :: QName l -> Bool
isFunTyCon (Special _ (FunCon _)) = True
isFunTyCon _ = False


combineMaybeContexts :: Maybe (Context l) -> Maybe (Context l) -> 
                        Maybe (Context l)
combineMaybeContexts Nothing mCtx = mCtx
combineMaybeContexts mCtx Nothing = mCtx
combineMaybeContexts (Just ctx1) (Just ctx2) = 
  if null assts then Nothing else Just (CxTuple (ann ctx1) assts)
  where
  assts = contextAssertions ctx1 ++ contextAssertions ctx2

contextAssertions :: Context l -> [Asst l]
contextAssertions (CxSingle _ asst) = [asst]
contextAssertions (CxTuple _ assts) = assts
contextAssertions (CxParen _ ctx) = contextAssertions ctx
contextAssertions (CxEmpty _) = []

instHeadQName :: InstHead l -> QName l
instHeadQName (IHead _ qname _) = qname
instHeadQName (IHInfix _ _ qname _) = qname
instHeadQName (IHParen _ ih) = instHeadQName ih

declHeadName :: DeclHead l -> Name l
declHeadName (DHead _ name _) = name
declHeadName (DHInfix _ _ name _ ) = name
declHeadName (DHParen _ dh) = declHeadName dh

declHeadTyVarBinds :: DeclHead l -> [TyVarBind l]
declHeadTyVarBinds (DHead _ _ tvbs) = tvbs
declHeadTyVarBinds (DHInfix _ tvbL _ tvbR) = [tvbL,tvbR]
declHeadTyVarBinds (DHParen _ dh) = declHeadTyVarBinds dh

tyVarBind2Type :: TyVarBind l -> Type l
tyVarBind2Type (KindedVar l name kind) = TyKind l (TyVar l name) kind
tyVarBind2Type (UnkindedVar l name) = TyVar l name

tyVarBind2Name :: TyVarBind l -> Name l
tyVarBind2Name (KindedVar l name kind) = name
tyVarBind2Name (UnkindedVar l name) = name

-- pre-condition: no type synonym appearing in type
decomposeFunType :: Type l -> ([Type l], Type l)
decomposeFunType (TyFun _ tyL tyR) = (tyL:tyArgs, tyRes)
  where
  (tyArgs, tyRes) = decomposeFunType tyR
decomposeFunType ty = ([], ty)

-- ----------------------------------------------------------------------------
-- Updating a name

class Id a => UpdId a where
  -- apply function to unqualified name part 
  -- and prefix module name (if qualified)
  updateId :: (Name l -> Name l) -> a -> a

instance SrcInfo l => UpdId (QName l) where
  updateId f (Qual l moduleName name) = 
    Qual l (nameTransModule moduleName) (updateId f name)
  updateId f (UnQual l name) = UnQual l (updateId f name)
  updateId f (Special l specialCon) =
    Qual l (tracingModuleNameShort l) (updateId f (Ident l (specialToId specialCon))) 

specialToId :: SrcInfo l => SpecialCon l -> String
specialToId (UnitCon _) = "Tuple0"
specialToId (ListCon _) = "List"
specialToId (FunCon _) = "Fun"
specialToId (TupleCon _ Boxed arity) = "Tuple" ++ show arity
specialToId (TupleCon l' Unboxed _) = notSupported l' "Unboxed tuple."
specialToId (Cons l') = "Cons"
specialToId (UnboxedSingleCon l') =
  notSupported l' "Unboxed singleton tuple constructor."

instance UpdId (Name l) where
  -- updateId f = f  is ill-typed
  updateId f (Ident l ident) = Ident l (getId (f (Ident undefined ident)))
  updateId f (Symbol l ident) = Symbol l (getId (f (Symbol undefined ident)))

instance SrcInfo l => UpdId (QOp l) where
  updateId f (QVarOp l qname) = QVarOp l (updateId f qname)
  updateId f (QConOp l qname) = QConOp l (updateId f qname)

instance UpdId (Op l) where
  updateId f (VarOp l name) = VarOp l (updateId f name)
  updateId f (ConOp l name) = ConOp l (updateId f name)

instance UpdId (CName l) where
  updateId f (VarName l name) = VarName l (updateId f name)
  updateId f (ConName l name) = ConName l (updateId f name)

-- ----------------------------------------------------------------------------

dropAnn :: Annotated ast => ast l -> ast ()
dropAnn = fmap (const ())


-- bogus span, does not appear in the source
noSpan :: SrcSpanInfo
noSpan = noInfoSpan (SrcSpan "" 0 0 0 0)

isNoSpan :: SrcSpanInfo -> Bool
isNoSpan (SrcSpanInfo (SrcSpan "" _ _ _ _) []) = True
isNoSpan _ = False

emptySpan :: SrcSpan
emptySpan = SrcSpan "" 0 0 0 0

isEmptySpan :: SrcSpan -> Bool
isEmptySpan (SrcSpan "" _ _ _ _) = True
isEmptySpan _ = False

-- ----------------------------------------------------------------------------
-- Error for non-supported language features

notSupported :: SrcInfo l => l -> String -> a
notSupported l construct = error $
  "hat-trans: unsupported language construct \"" ++ construct ++ "\" at " ++ 
    fileName l ++ ":" ++ show (startLine l) ++ ":" ++ show (startColumn l)

