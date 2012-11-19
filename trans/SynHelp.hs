module SynHelp where

import Language.Haskell.Exts.Annotated 
import Data.Char (isAlpha)

type Arity = Int

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


-- Build n-ary type application
-- pre-condiiton: list is non-empy
tyAppN :: [Type l] -> Type l
tyAppN [t] = t
tyAppN (t:ts) = TyApp (ann t) t (tyAppN ts)

litInt :: (Show i, Integral i) => l -> i -> Exp l
litInt l i = Lit l (Int l (fromIntegral i) (show i))

litString :: l -> String -> Exp l
litString l str = Lit l (String l str str) 
  
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
  concatMap getFieldNameFromFieldDecl fieldDecls

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
  GuardedRhss l (map guardedAlt2GuardedRhs gdAlts)

guardedAlt2GuardedRhs :: GuardedAlt l -> GuardedRhs l
guardedAlt2GuardedRhs (GuardedAlt l stmts exp) = GuardedRhs l stmts exp

qOp2Exp :: QOp l -> Exp l
qOp2Exp (QVarOp l qName) = Var l qName
qOp2Exp (QConOp l qName) = Con l qName

-- The first name possibly provides a module qualifier
cName2QName :: QName l -> CName l -> QName l
cName2QName (UnQual _ _) (VarName l name) = UnQual l name
cName2QName (UnQual _ _) (ConName l name) = UnQual l name
cName2QName (Qual _ mod _) (VarName l name) = Qual l mod name
cName2QName (Qual _ mod _) (ConName l name) = Qual l mod name
cName2QName (Special _ _) (VarName l name) = UnQual l name
cName2QName (Special _ _) (ConName l name) = UnQual l name

-- Build parts of syntax tree:

mkQName :: l -> String -> QName l
mkQName l str = 
  if null revQual 
    then UnQual l (mkName l str) 
    else Qual l (ModuleName l (reverse (tail revQual))) 
           (mkName l (reverse revName))
  where
  (revName,revQual) = break (== '.') (reverse str)

mkName :: l -> String -> Name l
mkName l str =
  if isAlpha (head str)
    then Ident l str
    else Symbol l str

-- Test for specific names:

isFunTyCon :: QName l -> Bool
isFunTyCon (Special _ (FunCon _)) = True
isFunTyCon _ = False


combineMaybeContexts :: Maybe (Context l) -> Maybe (Context l) -> 
                        Maybe (Context l)
combineMaybeContexts Nothing mCtx = mCtx
combineMaybeContexts mCtx Nothing = mCtx
combineMaybeContexts (Just ctx1) (Just ctx2) = 
  if null assts then Nothing else CxTuple (ann ctx1) assts
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

