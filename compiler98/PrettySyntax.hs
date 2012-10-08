{-
Convert abstract syntax tree (or parts of it) into 
a structured document for pretty printing.
-}

module PrettySyntax
  ( PPInfo(..)
--  , prettyPrintSimple
  , prettyPrintTokenId, prettyPrintId, simplePrintId
  , ppModule, ppTopDecls, ppClassCodes
  , ppType, ppContexts, ppSimple, ppDecl
  ) where 

import Extra(noPos)
import PrettyLib
import Syntax hiding (noDecls,TokenId)
import TokenId(TokenId(..),extractV,t_Arrow,t_List)
import Id(Id)
import IntState(IntState,strIS,lookupIS)
import Nice(niceInt,niceNT,mkAL)
import Info(Info(InfoData),tidI)
import NT(NewType(NewType))
import SysDeps (unpackPS,isAlphaNum)
import Data.Maybe(isJust,fromJust)
import Flags(Flags,sShowWidth,sShowQualified,sShowIndent)


{-
prettyPrintSimple :: Int -> (PPInfo TokenId -> a -> Doc) -> a -> String
prettyPrintSimple width pp =
  pretty width . 
  pp PPInfo{withPositions = False
           ,indent = 4
           ,id2str = show
           ,tyVar2str = show
           ,intState = error "prettyPrintTokenId: no intState"
           ,isFunctionArrow = (== t_Arrow)
           ,isList = (== t_List)
           ,maybeTuple = maybeTupleTokenId}
  where
  maybeTupleTokenId t = case t of
                          TupleId n -> Just n
                          _         -> Nothing
-}

prettyPrintTokenId :: Flags -> (PPInfo TokenId -> a -> Doc) -> a -> String
prettyPrintTokenId flags pp =
  pretty (sShowWidth flags) . 
  pp PPInfo{withPositions = False
           ,indent = sShowIndent flags
           ,id2str = id2strTokenId
           ,tyVar2str = show
           ,intState = error "prettyPrintTokenId: no intState"
           ,isFunctionArrow = (== t_Arrow)
           ,isList = (== t_List)
           ,maybeTuple = maybeTupleTokenId}
  where
  id2strTokenId =  if sShowQualified flags
                     then show  
                     else reverse . unpackPS . extractV 
  maybeTupleTokenId t = case t of
                          TupleId n -> Just n
                          _         -> Nothing


prettyPrintId :: Flags -> IntState -> (PPInfo Id -> a -> Doc) -> a -> String
prettyPrintId flags state pp =
  pretty (sShowWidth flags) . 
  pp PPInfo{withPositions = False 
           ,indent = sShowIndent flags
           ,id2str = if sShowQualified flags 
                       then strIS state else strISunqualified state
           ,tyVar2str = ('t' :) . show
           ,intState = state
           ,isFunctionArrow = isFunctionArrowId state
           ,isList = isListId state 
           ,maybeTuple = maybeTupleId state}


simplePrintId :: IntState -> (PPInfo Id -> a -> Doc) -> a -> String
simplePrintId state pp =
  simple . 
  pp PPInfo{withPositions = False 
           ,indent = 2 
           ,id2str = strISunqualified state
           ,tyVar2str = ('t' :) . show
           ,intState = state
           ,isFunctionArrow = isFunctionArrowId state
           ,isList = isListId state 
           ,maybeTuple = maybeTupleId state}


-- the following little functions should probably be defined in some other
-- modules:

strISunqualified :: IntState -> Id -> String
strISunqualified state id = 
  case lookupIS state id of
    Just info -> reverse . unpackPS . extractV . tidI $ info
    Nothing -> 'v' : show id


isFunctionArrowId :: IntState -> Id -> Bool
isFunctionArrowId state id = 
  case lookupIS state id of
    Just info -> tidI info == t_Arrow
    Nothing -> False


isListId :: IntState -> Id -> Bool
isListId state id = 
  case lookupIS state id of
    Just info -> tidI info == t_List 
    Nothing -> False


maybeTupleId :: IntState -> Id -> Maybe Int
maybeTupleId state id = 
  case lookupIS state id of
    Just info -> case tidI info of
                   TupleId n -> Just n
                   _         -> Nothing 
    Nothing -> Nothing


{- 
Information that is passed recursively unchanged 
to all pretty printing functions.
If boolean argument true, then positions are inserted as comments 
for most language constructs.
The function converts an identifier into a String.
-}

data PPInfo a = PPInfo {withPositions :: Bool
                       ,indent :: Int -- indentation for nesting
                       ,id2str :: a -> String
                       ,tyVar2str :: a -> String
                       ,intState :: IntState -- still needed for niceNT
                       ,isFunctionArrow :: a -> Bool -- ->
                       ,isList :: a -> Bool  -- []
                       ,maybeTuple :: a -> Maybe Int}


{- standard number of characters of indentation for every nesting -}

nestS :: PPInfo a -> Doc -> Doc
nestS = nest . indent

groupNestS :: PPInfo a -> Doc -> Doc
groupNestS = groupNest . indent


{- general formatting functions ------------------------------------------ -}

space :: Doc
space = text " "

dSpace :: Doc
dSpace = delimiter " "

fSpace :: Doc
fSpace = fdelimiter " "

fComma :: Doc
fComma = fdelimiter "" <> text ","

dSemiSpace :: Doc
dSemiSpace = delimiter "; "

sep :: Doc -> [Doc] -> Doc
sep delimiter [] = nil
sep delimiter docs = foldr1 (\l r -> l <> delimiter <> r) docs

term :: Doc -> [Doc] -> Doc
term delimiter = foldr (\l r -> l <> delimiter <> r) nil

encase :: Doc -> [Doc] -> Doc
encase delimiter docs = delimiter <> term delimiter docs

{- surround by paranthesis and separate by fComma, but not for 0 element -}
parensFComma1 :: PPInfo a -> [Doc] -> Doc
parensFComma1 info [] = nil
parensFComma1 info docs = 
  fSpace <> (groupNestS info $ parens $ sep fComma docs)


{- surround by paranthesis and separate by fComma, but not for 0 or 1 element 
-}
parensFComma2 :: PPInfo a -> [Doc] -> Doc
parensFComma2 info [] = nil
parensFComma2 info [doc] = doc
parensFComma2 info docs = groupNestS info $ parens $ sep fComma docs 


groupParens :: PPInfo a -> Doc -> Doc
groupParens info = groupNestS info . parens


{- the conversion functions for every abstract syntax construct ---------- -}

ppModule :: PPInfo a -> Module a -> Doc
ppModule info (Module pos id exports impdecls fixdecls topdecls) = 
    groupNestS info 
      (text "module " <> ppId info id <> 
      ppExports info exports <> text " where") <> line <> 
  ppImpDecls info impdecls <>
  ppFixDecls info fixdecls <> 
  ppTopDecls info topdecls


ppExports :: PPInfo a -> Maybe [Export a] -> Doc
ppExports info Nothing = parens nil
ppExports info (Just []) = nil
ppExports info (Just exps) = parensFComma1 info (map (ppExport info) exps)


ppExport :: PPInfo a -> Export a -> Doc
ppExport info (ExportEntity pos entity) = ppEntity info entity
ppExport info (ExportModid pos id) = text "module " <> ppId info id 


ppEntity :: PPInfo a -> Entity a -> Doc
ppEntity info (EntityVar pos id) = ppIdAsVar info id
ppEntity info (EntityConClsAll pos id) = ppId info id <> text "(..)"
ppEntity info (EntityConClsSome pos id ids) = 
  nestS info $ 
    ppId info id <> (parens . sep fComma . map (ppIdAsVar info . snd) $ ids)


ppImpDecls :: PPInfo a -> [ImpDecl a] -> Doc
ppImpDecls info [] = nil
ppImpDecls info decls = 
  line <> (sep line . map (ppImpDecl info) $ decls) <> line


ppFixDecls :: PPInfo a -> [FixDecl a] -> Doc
ppFixDecls info [] = nil
ppFixDecls info decls = 
  line <> (sep line . map (ppFixDecl info) $ decls) <> line


ppInfixClass :: PPInfo a -> InfixClass a -> Doc
ppInfixClass info InfixDef = text "infixl{-def-} "
ppInfixClass info InfixL   = text "infixl "
ppInfixClass info InfixR   = text "infixr "
ppInfixClass info Infix    = text "infix  "
ppInfixClass info (InfixPre id) = 
  text "prefix " <> ppIdAsOperator info id <> space


ppFixDecl :: PPInfo a -> FixDecl a -> Doc
ppFixDecl info (infixclass, precedenceLevel, fixIds) =
  groupNestS info $
    ppInfixClass info infixclass <> text (show precedenceLevel) <> 
    fSpace <> sep fComma (map (ppIdAsOperator info . stripFixId) fixIds)


ppImpSpec :: PPInfo a -> ImpSpec a -> Doc
ppImpSpec info (NoHiding entities) =
  fSpace <> (parens . sep fComma . map (ppEntity info) $ entities)
ppImpSpec info (Hiding []) = nil
ppImpSpec info (Hiding entities) =
  fSpace <> text "hiding " <> 
  (parens . sep fComma . map (ppEntity info) $ entities)


ppImpDecl :: PPInfo a -> ImpDecl a -> Doc
ppImpDecl info (Import (pos,id) impspec) =
  groupNestS info $ 
    text "import " <> ppId info id <> fSpace <> ppImpSpec info impspec
ppImpDecl info (ImportQ (pos1,id1) impspec) =
  groupNestS info $
    text "import qualified " <> ppId info id1 <> fSpace <> 
      ppImpSpec info impspec
ppImpDecl info (ImportQas (pos1,id1) (pos2,id2) impspec) =
  groupNestS info $
    text "import qualified " <> ppId info id1 <> fSpace <> text "as" <>
      fSpace <> ppId info id2 <> fSpace <> ppImpSpec info impspec
ppImpDecl info (Importas (pos1,id1) (pos2,id2) impspec) =
  groupNestS info $
    text "import " <> ppId info id1 <> fSpace <> text "as" <>
      fSpace <> ppId info id2 <> fSpace <> ppImpSpec info impspec



ppTopDecls :: PPInfo a -> Decls a -> Doc
ppTopDecls info (DeclsParse decls) = 
  line <> sep (line <> line) (map (ppDecl info) decls) <> line
ppTopDecls info (DeclsScc decls) = 
  line <> sep (line <> line) (map (ppDeclsDepend info) decls) <> line


ppDecls :: PPInfo a -> Decls a -> Doc
ppDecls info (DeclsParse decls) = 
  group $ sep dSemiSpace (map (ppDecl info) decls)
ppDecls info (DeclsScc decls) = 
  group $ sep dSemiSpace (map (ppDeclsDepend info) decls)


ppDeclsDepend :: PPInfo a -> DeclsDepend a -> Doc
ppDeclsDepend info (DeclsNoRec decl)  =
  text "-- not recursive" <> line <> 
  ppDecl info decl
ppDeclsDepend info (DeclsRec decls) =
  text "-- recursive" <> line <> 
  sep line (map (ppDecl info) decls)


ppType :: PPInfo a -> Type a -> Doc
ppType info = ppTypePrec info False


ppTypePrec :: PPInfo a -> Bool {- with parens? -} -> Type a -> Doc
ppTypePrec info withPar (TypeCons pos t []) = ppId info t
ppTypePrec info withPar (TypeCons pos t ts) = 
  if isFunctionArrow info t 
    then case ts of
      [] -> text "(->)"
      [t1] -> parenExp info pos withPar $ 
                text "(->)" <> ppTypePrec info True t1
      [t1,t2] -> parenExp info pos withPar $
                   ppTypePrec info True t1 <> fSpace <> text "->" <> fSpace <> 
                   ppTypePrec info False t2
    else if isList info t
    then case ts of
      [] -> text "([])"
      [t] -> group $ brackets $ ppTypePrec info False t
    else case maybeTuple info t of
      Just n -> if n == length ts 
                  then groupParens info . sep fComma . 
                    map (ppTypePrec info False) $ ts
                  else  parenExp info pos withPar $
                    (groupParens info . sep fComma . replicate n $ nil) <>
                    fSpace <> (sep fSpace . map (ppTypePrec info True) $ ts)
      Nothing -> parenExp info pos withPar $ 
        sep fSpace (ppId info t : map (ppTypePrec info True) ts)
ppTypePrec info withPar (TypeApp t1 t2) = 
  parenExp info noPos withPar $
    (ppTypePrec info False t1 <> fSpace <> ppTypePrec info True t2)
ppTypePrec info withPar (TypeVar pos id) = ppTyVar info id
ppTypePrec info withPar (TypeStrict pos typ) = 
  text "!" <> ppTypePrec info True typ


ppTypeWithContext :: PPInfo a -> [Context a] -> Type a -> Doc
ppTypeWithContext info ctxs ty =
  group (ppContexts info ctxs <> ppType info ty)


ppSimple :: PPInfo a -> Simple a -> Doc
ppSimple info (Simple pos id ids) = 
  sep fSpace (ppId info id : map (ppId info . snd) ids) 


ppTypeSimple :: PPInfo a -> Simple a -> Doc
ppTypeSimple info (Simple pos id ids) = 
  sep fSpace (ppId info id : map (ppTyVar info . snd) ids) 


ppContexts :: PPInfo a -> [Context a] -> Doc
ppContexts info []  = nil
ppContexts info cxs = 
  parensFComma2 info (map (ppContext info) cxs) <> text " =>" <> fSpace


ppContext :: PPInfo a -> Context a -> Doc
ppContext info (Context pos c vars) =
  ppId info c <> space <> sep fSpace (map (\(_,v)-> ppTyVar info v) vars)


ppDerivings :: PPInfo a -> [(Pos, a)] -> Doc
ppDerivings info [] = nil
ppDerivings info ds  = 
  groupNestS info $
    fSpace <> text "deriving" <>
    fSpace <> parensFComma2 info (map (ppId info . snd) ds) 


ppConstr :: PPInfo a -> Constr a -> Doc
ppConstr info (Constr pos c cs) =
  groupNestS info $
    ppIdAsVar info c <> fSpace <> ppFieldsType info cs
ppConstr info (ConstrCtx forall ctxs pos c cs) =
  groupNestS info $
    ppForall <> ppContexts info ctxs <> 
    ppIdAsVar info c <> fSpace <> ppFieldsType info cs
  where
  ppForall | null forall = nil
           | otherwise   = groupNestS info (text "forall" <> fSpace <> 
                             sep fSpace (map (ppTyVar info . snd) forall) <> 
                             text ".") <> 
                           fSpace


ppFieldsType :: PPInfo a -> [(Maybe [(Pos,a)],Type a)] -> Doc
ppFieldsType info args@((Just _,_):_) = 
  groupNestS info
    (braces . sep fComma . map (ppFieldType info) $ args)
ppFieldsType info args =
  group (sep fSpace . map (ppFieldType info) $ args)


ppFieldType :: PPInfo a -> (Maybe [(Pos, a)], Type a) -> Doc
ppFieldType info (Nothing,typ) = ppTypePrec info True typ
ppFieldType info (Just posidents,typ) = 
  group $
    (sep fComma . map (ppIdAsVar info . snd) $ posidents) <> 
    dSpace <> 
    text ":: " <> ppType info typ


ppDecl :: PPInfo a -> Decl a -> Doc
ppDecl info (DeclType s t) =
  groupNestS info $
    text "type " <> ppTypeSimple info s <> text " =" <> dSpace <> ppType info t
ppDecl info (DeclTypeRenamed pos tyconid) =
  groupNestS info $
    text "type" <> fSpace <> 
    (group . sep fSpace . map text $ 
      (niceInt Nothing state tyconid "" : map snd al)) <>
    text " =" <> dSpace <> text (niceNT Nothing state al nt)
  where
  Just (InfoData _ _ _ newType _) = lookupIS state tyconid
  NewType univQuantTyVars _ _ [nt] = newType
  al = mkAL univQuantTyVars
  state = intState info
ppDecl info (DeclDataPrim pos conid size) =
  groupNestS info $
    text "data primitive" <> fSpace <>  ppPos info pos <> 
    fSpace <> ppId info conid <> text " =" <> dSpace <> text (show size)
ppDecl info (DeclData dk ctxs s cs ds) =
  groupNestS info $
    text sort <> fSpace <> 
    groupNestS info (ppContexts info ctxs <> ppTypeSimple info s) <>
    ppConstrs info cs <> ppDerivings info ds
  where
  sort = case dk of
           Nothing -> "newtype"
           Just False -> "data"
           Just True -> "data unboxed"
ppDecl info (DeclConstrs pos did cs) =
  groupNestS info $
    text "-- data/dataprim [(field,selector)] =" <> dSpace <>
    (groupNestS info $
      ppIdAsVar info did <> 
      (brackets . sep fComma $ 
        map (\(ps,field,sel) -> 
                parens (ppIdAsVar info field <> space <> ppIdAsVar info sel)) 
                       cs))
ppDecl info (DeclClass pos ctxs cls args fundeps decls) =
  nestS info $
    text "class" <> fSpace <>
    groupNestS info (ppContexts info ctxs <> ppId info cls <> space <> 
      sep fSpace (map (ppTyVar info) args) <> ppFunDeps info fundeps)
    <> ppWhere info decls
ppDecl info (DeclInstance pos ctxs tycls insts valdefs) =
  nestS info $
    text "instance" <> fSpace <>
    groupNestS info (ppContexts info ctxs <> ppId info tycls <> space <>
      sep fSpace (map (parens . ppTypePrec info True) insts))
    <> ppWhere info valdefs 
ppDecl info (DeclDefault ts) =
  groupNestS info $
    text "default" <> dSpace <> (parens . sep fComma . map (ppType info) $ ts)
ppDecl info (DeclPrimitive pos ident arity t) =
  groupNestS info $
    ppIdAsVar info ident <> fSpace <> text "primitive" <> fSpace 
    <> text (show arity) <> text " ::" <> fSpace <> ppType info t
ppDecl info (DeclForeignImp pos callConv str ident arity cast t _) =
  groupNestS info $
    text "foreign import" <> fSpace <> text (show callConv) <> 
    fSpace <> string str <> fSpace <>
    text (show cast) <> fSpace <> ppIdAsVar info ident <> text " ::" <> 
    dSpace <> ppType info t
ppDecl info (DeclForeignExp pos callConv str ident t) =
  groupNestS info $
    text "foreign export" <> fSpace <> text (show callConv) <> fSpace <> 
    ppIdAsVar info ident <> text " ::" <> dSpace <> ppType info t
ppDecl info (DeclVarsType ids ctxs t) =
  groupNestS info $
    group (sep fComma (map (ppIdAsVar info . snd) ids)) <> text " ::" <>
    dSpace <> ppTypeWithContext info ctxs t
ppDecl info (DeclPat alt) = group $ ppAlt info "=" alt
ppDecl info (DeclFun pos fun funs) =
  group $
    ppPos info pos <>
    (sep line . map (ppFun info fun) $ funs) 
ppDecl info (DeclIgnore s) =
  text ("{- Ignoring " ++ s ++ " -}")
ppDecl info (DeclError s) =
  text ("ERROR:  " ++ s)
ppDecl info (DeclAnnot decl annots) =
  groupNestS info $ ppDecl info decl <> line <> ppAnnots info annots
ppDecl info (DeclFixity f) =
  ppFixDecl info f


ppAnnots :: PPInfo a -> [Annot a] -> Doc
ppAnnots info = sep line . map (ppAnnot info) 


ppAnnot :: PPInfo a -> Annot a -> Doc
ppAnnot info (AnnotArity (pos,ident) int) =
  text "{-# ARITY " <> ppIdAsVar info ident <> text " = " <> 
  text (show int) <> text "#-}"
ppAnnot info (AnnotPrimitive (pos,ident) prim) = 
  text "{-# PRIMITIVE " <> ppIdAsVar info ident <> text " = " <> 
  text (unpackPS prim) <> text "#-}"
ppAnnot info (AnnotNeed posidents) =
  text "{-# NEED " <> sep fSpace (map ppNeed posidents) <> text "#-}"
  where 
  ppNeed [x] = ppIdAsVar info x
  ppNeed xs  = 
    group $ text "{" <> sep fSpace (map (ppIdAsVar info) xs) <> text "}"
ppAnnot info (AnnotUnknown) =
  text "{-# ??? #-}"

ppFunDeps :: PPInfo a -> [FunDep a] -> Doc
ppFunDeps info [] = text ""
ppFunDeps info xs = text " | " <> sep (text ", ") (map (ppFunDep info) xs)

ppFunDep :: PPInfo a -> FunDep a -> Doc
ppFunDep info (as :->: bs) = ppTyVars as <> text "->" <> ppTyVars bs
  where
  --ppTyVars = parensFComma2 info . map (ppId info)
    ppTyVars = sep fSpace . map (ppId info)


ppWhere :: PPInfo a -> Decls a -> Doc
ppWhere info decls 
  | noDecls decls = nil
  | otherwise = line <> text "where" <> line <> ppTopDecls info decls


ppClassCodes :: PPInfo a -> [ClassCode b a] -> Doc
ppClassCodes info decls = sep line (map (ppClassCode info) decls)


ppClassCode :: PPInfo a -> ClassCode b a -> Doc
ppClassCode info (CodeClass pos cls) =
  groupNestS info $ text "code class" <> dSpace <> ppId info cls
ppClassCode info (CodeInstance pos cls typ arg ecs methods) =
  groupNestS info $ 
    text "code instance" <> fSpace <> ppId info cls <> fSpace <> 
    ppId info typ <> text " ? = ?" <> line <>
    sep dSemiSpace (map (ppIdAsVar info) methods)


ppConstrs :: PPInfo a -> [Constr a] -> Doc
ppConstrs info [] = dSpace <> text "{- no constructors -}"
ppConstrs info cs = 
  text " =" <> fSpace <> 
  group (sep (fSpace <> text "| ") (map (ppConstr info) cs))


ppFun :: PPInfo a -> a -> Fun a -> Doc
ppFun info id (Fun pats rhs w) =
  groupNestS info $
    (group 
      (group $ sep fSpace $ 
        ppIdAsVar info id : map (ppExpPrec info True) pats) <>
      ppRhs info "=" rhs) <>
    ppWhere info w


ppStmts :: PPInfo a -> [Stmt a] -> Doc
ppStmts info stmts = sep line (map (ppStmt info) stmts)


ppStmt :: PPInfo a -> Stmt a -> Doc
ppStmt info (StmtExp exp) = ppExp info exp
ppStmt info (StmtBind pat exp) = 
  groupNestS info $
    ppPat info pat <> dSpace <> text "<- " <> ppExp info exp
ppStmt info (StmtLet ds) = 
  groupNestS info $ text "let" <> dSpace <> ppDecls info ds 


ppPat :: PPInfo a -> Exp a -> Doc
ppPat = ppExp


ppPos :: PPInfo a -> Pos -> Doc 
ppPos info pos = 
  if withPositions info then (braces . text . show $ pos) <> space else nil


parenExp :: PPInfo a -> Pos -> Bool -> Doc -> Doc
parenExp info pos withPar doc =
  groupNestS info $ 
    ppPos info pos <> if withPar then parens doc else doc


ppExp :: PPInfo a -> Exp a -> Doc
ppExp info = ppExpPrec info False


ppExpPrec :: PPInfo a -> Bool -> Exp a -> Doc
ppExpPrec info withPar (ExpLambda pos pats e) =
  parenExp info pos withPar $
    text "\\ " <> sep fSpace (map (ppLambdaPat info) pats) <>
    text " ->" <> dSpace <> ppExpPrec info False e
ppExpPrec info withPar (ExpDo pos stmts) =
  parenExp info pos withPar $
    text "do" <> dSpace <> ppStmts info stmts
ppExpPrec info withPar (ExpLet pos ds e) =
  parenExp info pos withPar $
    text "let" <> dSpace <> ppDecls info ds <> fSpace <> text "in" <> fSpace <>
    parens (ppExpPrec info False e) 
ppExpPrec info withPar (ExpCase pos e alts) =
  parenExp info pos withPar $
    group
      (text "case" <> dSpace <> ppExpPrec info False e <> text " of")
    <> dSpace <>
    sep dSemiSpace (map (ppAlt info "->") alts)
ppExpPrec info _ (ExpFail) =  text "fail"
ppExpPrec info withPar (ExpFatbar e1 e2) =
  parenExp info noPos withPar $
    text "fatbar" <> dSpace <> ppExpPrec info False e1 <> dSpace <> 
    text "-- " <> ppExpPrec info False e2 
ppExpPrec info withPar (ExpIf pos c e1 e2) =
  parenExp info pos withPar $
    text "if " <> ppExpPrec info False c <> dSpace <>
    text "then " <> ppExpPrec info False e1 <> dSpace <>
    text "else " <> ppExpPrec info False e2
ppExpPrec info withPar (ExpType pos e cxs t) =
  parenExp info pos withPar $
    ppExpPrec info False e <> dSpace <> 
    text ":: " <> ppTypeWithContext info cxs t
ppExpPrec info withPar (ExpRecord exp fields) =
  parenExp info noPos withPar $
    ppExpPrec info True exp <> dSpace <> 
    (braces . sep fComma . map (ppField info) $ fields)
ppExpPrec info withPar (ExpApplication pos (ExpCon _ id : args)) 
  | isJust tuple && n == length args =
    ppPos info pos <> 
    (groupParens info . sep fComma . map (ppExpPrec info False) $ args)
  where
  tuple = maybeTuple info id
  Just n = tuple
ppExpPrec info withPar (ExpApplication pos (op : args)) 
  | isJust maybeConOrVarId && idIsOperator info id =
    case args of
      -- assume application consists of at least two expressions
      [arg1] -> groupParens info $
                  ppExpPrec info True arg1 <> dSpace <> ppIdAsOperator info id
      [arg1,arg2] ->  groupParens info $ 
                        ppExpPrec info True arg1 <> dSpace <> 
                        ppIdAsOperator info id <> dSpace <>
                        ppExpPrec info True arg2 
      (arg1:arg2:args) -> ppExpPrec info withPar 
                            (ExpApplication pos 
                              ((ExpApplication noPos [op,arg1,arg2]) : args))
  where
  maybeConOrVarId = case op of
                      ExpCon _ id -> Just id
                      ExpVar _ id -> Just id
                      _           -> Nothing
  id = fromJust maybeConOrVarId
ppExpPrec info withPar (ExpApplication pos exps) =
  parenExp info pos withPar $
    sep fSpace $ map (ppExpPrec info True) exps
ppExpPrec info withPar (ExpInfixList pos exps) =
  parenExp info pos withPar $
    sep fSpace $ map (ppExpPrec info True) exps
ppExpPrec info _ (ExpConOp pos id) = 
  ppPos info pos <> ppIdAsOperator info id
ppExpPrec info _ (ExpVarOp pos id) = 
  ppPos info pos <> ppIdAsOperator info id
ppExpPrec info _ (ExpCon pos id) = 
  ppPos info pos <> 
    case maybeTuple info id of
      Just n -> groupParens info . sep fComma . replicate n $ nil
      Nothing ->  ppIdAsVar info id
ppExpPrec info _ (ExpVar pos id) = 
  ppPos info pos <> ppIdAsVar info id
ppExpPrec info withPar (ExpDict exp) = 
  parenExp info noPos withPar $
    text "{d} " <> ppExpPrec info False exp
ppExpPrec info _ (ExpLit pos lit) = 
  ppPos info pos <> text (show lit)
ppExpPrec info _ (ExpList pos es) =
  groupNestS info $
    ppPos info pos <> (brackets $ sep fComma $ map (ppExpPrec info False) es)
ppExpPrec info _ (Exp2 pos id1 id2) = 
  ppPos info pos <> ppId info id1 <> text "." <> ppIdAsVar info id2
ppExpPrec info withPar (PatAs pos id pat) =
  parenExp info pos withPar $
    ppIdAsVar info id <> text "@" <> ppExpPrec info True pat
ppExpPrec info _ (PatWildcard pos) = 
  ppPos info pos <> text "_"
ppExpPrec info _ (PatIrrefutable pos pat) =
  groupNestS info $ 
    ppPos info pos <> text "~" <> ppExpPrec info True pat
ppExpPrec info withPar (PatNplusK pos n n' k _ _) =
  parenExp info pos withPar $
    ppIdAsVar info n <> fSpace <> text "+" <> fSpace <> ppExpPrec info True k 


ppLambdaPat :: PPInfo a -> Exp a -> Doc
ppLambdaPat info pat@(ExpApplication _ _) = ppExpPrec info True pat
ppLambdaPat info pat                      = ppExpPrec info False pat


ppField :: PPInfo a -> Field a -> Doc
ppField info (FieldExp pos var exp) =
  groupNestS info $
    ppPos info pos <> ppIdAsVar info var <> dSpace <> text "= " <>  
    ppExp info exp
ppField info (FieldPun pos var) =
  ppPos info pos <> ppIdAsVar info var


ppAlt :: PPInfo a -> String -> Alt a -> Doc
ppAlt info delimiter (Alt pat rhs w) =
  groupNestS info $
    group (ppPat info pat <> ppRhs info delimiter rhs) <>
    ppWhere info w


ppRhs :: PPInfo a -> String -> Rhs a -> Doc
ppRhs info delimiter (Unguarded exp) =
  space <> text delimiter <> dSpace <> ppExp info exp
ppRhs info delimiter (Guarded [gd]) =
  dSpace <> ppGdPat info delimiter gd
ppRhs info delimiter (Guarded gds) =
  encase line (map (ppGdPat info delimiter) gds)
   

ppGdPat :: PPInfo a -> String -> (Exp a, Exp a) -> Doc
ppGdPat info deli (e1,e2) =
  groupNestS info $
    text "| " <> ppExp info e1 <> space <> text deli <> dSpace <>
    ppExp info e2


{- various formatting functions for identifiers ---------------------------- -}

ppId :: PPInfo a -> a -> Doc
ppId info id = text (id2str info id)


{-
If the identifier is an operator, then surround it by paranthesis.
-}
ppIdAsVar :: PPInfo a -> a -> Doc
ppIdAsVar info id
  | isOperator name = text ('(' : name ++ ")")
  | otherwise       = text name
  where
  name = id2str info id

{-
if the identifier is a variable or constructor, then surround it by
backquotes.
-}
ppIdAsOperator :: PPInfo a -> a -> Doc
ppIdAsOperator info id 
  | isOperator name = text name
  | otherwise       = text ('`' : name ++ "`")
  where
  name = id2str info id


{-
Not nice: the unique of a type variable does not refer to a symbol table 
entry. So it has to be handled specially.
-}

ppTyVar :: PPInfo a -> a -> Doc
ppTyVar info = text . tyVar2str info


{- little helper functions ------------------------------------------------- -}

{- Test if declaration list empty -}
noDecls :: Decls id -> Bool
noDecls (DeclsParse decls) = null decls
noDecls (DeclsScc decls) = null decls


{- Test if id is an operator -}
idIsOperator :: PPInfo a -> a -> Bool
idIsOperator info id = isOperator (id2str info id)


{- Test if name is an operator -}
isOperator :: String -> Bool  
isOperator = not . isVarChar . last
  where
  isVarChar c = isAlphaNum c || c == '_' || c == '\'' 
                 || c == ']'  -- empty list []
                 || c == '}'  -- traceId


{- End PrettySyntax -------------------------------------------------------- -}
