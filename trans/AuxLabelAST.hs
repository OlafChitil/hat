module AuxLabelAST
  ( auxLabelSyntaxTree
  ) where


import Data.List (nubBy)
import Data.Char (isUpper)
import Data.Maybe (isJust)
import AuxTypes(Environment,mkIdentMap,useIdentMap,useEnvironment
               ,Identifier(..)
               ,AuxiliaryInfo(letBound),InitAuxInfo,initAuxInfo,defaultAuxInfo)
import AuxFile(Visibility,PatSort(Refutable,Irrefutable)
              ,extendEnv,getImports,addPat,foldrPat)
import AuxFixity(fixInfixList)
import TraceId
import Flags(Flags(sDbgTrusted))
import Syntax
import SyntaxUtil (infixFun)
import TokenId (TokenId(..))
import AssocTree
import Extra (Pos,noPos)


-- `auxLabelSyntaxTree' relabels the entire abstract syntax tree,
-- replacing TokenId with TraceId (which basically adds arity and
-- binding information for each variable name).  Along the way, we
-- also need to resolve the fixity of all applications (originally
-- done in a later phase of the compiler).

auxLabelSyntaxTree :: Flags -> Module TokenId 
                   -> IO (Environment,Module TraceId)
auxLabelSyntaxTree flags
	mod@(Module p modId exports imports fixdecls (DeclsParse decls)) =
  do
    let (identMap,_) = mkIdentMap decls
    (importenv,hiddenEnvs) <- 
      getImports (\_->vi) (initAT,identMap) flags imports
    let (irrefutableIds,totalenv) = 
          extendEnv totalenv iai vi importenv 
            (map DeclFixity fixdecls ++ decls)
    let Module p modId exports imports fixdecls decls = 
          relabelModule iai totalenv importenv hiddenEnvs mod
    return $ (totalenv,Module p modId exports imports fixdecls 
                        (addVars iai totalenv irrefutableIds decls))
  where
  iai = initAuxInfo . not . sDbgTrusted $ flags


-- `vi' is a degenerate visibility function.  When writing the .hx file,
-- visibility in the export list is important, but when relabeling the
-- syntax tree, everything is visible.
vi :: Visibility
vi = \_-> True


-- `letVar' and `lookEnv' build a TraceId for any TokenId by
-- looking up the arity information in the environment.  For letVar,
-- the distinction between let-bound and lambda-bound variables
-- is decided by the caller, but for `lookEnv', it is decided by the
-- environment.


letVar :: Environment -> TokenId -> TraceId
letVar env id =
  case useEnvironment env id of
    Just info | letBound info -> id `plus` info
    _ -> error ("AuxLabelAST.letVar: "++show id++" not let-bound in env")

lookEnv :: Environment -> TokenId -> TraceId
lookEnv env id =
  case useEnvironment env id of
    Just info -> id `plus` info
    _ -> case id of
           -- this is a horrible hack to by-pass qualified names.
           -- (only needed for use of current module as qualifier?)
           Qualified _ x -> case useEnvironment env (Visible x) of
                              Just info -> id `plus` info
                              _ -> stop
           _ -> stop
  where
  stop = error ("Variable or constructor not in scope: " ++ show id)

lookupTyCls :: Environment -> TokenId -> TraceId
lookupTyCls (auxTree,_) id =
  case lookupAT auxTree (TypeClass (show id)) of
    Just info -> id `plus` info
    Nothing -> mkTyCon defaultAuxInfo id
               -- type or class not in scope, esp. predefined ones

-- The class `Relabel' walks the abstract syntax tree, relabelling all
-- TokenId to TraceId.  Most instances are pretty trivial - the only
-- interesting ones are at the end of this section.

class Relabel g where
  relabel :: InitAuxInfo -> Environment -> g TokenId -> g TraceId

relabelModule :: InitAuxInfo -> Environment -> Environment -> [Environment] 
              -> Module TokenId -> Module TraceId
relabelModule iai env impEnv hiddenEnvs 
  (Module p mod Nothing imps fixs decls) =
    Module p (mkModule iai mod) Nothing 
      (zipWith (relabelImports iai impEnv) hiddenEnvs imps) []
      (relabel iai env decls)
relabelModule iai env impEnv hiddenEnvs 
  (Module p mod (Just exps) imps fixs decls) =
    Module p (mkModule iai mod) (Just (map (relabel iai env) exps))
      (zipWith (relabelImports iai impEnv) hiddenEnvs imps) [] 
      (relabel iai env decls)
					-- fixdecls are folded into the decls

instance Relabel Export where
  relabel iai env (ExportEntity pos entity) = 
    ExportEntity pos (relabel iai env entity)
  relabel iai env (ExportModid pos id)      = 
    ExportModid pos (mkModule iai id)


relabelImports iai env hiddenEnv (Import (pos,id) impspec) =
  Import (pos,mkModule iai id) (relabelImpSpec iai env hiddenEnv impspec)
relabelImports iai env hiddenEnv (ImportQ (pos,id) impspec) =
  ImportQ (pos,mkModule iai id) (relabelImpSpec iai env hiddenEnv impspec)
relabelImports iai env hiddenEnv (ImportQas (p1,id1) (p2,id2) impspec) =
  ImportQas (p1,mkModule iai id1) (p2,mkModule iai id2) 
    (relabelImpSpec iai env hiddenEnv impspec)
relabelImports iai env hiddenEnv (Importas (p1,id1) (p2,id2) impspec) =
  Importas (p1,mkModule iai id1) (p2,mkModule iai id2) 
    (relabelImpSpec iai env hiddenEnv impspec)

relabelImpSpec iai env hiddenEnv (NoHiding entities) = 
  NoHiding (map (relabel iai env) entities)
relabelImpSpec iai env hiddenEnv (Hiding entities)   = 
  Hiding (map (relabel iai hiddenEnv) entities)

instance Relabel Entity where
  relabel _ env (EntityVar p id) = 
    EntityVar p (lookEnv env id)
  relabel _ env (EntityConClsAll p id) = 
    EntityConClsAll p (lookupTyCls env id)
  relabel _ env (EntityConClsSome p id cons) = 
    EntityConClsSome p (lookupTyCls env id) (relabelRealPosIds env cons)

instance Relabel InfixClass where
  relabel _ env InfixDef = InfixDef
  relabel _ env InfixL = InfixL
  relabel _ env InfixR = InfixR
  relabel _ env Infix  = Infix
  relabel iai env (InfixPre x) = (InfixPre (mkLambdaBound iai x))

instance Relabel FixId where
  relabel _ env (FixCon p i) = FixCon p (lookEnv env i)
  relabel _ env (FixVar p i) = FixVar p (lookEnv env i)

instance Relabel Decls where
  relabel iai env (DeclsParse decls) = DeclsParse (map (relabel iai env) decls)
  relabel iai env (DeclsScc deps)    = DeclsScc (map (relabel iai env) deps)

instance Relabel DeclsDepend where
  relabel iai env (DeclsNoRec decl) = DeclsNoRec (relabel iai env decl)
  relabel iai env (DeclsRec decls)  = DeclsRec (map (relabel iai env) decls)

instance Relabel Decl where
  relabel iai env (DeclType s typ) = 
    DeclType (relabel iai env s) (relabel iai env typ)
  relabel iai env (DeclTypeRenamed p n) = DeclTypeRenamed p n
  relabel iai env (DeclData mb ctxs simp constrs pis) =
    DeclData mb (map (relabel iai env) ctxs) (relabel iai env simp)
      (map (relabel iai env) constrs) (relabelPosIds iai pis)
  relabel iai env (DeclDataPrim p i n) = -- used?
    DeclDataPrim p (mkLambdaBound iai i) n
  relabel iai env (DeclConstrs p i piis) = -- used?
    DeclConstrs p (mkLambdaBound iai i)
      (map (\(p,i1,i2)->(p,mkLambdaBound iai i1,mkLambdaBound iai i2)) piis)
  relabel iai env (DeclClass p ctxs cls vars fundeps decls) =
    DeclClass p (map (relabel iai env) ctxs) (mkClass iai cls)
      (map (mkTyVar iai) vars) (map (relabel iai env) fundeps)
      (relabel iai env decls)
  relabel iai env (DeclInstance p ctxs cls insts decls) =
    DeclInstance p (map (relabel iai env) ctxs) (mkClass iai cls)
      (map (relabel iai env) insts) (relabel iai env decls)
  relabel iai env (DeclDefault typs) =
    DeclDefault (map (relabel iai env) typs)
  relabel iai env (DeclPrimitive p i n typ) = -- used?
    DeclPrimitive p (letVar env i) n (relabel iai env typ)
  relabel iai env (DeclForeignImp p callConv str i1 n fspec typ i2) =
    DeclForeignImp p callConv str (letVar env i1) n fspec (relabel iai env typ)
      (mkLambdaBound iai i2)
  relabel iai env (DeclForeignExp p callConv str id typ) =
    DeclForeignExp p callConv str (mkLambdaBound iai id) (relabel iai env typ)
  relabel iai env (DeclVarsType pis ctxs typ) =
    DeclVarsType (relabelRealPosIds env pis) (map (relabel iai env) ctxs)
      (relabel iai env typ)
  relabel iai env (DeclPat (Alt (ExpInfixList p exps) rhs decls))
    | isJust infixFunExps 
    = relabel iai env (DeclFun pos fun [Fun [e1,e2] rhs decls])
    where
    infixFunExps = infixFun exps
    Just (e1,pos,fun,e2) = infixFunExps
  relabel iai env (DeclPat (Alt pat rhs ds@(DeclsParse decls))) =
    let (irrefutableDeclIds,newEnv) = extendEnv undefined iai vi env decls
    in  DeclPat 
          (Alt (relabel iai newEnv pat) (relabel iai newEnv rhs) 
            (addVars iai newEnv (nubBy (\x y -> show x == show y) -- for scope 
              irrefutableDeclIds) 
              (relabel iai newEnv ds)))
          -- dont' use relable Alt, because that extends environment also
          -- by variables in the pattern
  relabel iai env (DeclFun p f funs) =
    DeclFun p 
      -- ensure that defined id always with arity, even in class/instance
      (modArity (letVar env f) (funArity . head $ funs))
      (map (relabel iai env) funs)
  relabel _ env (DeclIgnore str) = DeclIgnore str
  relabel _ env (DeclError str) = DeclError str
  relabel iai env (DeclAnnot decl annots) =
	DeclAnnot (relabel iai env decl) (map (relabel iai env) annots)
  relabel iai env (DeclFixity (fixclass,n,fixids)) =
	DeclFixity (relabel iai env fixclass, n, map (relabel iai env) fixids)

instance Relabel Annot where
  relabel iai env (AnnotArity (p,id) n) = AnnotArity (p,mkLambdaBound iai id) n
  relabel iai env (AnnotPrimitive (p,id) pstr) = 
    AnnotPrimitive (p,mkLambdaBound iai id) pstr
  relabel iai env (AnnotNeed idss) = 
    AnnotNeed (map (map (mkLambdaBound iai)) idss)
  relabel _ env (AnnotUnknown) = AnnotUnknown

instance Relabel Rhs where
  relabel iai env (Unguarded exp)  = Unguarded (relabel iai env exp)
  relabel iai env (Guarded gdexps) =
    Guarded (map (\(gd,exp)-> (relabel iai env gd, relabel iai env exp)) 
      gdexps)

instance Relabel Type where
  relabel iai env (TypeCons p c typs) = 
    TypeCons p (lookupTyCls env c) (map (relabel iai env) typs)
  relabel iai env (TypeApp t1 t2) = 
    TypeApp (relabel iai env t1) (relabel iai env t2)
  relabel iai env (TypeVar p v)       = 
    TypeVar p (mkTyVar iai v)
  relabel iai env (TypeStrict p t)    = 
    TypeStrict p (relabel iai env t)

instance Relabel Sig where
  relabel iai env (Sig pis typ) = 
    Sig (relabelPosIds iai pis) (relabel iai env typ)

instance Relabel Simple where
  relabel iai env (Simple p id pis) = 
    Simple p (mkTyCon iai id) (relabelPosIds iai pis)

instance Relabel Context where
  relabel iai env (Context p id pis) = 
    Context p (mkClass iai id) (relabelPosIds iai pis)

instance Relabel Constr where
  relabel iai env (Constr p id mbs) =
    Constr p (lookEnv env id) (map locust mbs)
    where 
    locust (Nothing,typ) = (Nothing,relabel iai env typ)
    locust (Just pis,typ) = 
      (Just (relabelRealPosIds env pis),relabel iai env typ)
  relabel iai env (ConstrCtx fvs ctxs p id mbs) =
    ConstrCtx (relabelPosIds iai fvs) (map (relabel iai env) ctxs) p
      (lookEnv env id) (map locust mbs)
      where 
      locust (Nothing,typ) = (Nothing,relabel iai env typ)
      locust (Just pis,typ) = 
        (Just (relabelRealPosIds env pis),relabel iai env typ)

instance Relabel Field where
  relabel iai env (FieldExp p id exp) = 
    FieldExp p (mkField iai id) (relabel iai env exp)
  relabel iai env (FieldPun p id) = FieldPun p (mkField iai id)

instance Relabel Stmt where
  relabel iai env (StmtExp exp)      = StmtExp (relabel iai env exp)
  relabel iai env (StmtBind pat exp) = 
    StmtBind (relabel iai env pat) (relabel iai env exp)
  relabel iai env (StmtLet decls)    = StmtLet (relabel iai env decls)

instance Relabel Qual where
  relabel iai env (QualPatExp pat exp) = 
    QualPatExp (relabel iai env pat) (relabel iai env exp)
  relabel iai env (QualExp exp) = QualExp (relabel iai env exp)
  relabel iai env (QualLet decls) = QualLet (relabel iai env decls)

instance Relabel FunDep where
  relabel iai env (as :->: bs) =
        (map (mkTyVar iai) as) :->: (map (mkTyVar iai) bs)


-- This is where the non-trivial syntax types start.  Local defns (let-bound)
-- and lhs patterns (lambda-bound) must be added to the environment before
-- relabeling the rhs of a function or binding.

instance Relabel Fun where
  relabel iai env (Fun pats rhs ds@(DeclsParse decls)) =
    let (irrefutableDeclIds,declEnv) = extendEnv undefined iai vi env decls
        (irrefutableIds,newEnv) = 
          foldrPat (addPat iai Refutable vi) declEnv pats 
    in
      Fun (map (relabel iai newEnv) pats) (relabel iai newEnv rhs) 
        (addVars iai newEnv 
          (nubBy (\x y -> show x == show y) -- for shadowing of scopes
            (irrefutableDeclIds++irrefutableIds)) 
          (relabel iai newEnv ds))

instance Relabel Alt where
  -- only used for Alts in cases, not in pattern bindings
  relabel iai env (Alt pat rhs ds@(DeclsParse decls)) =
    let (irrefutableDeclIds,declEnv) = extendEnv undefined iai vi env decls
        (irrefutableIds,newEnv) = 
          addPat iai Refutable vi pat declEnv
    in
      Alt (relabel iai newEnv pat) (relabel iai newEnv rhs) 
        (addVars iai newEnv (nubBy (\x y -> show x == show y) -- for scope 
          (irrefutableDeclIds++irrefutableIds)) 
        (relabel iai newEnv ds))

instance Relabel Exp where
  relabel iai env (ExpScc str exp) = ExpScc str (relabel iai env exp)
  relabel iai env (ExpDict exp) = ExpDict (relabel iai env exp)
  relabel iai env (ExpFatbar e1 e2) = 
    ExpFatbar (relabel iai env e1) (relabel iai env e2)
  relabel iai env (ExpFail) = ExpFail
  relabel iai env (ExpLambda p pats exp) =
	let (irrefutableIds,newEnv) = 
              foldrPat (addPat iai Refutable vi) env pats in
	ExpLambda p (map (relabel iai newEnv) pats) 
          ((if null irrefutableIds 
             then id else ExpLet p (addVars iai newEnv irrefutableIds noDecls))
            (relabel iai newEnv exp))
  relabel iai env (ExpLet p ds@(DeclsParse decls) exp) =
	let (irrefutableIds,newEnv) = extendEnv undefined iai vi env decls in
	ExpLet p (addVars iai newEnv irrefutableIds (relabel iai newEnv ds)) 
          (relabel iai newEnv exp)
  relabel iai env (ExpDo p stmts) =
	ExpDo p (doStmts env stmts)
    where doStmts env [] = []
	  doStmts env (s@(StmtExp _):ss) = relabel iai env s: doStmts env ss
	  doStmts env (s@(StmtBind pat _):ss) =
		let (irrefutableIds,newEnv) = addPat iai Refutable vi pat env 
                in
		relabel iai newEnv s : 
                (if null irrefutableIds 
                   then id 
                   else (StmtLet (addVars iai newEnv irrefutableIds noDecls):))
                  (doStmts newEnv ss)
	  doStmts env (s@(StmtLet (DeclsParse decls)):ss) =
		let (irrefutableIds,newEnv) = 
                      extendEnv undefined iai vi env decls 
                    StmtLet reDecls = relabel iai newEnv s
                in (StmtLet (addVars iai newEnv irrefutableIds reDecls) 
                   : doStmts newEnv ss)
  relabel iai env (ExpCase p exp alts) =
    ExpCase p (relabel iai env exp) (map (relabel iai env) alts)
  relabel iai env (ExpIf p cond thn els) =
    ExpIf p (relabel iai env cond) (relabel iai env thn) (relabel iai env els)
  relabel iai env (ExpType p exp ctxs typ) =
    ExpType p (relabel iai env exp) (map (relabel iai env) ctxs) 
      (relabel iai env typ)
  relabel iai env (ExpRecord exp fields) =
    ExpRecord (relabel iai env exp) (map (relabel iai env) fields)
  relabel iai env (ExpApplication p exps) =
    ExpApplication p (map (relabel iai env) exps)
  relabel _ env (ExpVar p id) = ExpVar p (lookEnv env id)
  relabel _ env (ExpCon p id)   = ExpCon p (lookEnv env id)
  relabel _ env (ExpVarOp p id) = ExpVarOp p (lookEnv env id)
  relabel _ env (ExpConOp p id) = ExpConOp p (lookEnv env id)
  relabel iai env (ExpInfixList p exps) = 
    relabel iai env (fixInfixList env exps)
  relabel _ env (ExpLit p lit)   = ExpLit p lit
  relabel iai env (ExpList p exps) = ExpList p (map (relabel iai env) exps)
  relabel iai env (PatAs p id pat) =
    PatAs p (lookEnv env id) (relabel iai env pat)
  relabel _ env (PatWildcard p)  = PatWildcard p
  relabel iai env (PatIrrefutable p pat) = 
    PatIrrefutable p (relabel iai env pat)
  relabel iai env (PatNplusK p id1 id2 exp1 exp2 exp3) = -- *** No, No, NO
    PatNplusK p (mkLambdaBound iai id1) (mkLambdaBound iai id2)
      (relabel iai env exp1) (relabel iai env exp2) (relabel iai env exp3)


-- `relabelPosIds' does the simplest possible renaming of a list of
-- position/id pairs from the TokenId type to TraceId type.
relabelPosIds :: InitAuxInfo -> [(Pos,TokenId)] -> [(Pos,TraceId)]
relabelPosIds iai = map (\(p,i)->(p,mkLambdaBound iai i))

-- `relabelRealPosIds' does the correct (as opposed to simplest) renaming
-- of a list of position/id pairs from the TokenId type to TraceId type.
relabelRealPosIds :: Environment -> [(Pos,TokenId)] -> [(Pos,TraceId)]
relabelRealPosIds env = map (\(p,i)->(p,lookEnv env i))

-- Create definitions for let-bound variants of irrefutable variables.
addVars :: InitAuxInfo -> Environment -> [TokenId] -> Decls TraceId 
        -> Decls TraceId
addVars iai env irrefutableIds (DeclsParse ds) =
  DeclsParse (map mkDef irrefutableIds ++ ds)
  where
  mkDef id = 
    DeclFun noPos traceId
      [Fun [] 
        (Unguarded (ExpVar noPos (modLambdaBound iai traceId))) noDecls]
    where  
    traceId = lookEnv env id
