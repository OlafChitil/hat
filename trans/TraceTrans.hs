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

module TraceTrans (traceTrans,maybeStripOffQual) where

import Syntax
import SyntaxPos (HasPos(getPos))
import TokenId (TokenId(TupleId,Visible,Qualified)
               ,mkUnqualifiedTokenId,isTidCon,visImport
               ,qualify,visible,extractV,extractM,forceM,dropM
               ,tPrelude,t_Tuple,t_Arrow,tTrue,tFalse,t_otherwise,t_undef
               ,tMain,tmain,tseq,t_ColonNQ,t_ListNQ,tHatHack)
import TraceDerive (derive)
import SysDeps (PackedString,packString,unpackPS)
import Extra (Pos,noPos,mergePos,strPos,fromPos,mapListSnd,mapSnd)
import TraceId (TraceId,tokenId,arity,isLambdaBound,isTraced
               ,fixPriority,mkLambdaBound,mkTyCon,mkField
               ,getUnqualified,modLetBound,hasValueInfo,tyClsInfo,typeSynonymBody
               ,isExpandableTypeSynonym,TyCls(Ty,Cls,Syn)
               ,tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
               ,tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenMinus
	       ,tTokenFun)
import AuxTypes (AuxiliaryInfo,TySynBody(..),possTuple
                ,InitAuxInfo,defaultAuxInfo,initAuxInfo) 
import Data.List (isPrefixOf,union,partition,nubBy,delete)
import Data.Char (isAlpha,digitToInt)
import Data.Ratio (numerator,denominator)
import Data.Maybe (fromJust,catMaybes,isNothing,isJust)

-- import Extra (strace)

infixr 6 `typeFun`	-- hbc won't let me declare this later.

type Arity = Int

data Scope = Global | Local deriving Eq

isLocal :: Scope -> Bool
isLocal Local = True
isLocal Global = False


-- ----------------------------------------------------------------------------
-- Transform a module

traceTrans :: Bool    -- transform for tracing/suspect (not non-tracing/trusted)
           -> String  -- filename of module 
           -> String  -- base filename for the trace files (without extension)
           -> Module TraceId -> Module TokenId
traceTrans traced filename traceFilename 
 (Module pos modId exps impDecls fixDecls decls) =
  Module pos
    modId'
    (if isMain modId then Just [] {- export everything -} 
                     else tExports traced exps 
                            (decls'++conNameDefs++globalVarNameDefs) modId')
    (tImpDecls traced modId impDecls)
    [] -- no fix info needed, because pretty printed output not ambiguous
    (DeclsParse 
      (decls' 
       ++ [defNameMod pos modId filename traced]
       ++ conNameDefs 
       ++ globalVarNameDefs
       ++ map (defNameVar Global Local modTrace) mvars
       ++ map (defNameVar Local Local modTrace) vars 
       ++ (if traced then map (defNamePos modTrace) poss else [])
       ++ if isMain modId then [defMain traced traceFilename] else [] ))
  where
  conNameDefs = map (defNameCon modTrace) cons 
  globalVarNameDefs = map (defNameVar Global Global modTrace) tvars 
  modId' = nameTransModule modId
  modTrace = ExpVar pos (nameTraceInfoModule modId)
  (poss,tvars,vars,mvars,cons) = getModuleConsts consts
  (DeclsParse decls',consts) = tDecls Global traced (mkRoot pos) decls
  

-- ----------------------------------------------------------------------------
-- If module is part of the given module (Prelude or TPrelude),
-- then strip off all of these module qualifications from identifiers.
-- These qualified identifiers are introduced by desugaring in the parser
-- and the fixity correction.

maybeStripOffQual :: String -> Module TokenId -> Module TokenId
maybeStripOffQual p mod@(Module pos modId exps impDecls fixDecls decls) =
  if isPreModule p modId then fmap (stripModule p) mod else mod

isPreModule :: String -> TokenId -> Bool
isPreModule p = (p `isPrefixOf`) . reverse . unpackPS . extractV 

stripModule :: String -> TokenId -> TokenId
stripModule qual' (Qualified qual unqual) | qual' == qualModule qual =
  Visible unqual
stripModule qual token = token

qualModule :: PackedString -> String
qualModule = reverse . unpackPS
          -- reverse . takeWhile (/= '.') . unpackPS

-- ----------------------------------------------------------------------------
-- construct new main function definition

-- main = T.traceIO "artFilename" gmain
-- main = do
--  T.openTrace "artFilename"
--  case omain Prelude.undefined Prelude.undefined of
--    T.R v _ -> v
--  T.closeTrace
defMain :: Bool -> String -> Decl TokenId

defMain traced artFilename =
  DeclFun noPos tokenmain 
    [Fun [] (Unguarded 
      (ExpApplication noPos 
        [ExpVar noPos tokenTraceIO
        ,ExpLit noPos (LitString Boxed artFilename)
        ,ExpApplication noPos 
          [ExpVar noPos tokengmain,mkSRExp noPos False,mkRoot noPos]]))
      noDecls]
  where
  tokenmain = visible (reverse "main")
  tokengmain = nameTransLetVar (mkLambdaBound (initAuxInfo traced) tmain)

-- ----------------------------------------------------------------------------
-- Transform imports and exports

makeExport :: Decl TokenId -> [Export TokenId]
makeExport (DeclType (Simple _ id _) _) = 
  [ExportEntity noPos (EntityConClsSome noPos id [])]
makeExport (DeclData _ _ (Simple _ id _) _ _) =
  [ExportEntity noPos (EntityConClsAll noPos id)]
makeExport (DeclClass _ _ id _ _ _) =   
  [ExportEntity noPos (EntityConClsAll noPos id)]
makeExport (DeclFun _ id _) 
  | (head (getUnqual id)) `elem` ['g','!','a','+','h','*'] =
  -- only these functions shall be exported 
  [ExportEntity noPos (EntityVar noPos id)]
  where
  getUnqual = reverse . unpackPS . extractV
makeExport _ = []  

tExports :: Bool -> Maybe [Export TraceId] -> [Decl TokenId] -> TokenId 
         -> Maybe [Export TokenId]
tExports _ Nothing _ _ = Nothing
tExports _ (Just []) decls _ = Just (concatMap makeExport decls)
tExports traced (Just exports) decls thisModId =  
  Just (concatMap tExport exports)
  where
  tExport :: Export TraceId -> [Export TokenId]
  tExport (ExportModid pos modId) =
    if modId' == thisModId 
      then concatMap makeExport decls
      else [ExportModid pos modId']
    where
    modId' = nameTransModule modId
  tExport (ExportEntity pos entity) = 
    map (ExportEntity pos) (tEntity traced entity)


tImpDecls :: Bool -> TraceId -> [ImpDecl TraceId] -> [ImpDecl TokenId]
tImpDecls traced modId decls = 
    ImportQ (noPos,tPrelude) (Hiding [])
    -- ^ import original Prelude qualified
    --   actually should hide Prelude as far as possible using
    --   ImportQ (noPos,tPrelude) (NoHiding [])
    --   but nhc98 needs access to original Prelude for desugaring
    :Import (noPos,tHatHack) (Hiding [])	-- for list syntax : and []
    :ImportQas (noPos,Visible tracingModule) 
       (noPos,Visible tracingModuleShort) (Hiding [])
    :Import (noPos,Visible tracingModule) (NoHiding [EntityConClsSome noPos
                          (visImport "WrapVal") [(noPos, visImport "wrapVal")]])
    -- ^ hack for nhc98 <= 1.16
    :map (tImpDecl traced) decls

tImpDecl :: Bool -> ImpDecl TraceId -> ImpDecl TokenId
tImpDecl traced (Import (pos,id) spec) = 
  Import (pos,nameTransModule id) (tImpSpec traced spec)
tImpDecl traced (ImportQ (pos,id) spec) =
  ImportQ (pos,nameTransModule id) (tImpSpec traced spec)
tImpDecl traced (ImportQas (pos1,id1) (pos2,id2) spec) =
  ImportQas (pos1,nameTransModule id1) (pos2,nameTransModule id2) 
    (tImpSpec traced spec)
tImpDecl traced (Importas (pos1,id1) (pos2,id2) spec) =
  Importas (pos1,nameTransModule id1) (pos2,nameTransModule id2) 
    (tImpSpec traced spec)

tImpSpec :: Bool -> ImpSpec TraceId -> ImpSpec TokenId
tImpSpec traced (NoHiding entities) = 
  NoHiding (concatMap (tEntity traced) entities)
tImpSpec traced (Hiding entities)   = 
  Hiding (concatMap (tEntity traced) entities)

tEntity :: Bool -> Entity TraceId -> [Entity TokenId]
tEntity _ (EntityVar pos id) = 
  EntityVar pos (nameTransLetVar id) 
  : case arity id of
      Just a | a > 0 -> [EntityVar pos (nameTraceInfoGlobalVar id)
                        ,EntityVar pos (nameWorker id)]
      Just (-1)      -> [EntityVar pos (nameShare id)]
      _              -> []
tEntity traced (EntityConClsAll pos id) | hasValueInfo id = []
tEntity traced (EntityConClsAll pos id) =
  case tyClsInfo id of
    Ty cons labels -> (EntityConClsSome pos (nameTransTyConCls id)
                        (map ((,) pos . nameTransCon) consIds
                         ++ map ((,) pos . nameTransField) labelIds))
                      : map (EntityVar pos . nameTraceInfoCon) consIds
                      ++ map (EntityVar pos . nameTransLetVar) labelIds
                      ++ map (EntityVar pos . nameWorker) labelIds
                      ++ map (EntityVar pos . nameTraceInfoGlobalVar) labelIds
      where
      consIds = map (mkTyCon iai . possTuple) cons
      labelIds = map (mkField iai . visImport) labels
      iai = initAuxInfo traced 
    Syn helpers _ -> [EntityConClsSome pos (nameTransTyConCls id) []] ++
                       map (flip (EntityConClsSome pos) [] . 
                            nameTransTySynHelper id) 
                         [1..helpers] 
    -- Syn _ _ -> error ("tEntity: synonym with all entities: "++show (tokenId id))
    Cls methods -> [EntityConClsAll pos (nameTransTyConCls id)]
tEntity _ (EntityConClsSome pos id posIds) | hasValueInfo id = []
tEntity _ (EntityConClsSome pos id posIds) =
  case tyClsInfo id of
    Ty _ _ -> (EntityConClsSome pos (nameTransTyConCls id)
                (mapListSnd nameTransCon pCons
                 ++ mapListSnd nameTransField pFields))
             : map (\(pos,id) -> EntityVar pos (nameTransLetVar id)) pFields
             ++ map (\(pos,id) -> EntityVar pos (nameWorker id)) pFields
             ++ map (\(pos,id) -> EntityVar pos (nameTraceInfoGlobalVar id)) 
                  pFields
             ++ map (\(pos,id) -> EntityVar pos (nameTraceInfoCon id)) pCons
    Syn helpers _ -> [EntityConClsSome pos (nameTransTyConCls id) []] ++
                       map (flip (EntityConClsSome pos) [] . 
                            nameTransTySynHelper id) 
                         [1..helpers] 
    Cls _ -> [EntityConClsSome pos (nameTransTyConCls id) 
               (mapListSnd nameTransLetVar posIds 
                 ++ mapListSnd nameShare posIds)]
  where
  (pCons,pFields)   = partition (isTidCon.tokenId.snd) posIds

-- ----------------------------------------------------------------------------
-- New top-level definitions for generating shared trace info
-- 
-- Trace info for positions and identifier information. They have to be 
-- top-level, so that they (and their side-effect) are only evaluated once.
-- INCOMPLETE: an optimising compiler may need noinline pragma. 
-- The variables referring to variable information need to include the 
-- position in the name, because the same variable name may be used several 
-- times.

defNameMod :: Pos -> TraceId -> String -> Bool -> Decl TokenId
defNameMod pos id filename traced =
  DeclFun pos (nameTraceInfoModule id) 
    [Fun [] 
      (Unguarded 
        (ExpApplication pos 
          [ExpVar pos tokenMkModule
          ,ExpLit pos (LitString Boxed (fixPrelude (getUnqualified id)))
          ,ExpLit pos (LitString Boxed filename)
          ,ExpCon pos (if traced then tTrue else tFalse)])) 
      noDecls]
  where
    fixPrelude mod | "Prelude" `isPrefixOf` mod = "Prelude"
                   | otherwise                  = mod

defNameCon :: Exp TokenId -> (Pos,TraceId,[TraceId]) -> Decl TokenId
defNameCon modTrace (pos,id,labels) =
  DeclFun pos (nameTraceInfoCon id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          (ExpVar pos (tokenMkAtomConstructor withLabels)
          :modTrace
          :encodePos pos ++
           ExpLit pos (LitInt Boxed (fixPriority id))
          :ExpLit pos (LitInt Boxed (fromJust (arity id)))
          :ExpLit pos (LitString Boxed (getUnqualified id))
          :if withLabels
             then (:[]) . mkList pos . 
                    map (ExpVar pos . nameTraceInfoVar pos Global) $
                    labels
             else []
          )))
      noDecls]
  where
  withLabels = not (null labels)

defNameVar :: Scope -> Scope -> Exp TokenId -> (Pos,TraceId) -> Decl TokenId
defNameVar defScope visScope refMod (pos,id) =
  DeclFun pos (nameTraceInfoVar pos visScope id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          ( ExpVar pos tokenMkAtomVariable
          : refMod
          : encodePos pos ++
          [ExpLit pos (LitInt Boxed (fixPriority id))
           -- all identifiers in definition position are assumed to 
           -- be equipped with an arity; 
           -- only those defined by pattern bindings do not; they have arity 0.
          ,ExpLit pos (LitInt Boxed 
            (case (arity id) of 
              Just a -> a
              Nothing -> 0))
          ,ExpLit pos (LitString Boxed (getUnqualified id))
          ,ExpCon pos (if isLocal defScope then tTrue else tFalse)])))
      noDecls]

defNamePos :: Exp TokenId -> Pos -> Decl TokenId
defNamePos modTrace pos =
  DeclFun pos (nameTraceInfoPos pos)
    [Fun []
      (Unguarded
        (ExpApplication pos
          ( ExpVar pos tokenMkPos
          : modTrace
          : encodePos pos)))
      noDecls]

encodePos :: Pos -> [Exp TokenId]
-- encoding of positions in trace file
encodePos pos =
  [ExpLit pos (LitInt Boxed (10000*beginRow + beginCol))
  ,ExpLit pos (LitInt Boxed (10000*endRow + endCol))]
  where 
  (beginRow,beginCol,endRow,endCol) = fromPos pos

-- ----------------------------------------------------------------------------
-- abstract data type 
-- implements sets of positions, defined this-level and local variables, 
-- defined methods and defined constructors (no duplicates)
-- this-level means defined on the currently considered declaration level,
-- local means defined in some declaration local to the current declaration.
-- variables and constructors come with the position at which they are defined
-- precondition: a constructor with position is only added once
-- a variable with position may be added several times, because
-- position may be zero 
-- because same position may be used for a variable, an application etc,
-- a position may be added several times.
-- The scope states if the variable is defined globally or locally;

data ModuleConsts = 
  MC [Pos]  -- positions used in traces
    [(Pos,TraceId)]  -- this-level variable ids for traces
    [(Pos,TraceId)]  -- variable ids for use in traces
    [(Pos,TraceId)]  -- ids for methods for use in trace
    [(Pos,TraceId,[TraceId])]  -- constructor ids for use in traces
                               -- together with field labels (global)

emptyModuleConsts :: ModuleConsts
emptyModuleConsts = MC [] [] [] [] []

addPos :: Pos -> ModuleConsts -> ModuleConsts
addPos pos (MC poss tids ids mids cons) = 
  MC (pos `insert` poss) tids ids mids cons

addVar :: Pos -> TraceId -> ModuleConsts -> ModuleConsts
addVar pos id (MC poss tids ids mids cons) = 
  MC (pos `insert` poss) ((pos,id) `insert` tids) ids mids cons

addCon :: Pos -> TraceId -> [TraceId] -> ModuleConsts -> ModuleConsts
addCon pos id labels (MC poss tids ids mids cons) =
  MC (pos `insert` poss) tids ids mids ((pos,id,labels) : cons)

-- reclassify this-level variables as methods
classifyMethods :: ModuleConsts -> ModuleConsts
classifyMethods (MC poss tids ids [] cons) = MC poss [] ids tids cons

-- both from the same declaration level
merge :: ModuleConsts -> ModuleConsts -> ModuleConsts
merge (MC poss1 tids1 ids1 mids1 cons1) (MC poss2 tids2 ids2 mids2 cons2) = 
  MC (poss1 `union` poss2) (tids1 `union` tids2) (ids1 `union` ids2) 
    (mids1 `union` mids2) (cons1 ++ cons2)

-- combine this declaration level with a local declaration level
withLocal :: ModuleConsts -> ModuleConsts -> ModuleConsts
withLocal (MC poss1 tids1 ids1 mids1 cons1) (MC poss2 tids2 ids2 [] []) =
  MC (poss1 `union` poss2) tids1 (ids1 `union` tids2 `union` ids2) mids1 cons1
withLocal _ _ = 
  error "TraceTrans.withLocal: locally defined data constructors or method"

getModuleConsts :: ModuleConsts 
                -> ([Pos],[(Pos,TraceId)],[(Pos,TraceId)]
                   ,[(Pos,TraceId)],[(Pos,TraceId,[TraceId])])
getModuleConsts (MC pos tids ids mids cons) = (pos,tids,ids,mids,cons)

-- avoid duplicate
insert :: Eq a => a -> [a] -> [a] 
insert p ps = p : delete p ps

-- ----------------------------------------------------------------------------
-- Transformation of declarations, expressions etc.

-- the input of this transformation has a DeclFun for each definition equation
-- the following function combines equations for a variable into one DeclFun
combineFuns :: [Decl TraceId] -> [Decl TraceId]
combineFuns (decl@(DeclFun pos id [fun]) : decls) =
  case combineFuns decls of
    (DeclFun pos2 id2 fun2s : decls') | tokenId id == tokenId id2
       -> DeclFun (mergePos pos pos2) id (fun:fun2s) : decls'
    xs -> decl : xs
combineFuns (decl : decls) = decl : combineFuns decls
combineFuns [] = []

tDecls :: Scope -> Bool -> Exp TokenId -> Decls TraceId 
       -> (Decls TokenId,ModuleConsts)
tDecls scope traced parent (DeclsParse decls) = (DeclsParse decls',declsConsts)
  where
  (decls',declsConsts) = 
    foldr combine ([],emptyModuleConsts) . map (tDecl scope traced parent) 
    . combineFuns $ decls
  combine :: ([Decl id],[Decl id],ModuleConsts) -> ([Decl id],ModuleConsts)
          -> ([Decl id],ModuleConsts)
  combine (ds11,ds12,c1) (ds,c2) = (ds11++ds12++ds,c1 `merge` c2)

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

-- Division of result declarations into two lists seems to be rather
-- pointless (needed in older version); 
-- could just return a single list instead.
tDecl :: Scope -> Bool -> Exp TokenId -> Decl TraceId 
      -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tDecl _ _ _ synDecl@(DeclType lhsTy rhsTy) = 
  (map tTypeSynonym (splitSynonym synDecl),[],emptyModuleConsts)
  where
  tTypeSynonym :: Decl TraceId -> Decl TokenId
  tTypeSynonym (DeclType lhsTy rhsTy) = DeclType (tSimple lhsTy) (tType rhsTy)
  splitSynonym :: Decl TraceId -> [Decl TraceId]
  splitSynonym d@(DeclType (Simple pos tySyn tyVars) rhs) =
    d : zipWith mkDeclType (hrhss rhs) [1..]
    where
    mkDeclType hrhs no = 
      DeclType (Simple pos (mkTyCon defaultAuxInfo 
        		     (nameTransTySynHelper tySyn no)) tyVars) hrhs
    hrhss rhs = case rhs of 
             (TypeCons _ tyCon tys) 
               | isFunTyCon tyCon -> go rhs
               | isExpandableTypeSynonym tyCon -> 
                   hrhss (expandTypeSynonym tyCon tys)
             (TypeApp ty1 ty2) -> go rhs
             _ -> [] -- nothing to split off
    -- it is vital that this `go' agrees with the `go' in `splitSynonym' in
    -- AuxFile. Sadly the module structure of Hat is such that the two
    -- functions cannot sit next to each other (or be combined) without
    -- introducing a separate module for them.
    go :: Type TraceId -> [Type TraceId]
    go (TypeCons _ tyCon tys) 
      | isFunTyCon tyCon = case tys of
                             [] -> []
                             [ty] -> [ty]
                             [ty1,ty2] -> ty1 : (go ty2)
      | isExpandableTypeSynonym tyCon = go (expandTypeSynonym tyCon tys)
    go (TypeApp ty1 ty2) = go ty1 ++ go ty2
    go (TypeVar _ _) = []
    go ty = [ty]
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
  -- consisting of function types,type applications and type variables, 
  -- which can and may need to be expanded, and several inner type parts,
  -- for which new helper type synonyms are defined. These are always
  -- ex- and imported with the type synonym itself.
  -- A lot of effort, but it does work in the end.
tDecl Global traced _ (DeclData sort contexts lhsTy constrs pClss) = 
  ([DeclData sort (tContexts contexts) (tSimple lhsTy) 
    (map tConstr constrs) []] 
    -- "derive" should be empty, because transformed classes cannot be derived
  ,instDecl:fieldSelectorDecls++deriveDecls
  ,foldr addConInfo (fieldSelectorConsts `merge` deriveConsts) constrs)
  where
  (DeclsParse deriveDecls,deriveConsts) = 
     tDecls Global False (mkRoot noPos) 
       (DeclsParse (derive contexts lhsTy constrs pClss))
  instDecl = wrapValInstDecl traced (getPos lhsTy) contexts lhsTy constrs
  (fieldSelectorDecls,fieldSelectorConsts) = mkFieldSelectors constrs
  addConInfo :: Constr TraceId -> ModuleConsts -> ModuleConsts
  addConInfo constr = 
    addCon (getPos constr) (getConstrId constr) 
      (map snd (getConstrLabels constr))
tDecl _ _ _ (DeclDataPrim pos id size) = 
  error ("Cannot trace primitive data type (" ++ show (tokenId id) 
    ++ " at position " ++ strPos pos ++ ")")
tDecl _ traced parent (DeclClass pos contexts clsId tyIds fundeps decls) = 
  ([DeclClass pos (tContexts contexts) (nameTransTyConCls clsId) 
     (map nameTransTyVar tyIds) (map (fmap nameTransTyVar) fundeps) decls1]
  ,decls2  -- auxiliary definitions have to be outside the class definition
  ,classifyMethods declsConsts)
  where
  (decls1,decls2,declsConsts) = tDecls2 id traced parent decls
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


-- constructor definition in type definition
tConstr :: Constr TraceId -> Constr TokenId
tConstr (Constr pos conId tyArgs) =
  Constr pos (nameTransCon conId) (tTyArgs tyArgs)
tConstr (ConstrCtx tyVars contexts pos conId tyArgs) =
  ConstrCtx (tPosTyVars tyVars) (tContexts contexts) 
    pos (nameTransCon conId) (tTyArgs tyArgs)


-- build the instance of class WrapVal for type with given data constructors
-- this instance is needed for constructing and updating with labelled fields
wrapValInstDecl :: Bool -> Pos -> [Context TraceId] -> Simple TraceId 
                -> [Constr TraceId] -> Decl TokenId
wrapValInstDecl traced pos contexts ty constrs =
  DeclInstance pos (map (fmap tokenId) contexts) tokenWrapValClass 
    [(fmap tokenId (simpleToType ty))] 
    (DeclsParse [DeclFun pos (dropM tokenWrapValFun) (map wrapValFun constrs)])
  where
  traceTokenWrapValFun = 
    mkLambdaBound (initAuxInfo traced) (dropM tokenWrapValFun)
  sr = ExpVar pos (nameSR traceTokenWrapValFun)
  parent = mkParentVar pos
  varId = nameTrace2 traceTokenWrapValFun -- actually not a trace
  var = ExpVar pos varId
  infiniteTraces = map (ExpVar pos) . nameArgs $ traceTokenWrapValFun
  wrapValFun :: Constr TraceId -> Fun TokenId
  wrapValFun constr =
    Fun [sr,PatAs pos varId consApp,parent] 
      (Unguarded (wrapExp pos var consAppTrace)) noDecls
    where
    consAppTrace = 
      if numOfArgs == 0 
        then ExpApplication pos
               [ExpVar pos tokenMkExpValueUse,parent,sr,funAtom]
        else ExpApplication pos .
               (ExpVar pos (tokenMkExpValueApp numOfArgs) :) . (parent :) .
                (sr :) . (funAtom :) $ traces 
    funAtom = ExpVar pos (nameTraceInfoCon consId)
    consApp =
      if numOfArgs == 0 then ExpCon pos (tokenId consId)
        else ExpApplication pos . (ExpCon pos (tokenId consId) :) .
               map (wrapExp pos (PatWildcard pos)) $ traces
    consId = getConstrId constr
    traces = take numOfArgs infiniteTraces :: [Exp TokenId]
    numOfArgs = sum . map (repeated . fst) $ constrArgs
    repeated Nothing = 1
    repeated (Just labels) = length labels
    constrArgs :: [(Maybe [(Pos,TraceId)],Type TraceId)]
    constrArgs = getConstrArgumentList constr


tHaskellPrimitive :: Pos -> TokenId -> TraceId -> Arity -> Type TraceId 
                  -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tHaskellPrimitive pos hasId fnId arity ty 
  -- import of a Haskell function
  -- used for defining builtin Haskell functions
  -- transformation yields a wrapper to the untransformed function
  | arity == 0 =
    ( [ DeclVarsType [(pos,nameTransLetVar fnId)] [] (tFunType ty)
      , DeclFun pos (nameTransLetVar fnId)
         [ Fun [sr,parent] 
               (Unguarded (ExpApplication pos [combConstUse pos False,sr
                                              ,parent,ExpVar pos shareId]))
               noDecls ] ]
    , [DeclFun pos shareId
       [Fun []
         (Unguarded (ExpApplication pos 
           [combConstDef pos False,mkRoot pos
           ,ExpVar pos (nameTraceInfoVar pos Global fnId)
           ,ExpLambda pos [parent] 
             (ExpApplication pos 
               [expFrom pos ty,parent, ExpVar pos hasId])]))
         noDecls]]
    , addVar pos fnId emptyModuleConsts)
  | otherwise =
    ([DeclVarsType [(pos,nameTransLetVar fnId)] [] (tFunType ty)
     ,DeclFun pos (nameTransLetVar fnId) 
       [Fun [sr,parent]
         (Unguarded 
           (ExpApplication pos 
             [combFun pos False arity
             ,ExpVar pos (nameTraceInfoVar pos Global fnId)
             ,sr,parent,ExpVar pos wrappedId']))
          noDecls]]
    ,[DeclFun pos wrappedId' 
       [Fun (args++[hidden])
         (Unguarded (ExpApplication pos
           [expFrom pos tyRes,hidden
           ,ExpApplication pos (ExpVar pos hasId : zipWith to tyArgs args)]))
         noDecls]]
    ,addVar pos fnId emptyModuleConsts)
    where
    parent = mkParentVar pos
    sr = ExpVar pos (nameSR fnId)
    hidden = ExpVar pos (nameTrace2 fnId)
    args = take arity . map (ExpVar pos) . nameArgs $ fnId
    wrappedId' = nameWorker fnId
    shareId = nameShare fnId
    to :: Type TraceId -> Exp TokenId -> Exp TokenId
    to ty arg = ExpApplication pos [expTo pos ty, hidden, arg]
    -- assert: length (tyArgs) = arity
    (tyArgs,tyRes) = decomposeFunType ty
    decomposeFunType :: Type TraceId -> ([Type TraceId],Type TraceId)
    decomposeFunType (TypeCons _ tyCon [ty1,ty2]) | isFunTyCon tyCon =
      (ty1:args,res) 
      where
      (args,res) = decomposeFunType ty2
    decomposeFunType ty = ([],ty)


mkFieldSelectors :: [Constr TraceId] -> ([Decl TokenId],ModuleConsts)
mkFieldSelectors constrs = 
    foldr combine ([],emptyModuleConsts) . map (uncurry mkFieldSelector) $ 
      nonDuplicatePosFields
  where
  combine :: ([Decl TokenId],ModuleConsts) -> ([Decl TokenId],ModuleConsts) 
          -> ([Decl TokenId],ModuleConsts)
  combine (decls1,modConsts1) (decls2,modConsts2) = 
    (decls1++decls2,modConsts1 `merge` modConsts2)
  nonDuplicatePosFields :: [(Pos,TraceId)]
  nonDuplicatePosFields = 
    nubBy (\(_,id1) (_,id2) -> tokenId id1 == tokenId id2) posFields
  posFields = 
    concat [pf | (Just pf,_) <- concatMap getConstrArgumentList constrs]


-- construct the traced version of a field selector, using the 
-- normal field selector, i.e. from zname :: T -> R Int construct
-- gname :: SR -> Trace -> R (Fun T Int)
-- gname sr p = fun1 "name" hname sr p
-- hname :: Trace -> R T -> R Int
-- hname p (R v _) = projection mkNoSrcPos p (zname v)
mkFieldSelector :: Pos -> TraceId -> ([Decl TokenId],ModuleConsts)
mkFieldSelector pos fieldId =
  ([DeclFun pos (nameTransLetVar fieldId) 
     [Fun [sr,parent]
       (Unguarded
         (ExpApplication pos
           [combFun pos False 1
           ,ExpVar pos (nameTraceInfoVar pos Global fieldId)
           ,sr,parent,ExpVar pos wrappedId']))
       noDecls]
   ,DeclFun pos wrappedId' 
     [Fun [wrapExp pos var (PatWildcard pos),parent] 
       (Unguarded 
         (ExpApplication pos 
           [ExpVar pos tokenProjection,mkSRExp pos False,parent
           ,ExpApplication pos [ExpVar pos (nameTransField fieldId),var]
           ])) 
       noDecls]]
  ,addVar pos fieldId emptyModuleConsts)
  where
  sr = ExpVar pos (nameSR fieldId)
  parent = mkParentVar pos
  wrappedId' = nameWorker fieldId
  var = ExpVar pos varId
  varId:_ = nameArgs fieldId


tCaf :: Scope -> Bool -> Exp TokenId -> Pos -> TraceId -> Rhs TraceId 
     -> Decls TraceId
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tCaf scope traced parent pos id rhs localDecls =
  -- id sr p = constUse sr p id'
  -- id' = constDef parent "id" (\p' -> [[rhs]]_p')
  ([DeclFun pos (nameTransLetVar id)
     [Fun [useSR,useParent] 
       (Unguarded (ExpApplication pos 
         [combConstUse pos traced,useSR,useParent,id'])) 
       noDecls]
   ,DeclFun pos idId'
     [Fun [] 
       (Unguarded (ExpApplication pos 
         [combConstDef pos traced,parent
         ,ExpVar pos (nameTraceInfoVar pos scope id)
         ,ExpLambda pos [useParent] (smartExpLet pos localDecls' rhs')])) 
       noDecls]]
  ,[]
  ,addVar pos id emptyModuleConsts `withLocal` 
    (rhsConsts `merge` localDeclsConsts))
  where
  idId' = nameShare id
  id' = ExpVar pos idId'
  useSR = ExpVar pos (nameSR id)
  useParent = mkParentVar pos
  (rhs',rhsConsts) = tRhs traced True useParent failContinuation rhs
  (localDecls',localDeclsConsts) = tDecls Local traced useParent localDecls
  smartExpLet :: Pos -> Decls a -> Exp a -> Exp a
  smartExpLet pos (DeclsParse []) e = e
  smartExpLet pos decls e = ExpLet pos decls e

tFuns :: Scope -> Bool -> Pos -> TraceId -> [Fun TraceId]
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tFuns scope traced pos id funs =
  (DeclFun pos (nameTransLetVar id) 
    [Fun [sr,parent]
      (Unguarded
        (ExpApplication pos
          [combFun pos traced funArity
          ,ExpVar pos (nameTraceInfoVar pos scope id)
          ,sr,parent,ExpVar pos wrappedId']))
      noDecls]
   : (if isLocal scope 
        then (DeclFun pos (nameTraceInfoVar pos Global id) 
               [Fun [] (Unguarded 
                 (ExpVar pos (nameTraceInfoVar pos Local id))) noDecls] :) 
        else \x->x)
       (DeclFun pos wrappedId' (funs') : newDecls')
  -- The known-arity application optimisation needs a nameTraceInfoVar of
  -- the global kind (the name does not include the definition position)
  -- Hence for local definitions we need to define the nameTraceInfoVar
  -- in terms of the "local" nameTraceInfoVar that is defined globally.
  -- In same scope as type decl
  , []
  ,addVar pos id (emptyModuleConsts `withLocal` funConsts))
  where
  funArity = case funs of (Fun pats _ _ : _) -> length pats
  sr = ExpVar pos (nameSR id)
  parent = mkParentVar pos
  wrappedId' = nameWorker id
  (funs',newDecls',funConsts) = 
    tFunClauses traced pos parent (nameFuns id) 
      (map (ExpVar pos) (nameArgs id)) funArity False funs

tFunClauses :: Bool
            -> Pos 
            -> Exp TokenId -- variable that can be bound to parent
            -> [TokenId]   -- ids for definitions that clauses are turned into
            -> [Exp TokenId] -- vars for naming arguments that are not vars
            -> Arity
            -> Bool -- preceeding fun-clause will never fail
            -> [Fun TraceId] 
            -> ([Fun TokenId],[Decl TokenId],ModuleConsts)

tFunClauses _ _ _ _ _ _ True [] = ([],[],emptyModuleConsts)
tFunClauses _ pos parent ids pVars funArity False [] =
  ([Fun 
     (replicate funArity (PatWildcard pos) ++ [parent])
     (Unguarded (continuationToExp parent failContinuation)) noDecls]
  ,[],emptyModuleConsts)
tFunClauses traced pos parent ids pVars funArity _ (fun@(Fun pats _ _) : funs)
  | not (null funs) && funCanFail fun =
    ([Fun (pats'' ++ [parent]) rhs' decls'
     ,Fun (vars ++ [parent]) 
       (Unguarded (continuationToExp parent failCont)) noDecls]
    ,DeclFun pos contId funs' : funsDecls
    ,funConsts `merge` funsConsts)
  where
  contId = head ids
  failCont = functionContinuation contId vars
  (pats'',vars) = namePats pats' pVars 
  (Fun pats' rhs' decls',funConsts) = tFun traced True parent failCont fun
  (funs',funsDecls,funsConsts) = 
    tFunClauses traced pos parent (tail ids) pVars 
      funArity (neverFailingPats pats) funs
tFunClauses traced pos parent ids pVars funArity _ (fun@(Fun pats _ _): funs) =
  -- last clause or guards and numeric literals cannot fail
  (Fun (pats'++[parent]) rhs' decls' : funs'
  ,funsDecls
  ,funConsts `merge` funsConsts)
  where
  (Fun pats' rhs' decls',funConsts) = 
    tFun traced True parent failContinuation fun
  (funs',funsDecls,funsConsts) = tFunClauses traced pos parent ids pVars 
                                 funArity (neverFailingPats pats) funs


{-
-- Numeric literals need to be overloaded with respect to the new
-- transformed numeric classes; hence they cannot just be left wrapped
-- in patterns
-- Transform such literals into equality conditions in guards.
-- Need also to desugare ~, because that is the easiest way to deal with 
-- literals within the scope of a ~. NOT YET DONE.
tFun :: Bool -- traced
     -> Bool -- this is reduct of parent
     -> Exp TokenId -- parent
     -> ContExp -- continuation in case of pattern match failure
     -> Fun TraceId -> (Fun TokenId,ModuleConsts)
-- Definition similar to tGuardedExps
tFun traced cr parent contExp (Fun pats rhs decls) =
  if null conditions  -- implies null patsDecls
    then (Fun pats' (Unguarded rhs') decls',declsConsts `withLocal` rhsConsts)
    else
      (Fun pats' 
        (Unguarded (ExpApplication pos 
          (if traced 
             then
               [combGuard pos True,mkSRExp pos traced,parent,cond'
               ,ExpLambda pos [newParent] (ExpLet pos patsDecls' rhs')
               ,ExpLambda pos [newParent] 
                 (continuationToExp newParent contExp)]
             else
               [combGuard pos False,cond',ExpLet pos patsDecls' rhs'
               ,continuationToExp parent contExp])
        )) decls'
      ,pos `addPos` condConsts `merge` patsDeclsConsts `merge` declsConsts 
       `withLocal` rhsConsts)  
      -- condConsts contains positions of the boolean expressions
      -- patsDeclsConsts contains positions of the bound variables
  where
  (pats',conditions,patsDecls) = tPats pats
  (patsDecls',patsDeclsConsts) = 
    tDecls Local traced (if traced then newParent else parent) 
      (DeclsParse patsDecls) 
  (cond',condConsts) = tExp traced False parent (foldr1 andExp conditions)
  (rhs',rhsConsts) = 
    tRhs traced cr 
      (if null conditions || not traced then parent else newParent) contExp rhs
  (decls',declsConsts) = tDecls Local traced parent decls
  andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
  andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]
  pos = getPos pats
  newParent = ExpVar pos (nameFromPos pos)
-}

-- Numeric literals need to be overloaded with respect to the new
-- transformed numeric classes; hence they cannot just be left wrapped
-- in patterns
-- Transform such literals and n+k patterns into conditions in guards.
-- Have to be careful to preserve left-to-right pattern matching,
-- e.g. f 1 True = ... -> f x True | x == 1 = ... is wrong.
-- Assume that ~ has been removed before.
tFun :: Bool -- traced
     -> Bool -- this is reduct of parent
     -> Exp TokenId -- parent
     -> ContExp -- continuation in case of pattern match failure
     -> Fun TraceId -> (Fun TokenId,ModuleConsts)
-- Definition similar to tGuardedExps
tFun traced cr parent contExp (Fun pats rhs decls) =
  if isNothing numericLitInfos
    then (Fun pats' (Unguarded rhs') decls',declsConsts `withLocal` rhsConsts)
    else
      (Fun pats' 
        (Unguarded (ExpApplication pos 
          (if traced 
             then
               [combGuard pos True,mkSRExp pos traced,parent,cond'
               ,ExpLambda pos [parent]
                 (ExpApplication pos (ExpVar pos nameFun:argvars++[parent]))
               ,ExpLambda pos [parent] 
                 (continuationToExp parent contExp)]
             else
               [combGuard pos False,cond'
               ,ExpApplication pos (ExpVar pos nameFun:argvars++[parent])
               ,continuationToExp parent contExp])
        )) 
        (DeclsParse (def:decl'))
      ,pos `addPos` condConsts `merge` declConsts `merge` funConsts) 
      -- condConsts contains positions of the boolean expressions
      -- patsDeclsConsts contains positions of the bound variables
  where
  (pats',numericLitInfos) = tPats pats
  (rhs',rhsConsts) = 
    tRhs traced cr parent contExp rhs
  (decls',declsConsts) = tDecls Local traced parent decls
  Just (cond,bindings,argvars,argpats) = numericLitInfos

  (cond',condConsts) = tExp traced False parent cond
  (DeclsParse decl',declConsts) = 
    tDecls Local traced parent (DeclsParse bindings)
  def = DeclFun pos nameFun 
          [Fun (fpats'++[parent]) frhs' fdecls'
          ,Fun (replicate funArity (PatWildcard pos) ++ [parent])
             (Unguarded (continuationToExp parent contExp)) 
             noDecls]
  funArity = length argpats
  (Fun fpats' frhs' fdecls',funConsts) = 
    tFun traced cr parent contExp (Fun argpats rhs decls)
  pos = getPos pats



funCanFail :: Fun TraceId -> Bool
funCanFail (Fun pats rhs _) = 
  any numericLitIn pats || case rhs of
    Unguarded _ -> False
    Guarded gdExps -> gdExpsCanFail gdExps

numericLitIn :: Pat TraceId -> Bool
numericLitIn (ExpRecord pat fields) = 
  numericLitIn pat || any numericLitInField fields
  where
  numericLitInField (FieldExp _ _ pat) = numericLitIn pat
numericLitIn (ExpApplication _ pats) = any numericLitIn pats
numericLitIn (ExpList _ pats) = any numericLitIn pats
numericLitIn (PatAs _ _ pat) = numericLitIn pat
numericLitIn (PatIrrefutable _ pat) = numericLitIn pat
numericLitIn (ExpLit _ (LitInteger _ _)) = True
numericLitIn (ExpLit _ (LitRational _ _)) = True
numericLitIn _ = False


-- Returns False only if one of the guards definitely evaluates to True.
gdExpsCanFail :: [(Exp TraceId,Exp TraceId)] -> Bool
gdExpsCanFail [] = True
gdExpsCanFail ((ExpCon _ cid, _) : gdExps) = 
  not (isTrue cid) && gdExpsCanFail gdExps
gdExpsCanFail ((ExpVar _ cid, _) : gdExps) = 
  not (isOtherwise cid) && gdExpsCanFail gdExps
gdExpsCanFail (_ : gdExps) = gdExpsCanFail gdExps


namePats :: [Pat TokenId] -> [Pat TokenId] -> ([Pat TokenId],[Pat TokenId])
namePats pats vars = unzip (zipWith namePat pats vars)

-- Obtain a variable that names the given pattern;
-- straightforward if pattern has variable at top-level;
-- otherwise use provided variable
namePat :: Pat TokenId  -- pattern to name
        -> Pat TokenId  -- default variable
        -> (Pat TokenId,Pat TokenId)  -- named pattern, name variable
namePat pat@(ExpVar _ _) _ = (pat,pat)
namePat pat@(PatAs pos id pat') _ = (pat,ExpVar pos id)
namePat pat var@(ExpVar pos id) = (PatAs pos id pat,var)


tRhs :: Bool         -- traced?
     -> Bool         -- equal to parent? 
     -> Exp TokenId  -- parent
     -> ContExp      -- continuation in case of pattern match failure
     -> Rhs TraceId  
     -> (Exp TokenId,ModuleConsts)

tRhs traced cr parent failCont (Unguarded exp) = tExp traced cr parent exp
tRhs traced cr parent failCont (Guarded gdExps) =
  tGuardedExps traced cr parent failCont gdExps


tGuardedExps :: Bool         -- traced?
             -> Bool         -- equal to parent? 
             -> Exp TokenId  -- parent
             -> ContExp      -- continuation in case of pattern match failure
             -> [(Exp TraceId,Exp TraceId)]  
             -> (Exp TokenId,ModuleConsts)
tGuardedExps _ cr parent failCont [] = 
  (continuationToExp parent failCont,emptyModuleConsts)
tGuardedExps True cr parent failCont ((guard,exp):gdExps) =
  (ExpApplication pos 
    [combGuard pos True,mkSRExp pos True,parent,guard'
    ,ExpLambda pos [parent] exp',ExpLambda pos [parent] gdExps']
  ,pos `addPos` guardConsts `merge` expConsts `merge` gdExpsConsts)
  where
  (guard',guardConsts) = tExp True False parent guard
  (exp',expConsts) = tExp True cr parent exp
  (gdExps',gdExpsConsts) = tGuardedExps True cr parent failCont gdExps
  pos = getPos guard
tGuardedExps False cr parent failCont ((guard,exp):gdExps) =
  (ExpApplication pos 
    [combGuard pos False,guard',exp',gdExps']
  ,guardConsts `merge` expConsts `merge` gdExpsConsts)
  where
  (guard',guardConsts) = tExp False False parent guard
  (exp',expConsts) = tExp False cr parent exp
  (gdExps',gdExpsConsts) = tGuardedExps False cr parent failCont gdExps
  pos = getPos guard

-- -----------------------------------------
-- Abstract continuation for guards
--
-- To correctly create the trace within guards, a continuation is used.
-- The type ContExp should be abstract. Its implementation is only used in 
-- the following three functions.

data ContExp = Fail | Function TokenId [Exp TokenId]

failContinuation :: ContExp
failContinuation = Fail

functionContinuation :: TokenId -> [Exp TokenId] -> ContExp
functionContinuation = Function

continuationToExp :: Exp TokenId   -- trace
                  -> ContExp 
                  -> Exp TokenId
continuationToExp parent Fail = mkFailExp noPos parent
continuationToExp parent (Function fun args) =
  ExpApplication noPos (ExpVar noPos fun : args ++ [parent])


mapMerge2 :: (a -> (b,ModuleConsts)) -> [a] -> ([b],ModuleConsts)
mapMerge2 f = mapSnd (foldr merge emptyModuleConsts) . unzip . map f


-- Transform expressions

tExps :: Bool           -- traced
      -> Exp TokenId    -- parent
      -> [Exp TraceId]  -- expressions
      -> ([Exp TokenId],ModuleConsts)
tExps traced parent = mapMerge2 (tExp traced False parent)
tExpsC traced parent = mapMerge2 (tExp traced True parent)


-- Second argument True iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
tExp :: Bool -> Bool -> Exp TokenId -> Exp TraceId 
     -> (Exp TokenId,ModuleConsts)
tExp traced cr parent (ExpScc _ l@(ExpLambda _ _ _)) =
    -- ExpScc is a hack to inform that this lambda is really a do-stmt.
  (ExpApplication pos [fun, ExpVar pos tokenMkAtomDoLambda, sr, par, lambda]
  ,rpos)
  where
  (ExpApplication pos [fun, _, sr, par, lambda],rpos) = tExp traced cr parent l
tExp traced cr parent (ExpLambda pos pats body) =
  (ExpApplication pos 
    [combFun pos traced funArity
    ,ExpVar pos tokenMkAtomLambda
    ,mkSRExp pos traced
    ,parent
    ,if neverFailingPats pats 
       then ExpLambda pos (pats' ++ [parent]) body'
       else ExpLambda pos (vars ++ [parent])
              (ExpCase pos (mkTupleExp pos vars)
                [Alt (mkTupleExp pos pats') (Unguarded body') noDecls
                ,Alt (PatWildcard pos) 
                   (Unguarded (mkFailExp pos parent)) noDecls])]
  ,pos `addPos` bodyConsts)
  where
  (Fun pats' (Unguarded body') _,bodyConsts) = 
    tFun traced True parent failContinuation 
      (Fun pats (Unguarded body) noDecls)
  vars = map (ExpVar pos) . take funArity $ varsIds
  varsIds = namesFromPos pos
  funArity = length pats
tExp traced cr parent (ExpLet pos decls body) =
  (ExpLet pos decls' body'
  ,declConsts `withLocal` bodyConsts)
  where
  (decls',declConsts) = tDecls Local traced parent decls
  (body',bodyConsts) = tExp traced cr parent body
tExp traced cr parent (ExpDo pos stmts) =
  tExp traced cr parent (removeDo traced stmts)
tExp traced cr parent (ExpCase pos e alts) =
  (ExpApplication pos 
    [combCase pos traced,mkSRExp pos traced,parent
    ,ExpLet pos (DeclsParse (DeclFun pos varId fun' : defs')) 
      (ExpVar pos varId)
    ,e']
  ,pos `addPos` eConsts `merge` funConsts)
  where
  (varId:argId:funsIds) = namesFromPos pos
  (e',eConsts) = tExp traced False parent e
  (fun',defs',funConsts) = 
    tFunClauses traced pos parent funsIds 
      [ExpVar pos argId] 1 False
      . map alt2Fun $ alts
tExp True cr parent (ExpIf pos cond e1 e2) =
  (ExpApplication pos 
    [combIf pos True,mkSRExp pos True,parent,cond'
    ,ExpLambda pos [parent] e1',ExpLambda pos [parent] e2']
  ,pos `addPos` condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond',condConsts) = tExp True False parent cond
  (e1',e1Consts) = tExp True True parent e1
  (e2',e2Consts) = tExp True True parent e2
tExp False cr parent (ExpIf pos cond e1 e2) =
  (ExpApplication pos [combIf pos False,parent,cond',e1',e2']
  ,condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond',condConsts) = tExp False False parent cond
  (e1',e1Consts) = tExp False True parent e1
  (e2',e2Consts) = tExp False True parent e2
tExp traced cr parent (ExpType pos e contexts ty) =
  (ExpType pos e' (tContexts contexts) (wrapType (tType ty))
  ,eConsts)
  where
  (e',eConsts) = tExp traced cr parent e
tExp traced cr parent (ExpApplication pos (f@(ExpCon _ _) : es))=
  tConApp pos traced parent f es
tExp traced cr parent (ExpApplication pos (f@(ExpVar idPos id) : es)) 
  | case arity id of
      Nothing -> False
      Just a  -> a > 0 && a == length es && a <= 5 = 
  -- optimised for this special case
  (if traced || isTraced id
     then ExpApplication pos (combApplyArity pos (isTraced id) (length es):
            mkSRExp pos traced:mkSRExp idPos traced:parent:
            ExpVar pos (nameTraceInfoVar pos Global id):
            ExpVar pos (nameWorker id):es')
     else ExpApplication pos (ExpVar pos tokenUWrapForward:parent: 
            [ExpApplication pos (ExpVar pos (nameWorker id):es'++[parent])])
  ,pos `addPos` (idPos `addPos` esConsts))
  where
  (es',esConsts) = tExps traced parent es
tExp traced cr parent (ExpApplication pos es) =
  (ExpApplication pos 
    (combApply pos traced (length es - 1):mkSRExp pos traced:parent:es')
  ,pos `addPos` esConsts)
  where
  (es',esConsts) = tExps traced parent es
tExp traced cr parent (ExpVar pos id) =
  if isLambdaBound id  
    then 
      let e' = ExpVar pos (nameTransLambdaVar id) 
      in
        if cr 
        then (ExpApplication pos 
               [ExpVar pos tokenProjection,mkSRExp pos traced,parent,e']
             ,pos `addPos` emptyModuleConsts) 
        else (e',emptyModuleConsts)
    else 
      (ExpApplication pos 
        [ExpVar pos (nameTransLetVar id),mkSRExp pos traced,parent]
      ,pos `addPos` emptyModuleConsts)
tExp traced cr parent e@(ExpCon pos id) =
  tConApp pos traced parent e []
tExp traced cr parent (ExpLit pos litstr@(LitString _ s)) =
  -- Because the result is very large use special combinator that
  -- transforms string in traced string at runtime instead
  (ExpApplication pos
     [ExpVar pos tokenFromLitString,mkSRExp pos traced,parent
     ,ExpLit pos litstr]
  ,pos `addPos` emptyModuleConsts)
tExp traced cr parent (ExpLit pos lit@(LitChar _ _)) =
  (ExpApplication pos 
    [ExpVar pos tokenConChar,mkSRExp pos traced,parent,ExpLit pos lit]
  ,pos `addPos` emptyModuleConsts)
tExp traced cr parent (ExpLit pos lit@(LitRational b r)) =
  -- desugar rational constant into explicit use of ":%",
  -- because Rational is not a primitive type but defined in PreludeBasic
  -- however, this way mkNTRational is not used at all
  (ExpApplication pos 
    [combApply pos traced 1,sr,parent
    ,ExpApplication pos [ExpVar pos tokenFromRational,sr,parent]
    ,ExpApplication pos 
      [ExpCon pos tokenR
      ,ExpApplication pos 
        [ExpCon pos tokenConRational
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,num]
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,denom]]
      ,ExpApplication pos 
        [ExpVar pos tokenMkAtomRational,sr,parent,ExpLit pos lit]]]
  ,pos `addPos` emptyModuleConsts)
  where
  num = ExpLit pos (LitInteger b (numerator r))
  denom = ExpLit pos (LitInteger b (denominator r))
  sr = mkSRExp pos traced
tExp traced cr parent (ExpLit pos lit@(LitInteger _ _)) =
  (ExpApplication pos 
    [combApply pos traced 1,sr
    ,parent
    ,ExpApplication pos [ExpVar pos tokenFromInteger,sr,parent]
    ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,ExpLit pos lit]]
  ,pos `addPos` emptyModuleConsts)
  where
  sr = mkSRExp pos traced
tExp traced cr parent (ExpList pos es) =
  -- use special combinator that transforms list at runtime;
  -- desugaring and subsequent transformation would lead to large program.
  (ExpApplication pos
     [ExpVar pos tokenFromExpList,sr,parent,ExpList pos es']
  ,pos `addPos` esConsts)
  where
  (es',esConsts) = tExps traced parent es
  sr = mkSRExp pos traced
tExp traced cr parent (ExpRecord (ExpCon pos consId) fields) = -- construction
  (ExpApplication pos 
    [ExpVar pos tokenWrapValFun,sr,ExpRecord consUndefined fields',parent]
  ,pos `addPos` fieldsConsts)
  where
  consUndefined = 
    if consArity == 0 then ExpCon pos (nameTransCon consId)
      else ExpApplication pos . (ExpCon pos (nameTransCon consId) :) .
             take consArity . repeat $ 
               ExpApplication pos 
                 [ExpVar pos tokenUndefined,mkSRExp pos False,parent]
  Just consArity = arity consId
  sr = mkSRExp pos traced
  (fields',fieldsConsts) = mapMerge2 (tField traced parent) fields
tExp True cr parent (ExpRecord exp fields) = -- update
  (ExpLet pos 
    (DeclsParse $ 
      zipWith (DeclFun pos) fieldVarIds 
        (map ((:[]) . flip (Fun []) noDecls . Unguarded) fieldExps'))
    (ExpApplication pos
      (combUpdate pos True (length labels):mkSRExp pos True:parent:exp'
      :ExpLambda pos [var] (ExpRecord var varFields')
      :labels ++ fieldVars))
  ,pos `addPos` expConsts `merge` fieldsConsts)
  where
  (exp',expConsts) = tExp True False parent exp
  (fields',fieldsConsts) = mapMerge2 (tField True parent) fields
  labels = map (ExpVar pos . nameTraceInfoVar noPos Global) labelIds
  varFields' = zipWith (FieldExp pos) (map fieldLabel fields') fieldVars
  fieldExps' = map fieldExp fields'
  fieldVars = map (ExpVar pos) fieldVarIds
  fieldVarIds = map nameShare labelIds
  labelIds = map fieldLabel fields
  var = ExpVar pos (nameFromPos pos)
  pos = getPos fields
  fieldLabel (FieldExp _ labelId _) = labelId
  fieldExp (FieldExp _ _ exp) = exp
tExp False cr parent (ExpRecord exp fields) = -- update
  (ExpApplication pos
    [combUpdate pos False (-1),parent,exp'
    ,ExpLambda pos [var] (ExpRecord var fields')]
  ,expConsts `merge` fieldsConsts)
  where
  (exp',expConsts) = tExp False False parent exp
  (fields',fieldsConsts) = mapMerge2 (tField False parent) fields
  var = ExpVar pos (nameFromPos pos)
  pos = getPos fields
tExp _ _ _ _ = error "tExp: unknown sort of expression"


tField :: Bool -> Exp TokenId -> Field TraceId 
       -> (Field TokenId,ModuleConsts)
tField traced parent (FieldExp pos labelId exp) =
  (FieldExp pos (nameTransField labelId) exp',expConsts)
  where
  (exp',expConsts) = tExp traced False parent exp


-- return False if matching the pattern may fail
-- otherwise try to return True
-- (safe approximation)
neverFailingPat :: Pat id -> Bool
neverFailingPat (ExpVar _ _) = True
neverFailingPat (PatAs _ _ pat) = neverFailingPat pat
neverFailingPat (PatIrrefutable _ _) = True
neverFailingPat (PatWildcard _ ) = True
neverFailingPat _ = False

neverFailingPats :: [Pat id] -> Bool
neverFailingPats = all neverFailingPat

-- conversion of case alternative into function definition alternative 
alt2Fun :: Alt a -> Fun a
alt2Fun (Alt pat rhs decls) = Fun [pat] rhs decls 

fun2Alt :: Fun a -> Alt a
fun2Alt (Fun [pat] rhs decls) = Alt pat rhs decls


-- Transform data constructor application.
-- Number of arguments may be smaller than arity of the data constructor.
tConApp :: Pos           -- of application
        -> Bool          -- traced?
        -> Exp TokenId   -- parent
        -> Exp TraceId   -- data constructor
        -> [Exp TraceId] -- arguments
        -> (Exp TokenId,ModuleConsts)  

tConApp pos traced parent c@(ExpCon _ id) args 
  | conArity > numberOfArgs = -- undersaturated application
    (ExpApplication pos 
      (combPartial pos numberOfArgs
      :ExpCon pos (nameTransCon id)
      :combCn pos (conArity-numberOfArgs)
      :mkSRExp pos traced
      :parent
      :ExpVar pos (nameTraceInfoCon id)
      :args')
    ,pos `addPos` argsConsts)
  | otherwise = tSatConApp pos traced parent c args
  where
  Just conArity = arity id  -- a constructor always has an arity
  numberOfArgs = length args
  (args',argsConsts) = tExps traced parent args


-- Transform data constructor application with number of args equal arity.
tSatConApp :: Pos           -- of application
           -> Bool          -- traced?
           -> Exp TokenId   -- parent
           -> Exp TraceId   -- data constructor
           -> [Exp TraceId] -- arguments 
           -> (Exp TokenId,ModuleConsts)
tSatConApp pos traced parent (ExpCon _ id) args =
  (ExpApplication pos 
    (combCon pos (length args)
    :mkSRExp pos traced
    :parent
    :ExpCon pos (nameTransCon id)
    :ExpVar pos (nameTraceInfoCon id)
    :args')
  ,pos `addPos` argsConsts)
  where
  (args',argsConsts) = tExps traced parent args

-- Desugar do-statements 
removeDo :: Bool -> [Stmt TraceId] -> Exp TraceId
removeDo traced [StmtExp e] = e
removeDo traced (StmtExp e : stmts) =
  ExpApplication pos [ExpVar pos tTokenGtGt,e,removeDo traced stmts] 
  where
  pos = getPos e
removeDo traced (StmtLet decls : stmts) =
  ExpLet pos decls (removeDo traced stmts)
  where
  pos = getPos decls
removeDo traced (StmtBind pat e : stmts) =
  ExpApplication pos 
    [ExpVar pos tTokenGtGtEq,e
    ,ExpScc "" $	-- hack to inform this is really a do-stmt
    if neverFailingPat pat 
      then ExpLambda pos [pat] (removeDo traced stmts)
      else ExpLambda pos [newVar] 
             (ExpCase pos newVar 
               [Alt pat (Unguarded (removeDo traced stmts)) noDecls
               ,Alt (PatWildcard pos) 
                 (Unguarded 
                   (ExpApplication pos 
                     [ExpVar pos tTokenFail
                     ,ExpLit pos 
                       (LitString Boxed 
                         "pattern-match failure in do expression")]))
                 noDecls])
    ]
  where
  newVar = ExpVar pos (mkLambdaBound (initAuxInfo traced) newId)
  newId:_ = namesFromPos pos
  pos = getPos e

{-
mapCombine3 :: (a -> (b,[c],[d])) -> [a] -> ([b],[c],[d])
mapCombine3 f = (\(p,es,ds) -> (p,concat es,concat ds)) . unzip3 . map f

-- the first part of the result is transformed pattern
-- the transformation has to remove numeric constants and
-- n+k patterns from a pattern, because numeric arguments are
-- wrapped and hence translating into constants and n+k patterns is impossible.
-- Furthermore we want their reductions in the trace as well.
-- the second part contains boolean expressions that are
-- implicitly tested in the original pattern but have to be made
-- explicit in the transformed program, e.g.
-- tPats 3 = (x,{x==3},{})
-- the third part contains definitions of numeric variables 
-- that orginate from n+k patterns
-- e.g.: tPats (n+2) = (x,{x>=2},{n=x-2})
-- Note that variables in patterns are always lambda bound
-- (pattern bindings are treated specially anyway)
tPats :: [Pat TraceId] -> ([Pat TokenId],[Exp TraceId],[Decl TraceId])
tPats = mapCombine3 tPat

tPat :: Pat TraceId -> (Pat TokenId,[Exp TraceId],[Decl TraceId])
tPat (ExpRecord (ExpCon pos id) fields) = 
  (wrapExp pos (ExpRecord (ExpCon pos (nameTransCon id)) fields') 
    (PatWildcard pos)
  ,fieldsExps
  ,fieldsDecls)
  where
  (fields',fieldsExps,fieldsDecls) = mapCombine3 tField fields
  tField (FieldExp pos id pat) = 
    (FieldExp pos (nameTransField id) pat',patExps,patDecls)
    where
    (pat',patExps,patDecls) = tPat pat
tPat (ExpApplication pos (ExpCon pos2 id : pats)) = 
  (wrapExp pos 
    (ExpApplication pos (ExpCon pos2 (nameTransCon id) : pats'))
    (PatWildcard pos)
  ,patsExps,patsDecls)
  where
  (pats',patsExps,patsDecls) = tPats pats
  -- negative numeric literals are represented as (negate number):
tPat (ExpApplication _ [_,ExpLit pos (LitInteger boxed i)]) =
  tPat (ExpLit pos (LitInteger boxed (-i)))
tPat (ExpApplication _ [_,ExpLit pos (LitRational boxed r)]) =
  tPat (ExpLit pos (LitRational boxed (-r)))
tPat (ExpApplication _ [_,ExpLit pos _]) = error "tPat: app expLit"
tPat (ExpVar pos id) = (ExpVar pos (nameTransLambdaVar id),[],[])
tPat (ExpCon pos id) = 
  (wrapExp pos (ExpCon pos (nameTransCon id)) (PatWildcard pos),[],[])
tPat (ExpLit pos (LitString _ s)) =
  tPat . mkTList pos . map (ExpLit pos . LitChar Boxed) $ s
tPat (ExpLit pos lit@(LitChar _ _)) = 
  (wrapExp pos (ExpLit pos lit) (PatWildcard pos),[],[]) 
tPat e@(ExpLit pos lit) = -- only LitInteger and LitRational left
  (ExpVar pos (nameTransLambdaVar tid)
  ,[ExpApplication pos 
     [ExpVar pos tTokenEqualEqual,ExpVar pos tid,e]]
  ,[])
  where
  tid = mkLambdaBound (nameFromPos pos)
tPat (ExpList pos pats) = tPat . mkTList pos $ pats
tPat (PatAs pos id pat) = 
  (PatAs pos (nameTransLambdaVar id) pat',patExps,patDecls)
  where
  (pat',patExps,patDecls) = tPat pat
tPat (PatWildcard pos) = (PatWildcard pos,[],[])  -- type change
tPat (PatIrrefutable pos pat) = 
  if null patExps 
    then
      (case pat' of
         ExpApplication pos' [r,p',t'] -> 
           ExpApplication pos' [r,PatIrrefutable pos p',t']
         x -> x
      ,[],[])
    else error "Numeric pattern inside ~ is currently not implemented."
  where
  (pat',patExps,patDecls) = tPat pat 
tPat (PatNplusK pos id _ k _ _) = 
  (ExpVar pos (nameTransLambdaVar tid2)
  ,[ExpApplication pos 
     [ExpVar pos tTokenGreaterEqual,var2,k]]
  ,[DeclFun pos id
     [Fun []
       (Unguarded (ExpApplication pos [ExpVar pos tTokenMinus,var2,k]))
       noDecls]])
  where
  var2 = ExpVar pos tid2
  tid2 = mkLambdaBound (nameFromPos pos)
tPat p = error ("tPat: unknown pattern at " ++ strPos (getPos p))
-}

-- new:

mapCombinePats :: (Pat TraceId 
                   -> (Exp TokenId, Maybe (c,d,[Exp TokenId],[Pat TraceId])))
               -> [Pat TraceId] 
               -> ([Exp TokenId], Maybe (c,d,[Exp TokenId],[Pat TraceId]))
mapCombinePats f [] = ([],Nothing)
mapCombinePats f (x:xs) = 
  case f x of
    (pat,Nothing) -> (pat:pats,numPatInfos)
    (pat,Just (e,d,vs,ps)) -> (pat:xvs,Just (e,d,vs++xvs,ps++xs))
  where
  (pats,numPatInfos) = mapCombinePats f xs
  xvs = map patToVar xs

patToVar :: Pat TraceId -> Exp TokenId
patToVar (ExpVar pos id) = ExpVar pos (nameTransLambdaVar id)
patToVar (PatAs pos id _) = ExpVar pos (nameTransLambdaVar id)
patToVar pat = ExpVar pos (nameFromPos pos)
  where
  pos = getPos pat


tPats :: [Pat TraceId] 
      -> ([Pat TokenId] -- transformed patterns
         ,Maybe -- only if there is a numeric pattern (k or n+k)
           (Exp TraceId     -- test for the numeric pattern (e.g. x==k)
           ,[Decl TraceId]  -- binding for n if there is an n+k pattern
           ,[Exp TokenId]   -- variables inserted in pattern after num literal
           ,[Pat TraceId])) -- original patterns after num literal
tPats = mapCombinePats tPat

tPat :: Pat TraceId 
     -> (Pat TokenId
        ,Maybe (Exp TraceId,[Decl TraceId],[Exp TokenId],[Pat TraceId]))
tPat (ExpRecord (ExpCon pos id) fields) = 
  (wrapExp pos (ExpRecord (ExpCon pos (nameTransCon id)) fields') 
    (PatWildcard pos)
  ,numInfos)
  where
  (poss,ids,pats) = unzipFields fields
  (pats',numInfos) = mapCombinePats tPat pats
  fields' = zipFields poss (map nameTransField ids) pats'
  unzipFields :: [Field a] -> ([Pos],[a],[Pat a])
  unzipFields = unzip3 . map (\(FieldExp pos id pat) -> (pos,id,pat))
  zipFields :: [Pos] -> [a] -> [Pat a] -> [Field a]
  zipFields = zipWith3 FieldExp 
tPat (ExpApplication pos (ExpCon pos2 id : pats)) = 
  (wrapExp pos 
    (ExpApplication pos (ExpCon pos2 (nameTransCon id) : pats'))
    (PatWildcard pos)
  ,patsNumInfo)
  where
  (pats',patsNumInfo) = tPats pats
  -- negative numeric literals are represented as (negate number):
tPat (ExpApplication _ [_,ExpLit pos (LitInteger boxed i)]) =
  tPat (ExpLit pos (LitInteger boxed (-i)))
tPat (ExpApplication _ [_,ExpLit pos (LitRational boxed r)]) =
  tPat (ExpLit pos (LitRational boxed (-r)))
tPat (ExpApplication _ [_,ExpLit pos _]) = error "tPat: app expLit"
tPat (ExpVar pos id) = (ExpVar pos (nameTransLambdaVar id),Nothing)
tPat (ExpCon pos id) = 
  (wrapExp pos (ExpCon pos (nameTransCon id)) (PatWildcard pos),Nothing)
tPat (ExpLit pos (LitString _ s)) =
  tPat . mkTList pos . map (ExpLit pos . LitChar Boxed) $ s
tPat (ExpLit pos lit@(LitChar _ _)) = 
  (wrapExp pos (ExpLit pos lit) (PatWildcard pos),Nothing) 
tPat e@(ExpLit pos lit) = -- only LitInteger and LitRational left
  (ExpVar pos (nameTransLambdaVar tid)
  ,Just (ExpApplication pos [ExpVar pos tTokenEqualEqual,ExpVar pos tid,e]
        ,[],[],[]))
  where
  tid = mkLambdaBound defaultAuxInfo (nameFromPos pos)
tPat (ExpList pos pats) = tPat . mkTList pos $ pats
tPat (PatAs pos id pat) = 
  (PatAs pos (nameTransLambdaVar id) pat',patNumInfos)
  where
  (pat',patNumInfos) = tPat pat
tPat (PatWildcard pos) = (PatWildcard pos,Nothing)  -- type change
tPat (PatIrrefutable pos pat) = 
  if isNothing patNumInfos
    then
      (case pat' of
         ExpApplication pos' [r,p',t'] -> 
           ExpApplication pos' [r,PatIrrefutable pos p',t']
         x -> x
      ,Nothing)
    else error "Numeric literal or n+k inside ~ is currently not implemented."
  where
  (pat',patNumInfos) = tPat pat 
tPat (PatNplusK pos id _ k _ _) = 
  (ExpVar pos (nameTransLambdaVar tid2)
  ,Just (ExpApplication pos [ExpVar pos tTokenGreaterEqual,var2,k]
        ,[DeclFun pos id
           [Fun []
             (Unguarded (ExpApplication pos [ExpVar pos tTokenMinus,var2,k]))
           noDecls]]
        ,[],[]))
  where
  var2 = ExpVar pos tid2
  tid2 = mkLambdaBound defaultAuxInfo (nameFromPos pos)
tPat p = error ("tPat: unknown pattern at " ++ strPos (getPos p))


-- Convert a list of expressions into a list expression (with TraceIds).
-- Needed for list literals in patterns.
mkTList :: Pos -> [Exp TraceId] -> Exp TraceId
mkTList pos = 
  foldr (\x xs -> ExpApplication pos [cons,x,xs]) (ExpCon pos tTokenNil)
  where
  cons = ExpCon pos tTokenCons

mkList :: Pos -> [Exp TokenId] -> Exp TokenId
mkList pos = 
  foldr (\x xs -> ExpApplication pos [cons,x,xs]) (ExpCon pos t_ListNQ)
  where
  cons = ExpCon pos t_ColonNQ

-- ----------------------------------------------------------------------------
-- Transform types

tTyArgs :: [(Maybe [(Pos,TraceId)],Type TraceId)] 
        -> [(Maybe [(Pos,TokenId)],Type TokenId)]
tTyArgs = map tTyArg

tTyArg :: (Maybe [(Pos,TraceId)],Type TraceId)
       -> (Maybe [(Pos,TokenId)],Type TokenId)
tTyArg (maybePosIds,ty) = 
  (fmap (mapListSnd nameTransField) maybePosIds,wrapType (tType ty))



-- ty ==> R [[ty]]
tConstType :: Type TraceId -> Type TokenId
tConstType ty = wrapType (tType ty)


-- ty ==> RefSrcPos -> Trace -> R [[ty]]
tFunType :: Type TraceId -> Type TokenId
tFunType ty = 
  TypeCons pos tokenRefSrcPos [] `typeFun` TypeCons pos tokenRefExp [] 
    `typeFun` wrapType (tType ty)
  where
  pos = getPos ty

-- create type of worker from original type
tWorkerType :: Arity -> Type TraceId -> Type TokenId
tWorkerType 0 ty = 
  TypeCons (getPos ty) tokenRefExp [] `typeFun` wrapType (tType ty)
tWorkerType a ty = tWorkerSynExpand a ty []

-- expand a type synonym and then apply worker type transformation
-- need to collect type arguments
tWorkerSynExpand :: Arity -> Type TraceId -> [Type TraceId] -> Type TokenId
tWorkerSynExpand a (TypeCons pos tyCon []) [ty1,ty2] | isFunTyCon tyCon =
  wrapType (tType ty1) `typeFun` tWorkerType (a-1) ty2
tWorkerSynExpand a (TypeCons pos tyCon []) tys =
  tWorkerType a (expandTypeSynonym tyCon tys)
tWorkerSynExpand a (TypeCons pos tyCon ty1s) ty2s = 
  tWorkerSynExpand a (TypeCons pos tyCon []) (ty1s++ty2s)
tWorkerSynExpand a (TypeApp ty1 ty2) tys = 
  tWorkerSynExpand a ty1 (ty2:tys)
tWorkerSynExpand _ _ _ = 
  error "tWorkerSynExpand: type must be a function but is not"

expandTypeSynonym :: TraceId -> [Type TraceId] -> Type TraceId
expandTypeSynonym tySyn tys =
  case typeSynonymBody tySyn of
    Nothing -> error ("expandTypeSynonym: " ++ show (tokenId tySyn) ++ 
                        " is not a type synonym")
    Just body -> fst (go body 1)
  where
  go :: TySynBody -> Int -> (Type TraceId,Int)
  go THelper n = 
    (TypeCons noPos (mkTyCon defaultAuxInfo (nameTransTySynHelper tySyn n)) tys
    ,n+1)
  go (TVar v) n = (tys!!v,n)
  go TFun n = (TypeCons noPos tTokenFun [],n)
  go (TApp ty1 ty2) n = (TypeApp ty1' ty2',n2)
    where
    (ty1',n1) = go ty1 n
    (ty2',n2) = go ty2 n1

-- just rewrite function types:
-- t1 -> t2  ==>  Fun [[t1]] [[t2]]
tType :: Type TraceId -> Type TokenId
tType (TypeCons pos tyCon tys) =
  TypeCons pos (nameTransTyConCls tyCon) (map tType tys)
tType (TypeApp lTy rTy) = TypeApp (tType lTy) (tType rTy)
tType (TypeVar pos tyId) = TypeVar pos (nameTransTyVar tyId)
tType (TypeStrict pos ty) = TypeStrict pos (tType ty)

-- ty ==> R ty  (!ty ==> ! (R ty))
wrapType :: Type TokenId -> Type TokenId
wrapType (TypeStrict pos ty) = TypeStrict pos (wrapType ty)
wrapType ty = TypeCons noPos tokenR [ty]

-- replace TraceIds by TokenIds (renaming -> to T.Fun, [] -> T.List etc)
tokenIdType :: Type TraceId -> Type TokenId
tokenIdType (TypeCons pos tyCon tys) =
  TypeCons pos (nameTransTyConCls tyCon) (map tokenIdType tys)
tokenIdType (TypeApp lTy rTy) = TypeApp (tokenIdType lTy) (tokenIdType rTy)
tokenIdType (TypeVar pos tyId) = TypeVar pos (nameTransTyVar tyId)
tokenIdType (TypeStrict pos ty) = TypeStrict pos (tokenIdType ty) 

-- replace TraceIds by TokenIds (no renaming - just plain original types)
typePlain :: Type TraceId -> Type TokenId
typePlain (TypeCons pos tyCon tys) =
  TypeCons pos (tokenId tyCon) (map typePlain tys)
typePlain (TypeApp lTy rTy) = TypeApp (typePlain lTy) (typePlain rTy)
typePlain (TypeVar pos tyId) = TypeVar pos (tokenId tyId)
typePlain (TypeStrict pos ty) = TypeStrict pos (typePlain ty) 

-- function type constructor
-- infixr 6 `typeFun`
typeFun :: Type TokenId -> Type TokenId -> Type TokenId
typeFun ty1 ty2 = TypeCons noPos t_Arrow [ty1,ty2]


tContexts :: [Context TraceId] -> [Context TokenId]
tContexts = map tContext

tContext :: Context TraceId -> Context TokenId
tContext (Context pos clsId posTyVarIds) =
  Context pos (nameTransTyConCls clsId)
          (map (\(pos',tyVarId)->(pos',nameTransTyVar tyVarId)) posTyVarIds)

tSimple :: Simple TraceId -> Simple TokenId 
tSimple (Simple pos tycon posArgs) =
  Simple pos (nameTransTyConCls tycon) (tPosTyVars posArgs)

tPosShares :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosShares = mapListSnd nameShare

tPosExps :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosExps = mapListSnd nameTransLetVar

tPosClss :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosClss = mapListSnd nameTransTyConCls

tPosTyVars :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosTyVars = mapListSnd nameTransTyVar


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

nameTraceInfoModule :: TraceId -> TokenId
nameTraceInfoModule = prefixModName 't'

nameTraceInfoVar :: Pos -> Scope -> TraceId -> TokenId
nameTraceInfoVar pos Global = prefixName 'a' '+'
nameTraceInfoVar pos Local = prefixPosName 'a' '+' pos

nameTraceInfoGlobalVar :: TraceId -> TokenId
nameTraceInfoGlobalVar = prefixName 'a' '+'

nameTraceInfoCon :: TraceId -> TokenId
nameTraceInfoCon = prefixName 'a' '+'

nameTraceInfoPos :: Pos -> TokenId
nameTraceInfoPos pos = mkUnqualifiedTokenId (('p':) . showsEncodePos pos $ "")

-- names referring to transformed program fragments:

nameTransModule :: TraceId -> TokenId
nameTransModule = updateToken updateModule
  where
  updateModule ('N':'o':'t':'H':'a':'t':'.':orgName) = orgName
  updateModule (name@"Main") = name  -- if the module is `Main', then unchanged
  updateModule name = modulePrefix ++ name

nameTransTyConCls  :: TraceId -> TokenId
nameTransTyConCls = updateToken id  -- only module name is changed

-- names of helper synonyms are a bit of a hack; a name conflict is possible
nameTransTySynHelper :: TraceId -> Int -> TokenId
nameTransTySynHelper tySyn no = updateToken (++ ("___" ++ show no)) tySyn

nameTransTyVar :: TraceId -> TokenId
nameTransTyVar = updateToken id  -- only module name is changed

nameTransCon :: TraceId -> TokenId
nameTransCon = updateToken id  -- only module name is changed

nameTransField :: TraceId -> TokenId
nameTransField = prefixName 'b' '^'

nameTransLetVar :: TraceId -> TokenId
nameTransLetVar = prefixName 'g' '!'

nameTransLambdaVar :: TraceId -> TokenId
nameTransLambdaVar = prefixName 'f' '&'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: TraceId -> TokenId
nameWorker = prefixName 'h' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: TraceId -> TokenId
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: TraceId -> TokenId
nameShare = prefixName 's' '|'

-- variable for a trace including position
nameTraceShared :: Pos -> TraceId -> TokenId
nameTraceShared = prefixPosName 'j' '$'

-- variable for parent
nameParent :: TokenId
nameParent = mkUnqualifiedTokenId "p"

-- variable for a trace
nameTrace :: TraceId -> TokenId
nameTrace = prefixName 'j' '$'

-- second variable for a trace
nameTrace2 :: TraceId -> TokenId
nameTrace2 = prefixName 'k' '@'

-- name for a local variable for a source reference
nameSR :: TraceId -> TokenId
nameSR = prefixName 'p' '%'

-- intermediate function
nameFun :: TokenId
nameFun = mkUnqualifiedTokenId "h"

-- infinite list of var ids made from one id (for function clauses)
nameFuns :: TraceId -> [TokenId]
nameFuns = prefixNames 'y' '>'

-- infinite list of var ids made from one id (for naming arguments)
nameArgs :: TraceId -> [TokenId]
nameArgs = prefixNames 'z' '^'

-- a single id made from a position (different from below)
nameFromPos :: Pos -> TokenId
nameFromPos pos = mkUnqualifiedTokenId . ('v':) . showsEncodePos pos $ "n"

-- infinite list of ids made from a position
namesFromPos :: Pos -> [TokenId]
namesFromPos pos =
  map (mkUnqualifiedTokenId . ('v':) . showsEncodePos pos . ('v':) . show) 
    [1..]

-- Generation of new variables

showsEncodePos :: Pos -> ShowS
showsEncodePos pos = shows beginRow . ('v':) . shows beginColumn . ('v':) 
  . shows endRow  . ('v':) . shows endColumn
  where
  (beginRow,beginColumn,endRow,endColumn) = fromPos pos

showsSymEncodePos :: Pos -> ShowS
showsSymEncodePos pos = 
  \xs -> numToSym (show beginRow) ++ '=' : numToSym (show beginColumn) ++ '=' 
    : numToSym (show endRow) ++ '=' : numToSym (show endColumn) ++ xs 
  where
  (beginRow,beginColumn,endRow,endColumn) = fromPos pos

prefixName :: Char -> Char -> TraceId -> TokenId
prefixName c d = updateToken update
  where
  update name = if isOperatorName name then d:name else c:name

prefixModName :: Char -> TraceId -> TokenId
prefixModName c = updateToken update
  where
  update name = c: map (\c->if c=='.' then '_' else c) name

prefixPosName :: Char -> Char -> Pos -> TraceId -> TokenId
prefixPosName c d pos = updateToken update
  where
  update name = if isOperatorName name 
                  then (d:) . showsSymEncodePos pos $ name 
                  else (c:) . showsEncodePos pos $ name

prefixNames :: Char -> Char -> TraceId -> [TokenId]
prefixNames c d token = map (($ token) . updateToken . update) [1..]
  where
  update no name = if isOperatorName name
                    then (d:) . (++ name) . numToSym . show $ no
                    else (c:) . (++ name) . show $ no

isOperatorName :: String -> Bool
isOperatorName = not . (\c -> isAlpha c || c == '_') . head

numToSym :: String -> String
numToSym = map (("!#$%&*+^@>" !!) . digitToInt)

-- Tokens

modulePrefix = "Hat."

-- apply function to unqualified name part 
-- and prefix module name (if qualified)
updateToken :: (String -> String) -> TraceId -> TokenId
updateToken f traceId | isFunTyCon traceId = 
  Qualified tracingModuleShort (packString . reverse $ "Fun")
updateToken f traceId = 
  case tokenId (traceId) of
    t | eqPredefined "[]" t -> 
      Qualified tracingModuleShort (packString . reverse . f $ "List")
    t | eqPredefined ":" t -> 
      Qualified tracingModuleShort (packString . reverse . f $ "Cons")
    t@(TupleId n) -> 
      Qualified 
        tracingModuleShort
        (packString . reverse . f $ ("Tuple"++show n)) 
    Visible n     -> 
      Visible (packString . reverse . f . unqual $ n) 
    Qualified m n -> 
      Qualified 
        (packString . reverse . updateModule . reverse . unpackPS $ m)
        (packString . reverse . f . unqual $ n) 
    _             -> error "TraceTrans: updateToken"
  where
  updateModule (name@"Main") = name -- if module is `Main', then unchanged
  updateModule name = modulePrefix ++ name
  unqual :: PackedString -> String
  unqual = reverse . unpackPS

--             case reverse . unpackPS $ n of -- change predefined names
--               -- "->" -> "Fun"
--               ":" -> "Cons" 
--               "[]" -> "List" -- here both type and data constructor
--               s -> s


-- ----------------------------------------------------------------------------
-- hardwired Haskell combinators and other names used by transformed modules

mkRoot :: Pos -> Exp TokenId
mkRoot pos = ExpVar pos tokenMkRoot

mkParentVar :: Pos -> Exp TokenId
mkParentVar pos = ExpVar pos nameParent

mkSRExp :: Pos -> Bool -> Exp TokenId
mkSRExp pos traced = 
  ExpVar pos (if traced then nameTraceInfoPos pos else tokenMkNoPos)

combApply :: Pos -> Bool -> Arity -> Exp TokenId
combApply pos traced a = 
  testArity pos (if traced then 15 else 8) a "application with more than "
    " arguments."
    (ExpVar pos ((if traced then tokenAp else tokenUAp) a))

combApplyArity :: Pos -> Bool -> Arity -> Exp TokenId
combApplyArity pos traced a =
  ExpVar pos ((if traced then tokenApp else tokenUApp) a)

combFun :: Pos -> Bool -> Arity -> Exp TokenId
combFun pos traced a = 
  testArity pos (if traced then 15 else 8) a 
    "function definition with more than " " arguments."
    (ExpVar pos ((if traced then tokenFun else tokenUFun) a))

combConstUse :: Pos -> Bool -> Exp TokenId
combConstUse pos traced = 
  ExpVar pos (if traced then tokenConstUse else tokenUConstUse)

combConstDef :: Pos -> Bool -> Exp TokenId 
combConstDef pos traced = 
  ExpVar pos (if traced then tokenConstDef else tokenUConstDef)

combGuard :: Pos -> Bool -> Exp TokenId
combGuard pos traced =
  ExpVar pos (if traced then tokenGuard else tokenUGuard)

combIf :: Pos -> Bool -> Exp TokenId
combIf pos traced =
  ExpVar pos (if traced then tokenIf else tokenUIf)

combCase :: Pos -> Bool -> Exp TokenId
combCase pos traced =
  ExpVar pos (if traced then tokenCase else tokenUCase)

combCon :: Pos -> Arity -> Exp TokenId
combCon pos arity =
  testArity pos 15 arity "application of constructor to more than "
    " arguments."
    (ExpVar pos (tokenCon arity))

combPartial :: Pos -> Arity -> Exp TokenId
combPartial pos arity =
  testArity pos 8 arity "partial application of constructor to more than "
    " arguments."
    (ExpVar pos (tokenPa arity))

combCn :: Pos -> Arity -> Exp TokenId
combCn pos arity =
  testArity pos 12 arity "partial application of constructor with more than "
    " missing arguments."
    (ExpVar pos (tokenCn arity))  

combUpdate :: Pos -> Bool -> Arity -> Exp TokenId
combUpdate pos traced arity = 
  testArity pos (if traced then 10 else maxBound) arity 
    "field update with more than " " labels."
    (ExpVar pos (if traced then tokenUpdate arity else tokenUUpdate))

testArity :: Pos -> Arity -> Arity -> String -> String -> a -> a
testArity pos maxArity arity str1 str2 x =
  if arity > maxArity 
    then error ("Cannot handle " ++ str1 ++ show maxArity ++ str2
                ++ "\nAttempted arity " ++ show arity ++ " at " ++ strPos pos)
    else x

-- apply data constructor R
wrapExp :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
wrapExp pos ev et = ExpApplication pos [ExpCon pos tokenR,ev,et]

-- hardwired tokens:
-- constants are here just for sharing

tracingModule :: PackedString		-- name of module with combinators
tracingModule = packString . reverse $ "Hat.Hat"

tracingModuleShort :: PackedString
tracingModuleShort = packString . reverse $ "T"  -- abbreviation

tPreludeModule :: PackedString
tPreludeModule = packString . reverse $ "Hat.PreludeBasic"

mkTPreludeToken :: String -> TokenId
mkTPreludeToken s = Qualified tPreludeModule (packString . reverse $ s)

mkTracingToken :: String -> TokenId
mkTracingToken s = Qualified tracingModuleShort (packString . reverse $ s)

mkTracingTokenArity :: String -> Arity -> TokenId
mkTracingTokenArity s a = mkTracingToken (s ++ show a)

mkTypeToken :: String -> TokenId
mkTypeToken s@"fromId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toChar" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromChar" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toInt" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromInt" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toInteger" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromInteger" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toFloat" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromFloat" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toDouble" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromDouble" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s = Visible (packString . reverse $ s)


-- tokens for trace constructors:

tokenMkRoot :: TokenId
tokenMkRoot = mkTracingToken "mkRoot"

tokenR :: TokenId
tokenR = mkTracingToken "R"

tokenMkModule :: TokenId
tokenMkModule = mkTracingToken "mkModule"

tokenMkPos :: TokenId
tokenMkPos = mkTracingToken "mkSrcPos" 

tokenMkNoPos :: TokenId
tokenMkNoPos = mkTracingToken "mkNoSrcPos"

tokenMkAtomConstructor :: Bool -> TokenId
tokenMkAtomConstructor withFields = 
  mkTracingToken 
    (if withFields then "mkConstructorWFields" else "mkConstructor")

tokenMkAtomVariable :: TokenId
tokenMkAtomVariable = mkTracingToken "mkVariable"

tokenMkExpValueApp :: Arity -> TokenId
tokenMkExpValueApp = mkTracingTokenArity "mkValueApp"

tokenMkExpValueUse :: TokenId
tokenMkExpValueUse = mkTracingToken "mkValueUse"

tokenMkAtomRational :: TokenId
tokenMkAtomRational = mkTracingToken "mkAtomRational"

tokenMkAtomLambda :: TokenId
tokenMkAtomLambda = mkTracingToken "mkLambda"

tokenMkAtomDoLambda :: TokenId
tokenMkAtomDoLambda = mkTracingToken "mkDoLambda"

-- tokens for expression combinators:

tokenAp :: Arity -> TokenId
tokenAp = mkTracingTokenArity "ap" 
tokenUAp :: Arity -> TokenId
tokenUAp = mkTracingTokenArity "uap"

tokenApp :: Arity -> TokenId
tokenApp = mkTracingTokenArity "app" 
tokenUApp :: Arity -> TokenId
tokenUApp = mkTracingTokenArity "uapp"

tokenFun :: Arity -> TokenId
tokenFun = mkTracingTokenArity "fun"
tokenUFun :: Arity -> TokenId
tokenUFun = mkTracingTokenArity "ufun"

tokenCon :: Arity -> TokenId
tokenCon = mkTracingTokenArity "con"

tokenPa :: Arity -> TokenId
tokenPa = mkTracingTokenArity "pa"

tokenCn :: Arity -> TokenId
tokenCn = mkTracingTokenArity "cn"

tokenConstUse :: TokenId
tokenConstUse = mkTracingToken "constUse"
tokenUConstUse :: TokenId
tokenUConstUse = mkTracingToken "uconstUse"

tokenConstDef :: TokenId
tokenConstDef = mkTracingToken "constDef"
tokenUConstDef :: TokenId
tokenUConstDef = mkTracingToken "uconstDef"

tokenGuard :: TokenId
tokenGuard = mkTracingToken "cguard"
tokenUGuard :: TokenId
tokenUGuard = mkTracingToken "ucguard"

tokenIf :: TokenId
tokenIf = mkTracingToken "cif"
tokenUIf :: TokenId
tokenUIf = mkTracingToken "ucif"

tokenCase :: TokenId
tokenCase = mkTracingToken "ccase"
tokenUCase :: TokenId
tokenUCase = mkTracingToken "uccase"

tokenUpdate :: Arity -> TokenId
tokenUpdate = mkTracingTokenArity "update"
tokenUUpdate :: TokenId
tokenUUpdate = mkTracingToken "uupdate"

tokenProjection :: TokenId
tokenProjection = mkTracingToken "projection"

tokenConChar :: TokenId
tokenConChar = mkTracingToken "conChar"

tokenConInteger :: TokenId
tokenConInteger = mkTracingToken "conInteger"

tokenFromLitString :: TokenId
tokenFromLitString = mkTracingToken "fromLitString"
tokenFromExpList :: TokenId
tokenFromExpList = mkTracingToken "fromExpList"

tokenWrapValClass :: TokenId
tokenWrapValClass = mkTracingToken "WrapVal"
tokenWrapValFun :: TokenId
tokenWrapValFun = mkTracingToken "wrapVal"

tokenUWrapForward :: TokenId
tokenUWrapForward = mkTracingToken "uwrapForward"

-- tokens of the Prelude

tokenUndefined :: TokenId
tokenUndefined = mkTPreludeToken "gundefined"

-- for integer literals
tokenFromInteger :: TokenId
tokenFromInteger = mkTPreludeToken "gfromInteger"

-- for rational literals
tokenConRational :: TokenId
tokenConRational = mkTPreludeToken ":%"
tokenFromRational :: TokenId
tokenFromRational = mkTPreludeToken "gfromRational"

-- function for pattern-match failure error message
tokenFatal :: TokenId
tokenFatal = mkTracingToken "fatal"

-- other hardcoded tokens:

tokenTraceIO :: TokenId
tokenTraceIO = mkTracingToken "traceIO"

tokenRefSrcPos :: TokenId
tokenRefSrcPos = mkTracingToken "RefSrcPos"

tokenRefExp :: TokenId
tokenRefExp = mkTracingToken "RefExp"

-- ----------------------------------------------------------------------------

expTo :: Pos -> Type TraceId -> Exp TokenId
expTo = expType True

expFrom :: Pos -> Type TraceId -> Exp TokenId
expFrom = expType False

expType :: Bool -> Pos -> Type TraceId -> Exp TokenId
expType to pos (TypeVar _ tyId) = 
  ExpVar pos (mkTypeToken (prefix to ++ "Id"))
expType to pos (TypeCons _ tyCon []) = 
  ExpVar pos (mkTypeToken (prefix to ++ typeName tyCon))
expType to pos (TypeCons _ tyCon [ty1,ty2]) | isFunTyCon tyCon =
  ExpApplication pos 
    [ExpVar pos (mkTypeToken (prefix to ++ "Fun")) 
    ,expType (not to) pos ty1 
    ,expType to pos ty2] 
expType to pos (TypeCons _ tyCon tys) = 
  ExpApplication pos 
    (ExpVar pos (mkTypeToken (prefix to ++ typeName tyCon)) 
    : map (expType to pos) tys)

prefix :: Bool -> String
prefix True = "to"
prefix False = "from"

typeName :: TraceId -> String
typeName aId = 
  case tokenId aId of
    TupleId n               -> "Tuple" ++ show n
    t | eqPredefined "[]" t -> "List"
    _                       -> getUnqualified aId

-- ----------------------------------------------------------------------------
-- various little helper functions

extractUnqual :: TokenId -> String
extractUnqual = reverse . unpackPS . extractV


eqPredefinedTrace :: String -> TraceId -> Bool
eqPredefinedTrace s id = eqPredefined s (tokenId id)

eqPredefined :: String -> TokenId -> Bool
eqPredefined s id = 
  -- a bit of a hack
  -- without qualification and even with qualification "Prelude"
  -- the token does not necessarily originate from Prelude
  -- but we pretend it does
  ((==) s . extractUnqual $ id) && 
    ((==) "Prelude" . qualModule . extractM $ id)

-- test for specific tokens

isFunTyCon :: TraceId -> Bool
isFunTyCon = eqPredefinedTrace "->"
-- (tokenId id) == t_Arrow

isTrue :: TraceId -> Bool
isTrue = eqPredefinedTrace "True"
-- (tokenId id) == tTrue

isOtherwise :: TraceId -> Bool
isOtherwise = eqPredefinedTrace "otherwise"
-- (tokenId id) == t_otherwise

isMain :: TraceId -> Bool
isMain = eqPredefinedTrace "Main"
-- (tokenId id) == tMain

-- other stuff

mkFailExp :: Pos -> Exp TokenId -> Exp TokenId
mkFailExp pos parent = ExpApplication pos [ExpVar pos tokenFatal,parent]

mkTupleExp :: Pos -> [Exp TokenId] -> Exp TokenId
mkTupleExp pos es = ExpApplication pos (ExpCon pos (t_Tuple (length es)): es)


-- ----------------------------------------------------------------------------

instance Functor Module where 
  fmap f (Module pos id mayExports imps fixs decls) =
    Module pos (f id) (fmap (map (fmap f)) mayExports) (map (fmap f) imps) 
      (map (mapDeclFixity f) fixs) (fmap f decls)

instance Functor Export where
  fmap f (ExportEntity pos entity) = ExportEntity pos (fmap f entity)
  fmap f (ExportModid pos id) = ExportModid pos (f id)

instance Functor ImpDecl where
  fmap f (Import (pos,id) impSpec) = Import (pos,f id) (fmap f impSpec)
  fmap f (ImportQ (pos,id) impSpec) = ImportQ (pos,f id) (fmap f impSpec)
  fmap f (ImportQas (pos,id) (pos2,id2) impSpec) =
    ImportQas (pos,f id) (pos2,f id2) (fmap f impSpec)
  fmap f (Importas (pos,id) (pos2,id2) impSpec) =
    Importas (pos,f id) (pos2,f id2) (fmap f impSpec)

instance Functor ImpSpec where
  fmap f (NoHiding entities) = NoHiding (map (fmap f) entities)
  fmap f (Hiding entities) = Hiding (map (fmap f) entities)

instance Functor Entity where
  fmap f (EntityVar pos id) = EntityVar pos (f id)
  fmap f (EntityConClsAll pos id) = EntityConClsAll pos (f id)
  fmap f (EntityConClsSome pos id pids) =
				EntityConClsSome pos (f id) (mapListSnd f pids)

instance Functor InfixClass where
  fmap f InfixDef = InfixDef
  fmap f InfixL = InfixL
  fmap f InfixR = InfixR
  fmap f Infix = Infix
  fmap f (InfixPre a) = InfixPre (f a)

mapDeclFixity :: (a -> b) -> FixDecl a -> FixDecl b
mapDeclFixity f (iclass,fix,fixIds) = (fmap f iclass,fix,map (fmap f) fixIds)

instance Functor FixId where
  fmap f (FixCon pos id) = FixCon pos (f id)
  fmap f (FixVar pos id) = FixVar pos (f id)

instance Functor Decls where
  fmap f (DeclsParse decls) = DeclsParse (map (fmap f) decls)
  fmap f (DeclsScc decldeps) = DeclsScc (map (fmap f) decldeps)

instance Functor DeclsDepend where
  fmap f (DeclsNoRec d) = DeclsNoRec (fmap f d)
  fmap f (DeclsRec ds) = DeclsRec (map (fmap f) ds)

instance Functor Decl where
  fmap f (DeclType simple ty) = DeclType (fmap f simple) (fmap f ty)
  fmap f (DeclTypeRenamed pos id) = DeclTypeRenamed pos id
  fmap f (DeclData sort contexts simple constrs derive) = 
    DeclData sort (map (fmap f) contexts) (fmap f simple) 
      (map (fmap f) constrs) (map (\(p,i)->(p,f i)) derive)
  fmap f (DeclDataPrim pos id size) = DeclDataPrim pos (f id) size
  fmap f (DeclConstrs pos id fields) =
    DeclConstrs pos (f id) (map (\(p,i1,i2)->(p,f i1,f i2)) fields)
  fmap f (DeclClass pos contexts cls tys fundeps decls) =
    DeclClass pos (map (fmap f) contexts) (f cls) (map f tys)
                  (map (fmap f) fundeps) (fmap f decls)
  fmap f (DeclInstance pos contexts cls insts decls) =
    DeclInstance pos (map (fmap f) contexts) (f cls) (map (fmap f) insts) 
      (fmap f decls)
  fmap f (DeclDefault tys) = DeclDefault (map (fmap f) tys)
  fmap f (DeclPrimitive pos id a ty) = DeclPrimitive pos (f id) a (fmap f ty)
  fmap f (DeclForeignImp pos callconv extfun id a fspec ty id') =
    DeclForeignImp pos callconv extfun (f id) a fspec (fmap f ty) (f id')
  fmap f (DeclVarsType vars contexts ty) =
    DeclVarsType (mapListSnd f vars) (map (fmap f) contexts) (fmap f ty)
  fmap f (DeclPat alt) = DeclPat (fmap f alt)
  fmap f (DeclFun pos id funs) = DeclFun pos (f id) (map (fmap f) funs)
  fmap f (DeclIgnore s) = DeclIgnore s
  fmap f (DeclError s) = DeclError s
  fmap f (DeclAnnot decl annots) = 
    DeclAnnot (fmap f decl) (map (fmap f) annots)
  fmap f (DeclFixity fixity) = 
    DeclFixity (mapDeclFixity f fixity)

instance Functor Annot where
  fmap f (AnnotArity (p,id) i) = AnnotArity (p,f id) i
  fmap f (AnnotPrimitive (p,id) s) = AnnotPrimitive (p, f id) s
  fmap f (AnnotNeed idss) = AnnotNeed (map (map f) idss)
  fmap f AnnotUnknown = AnnotUnknown

instance Functor FunDep where
  fmap f (as :->: bs) = (map f as) :->: (map f bs)

instance Functor Fun where
  fmap f (Fun pats rhs decls) = 
    Fun (map (fmap f) pats) (fmap f rhs) (fmap f decls)

instance Functor Alt where
  fmap f (Alt pat rhs decls) =
    Alt (fmap f pat) (fmap f rhs) (fmap f decls)

instance Functor Rhs where
  fmap f (Unguarded exp) = Unguarded (fmap f exp)
  fmap f (Guarded gdexps) = 
    Guarded (map (\(e1,e2)->(fmap f e1,fmap f e2)) gdexps)

instance Functor Type where
  fmap f (TypeCons pos id tys) = TypeCons pos (f id) (map (fmap f) tys)
  fmap f (TypeApp ty1 ty2) = TypeApp (fmap f ty1) (fmap f ty2)
  fmap f (TypeVar pos id) = TypeVar pos (f id)
  fmap f (TypeStrict pos ty) = TypeStrict pos (fmap f ty)

instance Functor Simple where
  fmap f (Simple pos id ids) = Simple pos (f id) (mapListSnd f ids)

instance Functor Context where
  fmap f (Context pos id pis) =
          Context pos (f id) (map (\(p,id2)->(p,f id2)) pis)

instance Functor Constr where
  fmap f (Constr pos id fields) = Constr pos (f id) (mapFields f fields)
  fmap f (ConstrCtx vars contexts pos id fields) =
    ConstrCtx (mapListSnd f vars) (map (fmap f) contexts) pos (f id) 
      (mapFields f fields)

mapFields :: (a -> b) 
          -> [(Maybe [(Pos,a)],Type a)] -> [(Maybe [(Pos,b)],Type b)]
mapFields f = map (\(may,ty)-> (fmap (mapListSnd f) may, fmap f ty))

instance Functor Stmt where
  fmap f (StmtExp exp) = StmtExp (fmap f exp)
  fmap f (StmtBind e1 e2) = StmtBind (fmap f e1) (fmap f e2)
  fmap f (StmtLet decls) = StmtLet (fmap f decls)

instance Functor Exp where
  fmap f (ExpScc s e) = ExpScc s (fmap f e)
  fmap f (ExpDict e) = ExpDict (fmap f e)
  fmap f (ExpLambda pos pats e) = ExpLambda pos (map (fmap f) pats) (fmap f e)
  fmap f (ExpLet pos decls e) = ExpLet pos (fmap f decls) (fmap f e)
  fmap f (ExpDo pos stmts) = ExpDo pos (map (fmap f) stmts)
  fmap f (ExpCase pos e alts) = ExpCase pos (fmap f e) (map (fmap f) alts)
  fmap f (ExpFatbar e1 e2) = ExpFatbar (fmap f e1) (fmap f e2)
  fmap f ExpFail = ExpFail
  fmap f (ExpIf pos e1 e2 e3) = ExpIf pos (fmap f e1) (fmap f e2) (fmap f e3)
  fmap f (ExpType pos e contexts ty) = 
    ExpType pos (fmap f e) (map (fmap f) contexts) (fmap f ty)
  fmap f (ExpRecord e fields) = ExpRecord (fmap f e) (map (fmap f) fields)
  fmap f (ExpApplication pos es) = ExpApplication pos (map (fmap f) es)
  fmap f (ExpVar pos id) = ExpVar pos (f id)
  fmap f (ExpCon pos id) = ExpCon pos (f id)
  fmap f (ExpInfixList pos es) = ExpInfixList pos (map (fmap f) es)
  fmap f (ExpVarOp pos id) = ExpVarOp pos (f id)
  fmap f (ExpConOp pos id) = ExpConOp pos (f id)
  fmap f (ExpLit pos lit) = ExpLit pos lit
  fmap f (ExpList pos es) = ExpList pos (map (fmap f) es)
  fmap f (Exp2 pos id1 id2) = Exp2 pos (f id1) (f id2)
  fmap f (PatAs pos id pat) = PatAs pos (f id) (fmap f pat)
  fmap f (PatWildcard pos) = PatWildcard pos
  fmap f (PatIrrefutable pos pat) = PatIrrefutable pos (fmap f pat)
  fmap f (PatNplusK pos id1 id2 e1 e2 e3) =
    PatNplusK pos (f id1) (f id2) (fmap f e1) (fmap f e2) (fmap f e3)

instance Functor Field where
  fmap f (FieldExp pos id e) = FieldExp pos (f id) (fmap f e)
  fmap f (FieldPun pos id) = FieldPun pos (f id)

-- ----------------------------------------------------------------------------
-- End
