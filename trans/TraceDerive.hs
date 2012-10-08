module TraceDerive where

import Syntax
import TraceId 
  (TraceId,mkLambdaBound,tokenId,getUnqualified,dropModule,modArity
  ,tPriority,tFixity,Fixity(..)
  ,tTokenTrue,tTokenFalse,tTokenEqualEqual,tTokenAndAnd
  ,tTokenEQ,tTokenCompare,tTokenLocalFromEnum,tTokenInt
  ,tTokenMinBound,tTokenMaxBound,tTokenFromEnum,tTokenToEnum
  ,tTokenError,tTokenEnumFrom,tTokenEnumFromThen,tTokenEnumFromTo
  ,tTokenEnumFromThenTo,tTokenGreater,tTokenGreaterEqual,tTokenCompose
  ,tTokenShowsPrec,tTokenShowParen,tTokenShowString,tTokenShowChar
  ,tTokenReadsPrec,tTokenReadParen,tTokenYield,tTokenAlt,tTokenThenAp
  ,tTokenThenLex,tTokenRange,tTokenIndex,tTokenInRange,tTokenMap,tTokenMinus
  ,tTokenLocalToEnum,tTokenTuple2,tTokenFun,tTokenRangeSize
  ,tTokenGtGtEq,tTokenPlus,tTokenTimes,tTokenReturn)
import TokenId (mkUnqualifiedTokenId,tEq)
import AuxTypes (defaultAuxInfo)

-- ----------------------------------------------------------------------------

-- derive instances for all given classes for a data/newtype
derive :: [Context TraceId] -> Simple TraceId -> [Constr TraceId] 
       -> [(Pos,TraceId)] -> [Decl TraceId]
derive tyContexts sTy constrs = 
  map (deriveClass tyContexts (simpleToType sTy) pTyVars constrs)
  where
  Simple _ _ pTyVars = sTy

-- derive instance for a given class for a data/newtype
deriveClass :: [Context TraceId] -> Instance TraceId -> [(Pos,TraceId)] 
            -> [Constr TraceId] -> (Pos,TraceId) -> Decl TraceId
deriveClass tyContexts instTy pTyVars constrs (pos,cls) 
  | getUnqualified cls == "Eq" = deriveEq pos usualContexts cls instTy constrs
  | getUnqualified cls == "Ord" = 
    deriveOrd pos usualContexts cls instTy constrs
  | getUnqualified cls == "Bounded" =
    deriveBounded pos usualContexts cls instTy constrs
  | getUnqualified cls == "Enum" = 
    deriveEnum pos usualContexts cls instTy constrs
  | getUnqualified cls == "Read" = 
    deriveRead pos usualContexts cls instTy constrs
  | getUnqualified cls == "Show" = 
    deriveShow pos usualContexts cls instTy constrs
  | getUnqualified cls == "Ix" = 
    deriveIx pos usualContexts cls instTy constrs
  | otherwise = error ("deriveClass: unknown class " ++ show (tokenId cls))
  where
  -- this is a HACK that covers only the common cases
  -- for correct result would need to implement full context reduction
  -- and take the least fixpoint
  usualContexts = tyContexts ++ map (\pt-> Context pos cls [pt]) pTyVars

-- ----------------------------------------------------------------------------

deriveEq :: Pos 
         -> [Context TraceId] -> TraceId -> Instance TraceId 
         -> [Constr TraceId] 
         -> Decl TraceId
deriveEq pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (DeclsParse 
      [DeclFun pos (modArity (dropModule tTokenEqualEqual) 2)
        (map funEqConstr constrs ++ 
          [Fun [PatWildcard pos,PatWildcard pos] 
            (Unguarded (ExpCon pos tTokenFalse)) noDecls])])
  where
  vars = traceVars pos
  funEqConstr :: Constr TraceId -> Fun TraceId
  funEqConstr constr = 
    if arity == 0 
      then
        Fun [ExpCon pos conId,ExpCon pos conId] 
          (Unguarded (ExpCon pos tTokenTrue)) noDecls
      else
        Fun 
          [ExpApplication pos (ExpCon pos conId : argsL)
          , ExpApplication pos  (ExpCon pos conId : argsR)]
          (Unguarded (foldr1 andExp (zipWith equalExp argsL argsR)))
          noDecls
    where
    andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]
    equalExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    equalExp e1 e2 = ExpApplication pos [ExpVar pos tTokenEqualEqual,e1,e2]
    (argsL,restVars) = splitAt arity vars
    argsR = take arity restVars
    conId = getConstrId constr
    arity = constrArity constr

-- ----------------------------------------------------------------------------

deriveOrd :: Pos 
          -> [Context TraceId] -> TraceId -> Instance TraceId 
          -> [Constr TraceId] 
          -> Decl TraceId
deriveOrd pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (DeclsParse 
      [DeclFun pos (modArity (dropModule tTokenCompare) 2)
        (concatMap funCompareEqConstr constrs ++ 
          [Fun [var1,var2] 
            (Unguarded 
              (ExpApplication pos 
                [ExpVar pos tTokenCompare
                ,ExpType pos (ExpApplication pos [localFromEnumVar,var1])
                   [] (TypeCons pos tTokenInt [])
                ,ExpApplication pos [localFromEnumVar,var2]]))
            (DeclsParse 
              [DeclFun pos tTokenLocalFromEnum
                (zipWith funLocalFromEnum constrs [0..])])])])
  where
  var1:var2:vars = traceVars pos
  localFromEnumVar = ExpVar pos tTokenLocalFromEnum
  funCompareEqConstr constr = 
    if arity == 0 then []
      else
        [Fun
          [ExpApplication pos (ExpCon pos conId : argsL)
          ,ExpApplication pos (ExpCon pos conId : argsR)]
          (Unguarded (foldr1 caseExp (zipWith compareExp argsL argsR)))
          noDecls]
    where
    caseExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    caseExp e1 e2 = 
      ExpCase pos e1 
        [Alt (ExpCon pos tTokenEQ) (Unguarded e2) noDecls
        ,Alt var1 (Unguarded var1) noDecls]
    compareExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    compareExp e1 e2 = ExpApplication pos [ExpVar pos tTokenCompare,e1,e2]
    (argsL,restVars) = splitAt arity vars
    argsR = take arity restVars
    conId = getConstrId constr
    arity = constrArity constr
  funLocalFromEnum constr num =
    Fun [ExpApplication pos (ExpCon pos conId : args)]
      (Unguarded (ExpLit pos (LitInteger Boxed num)))
      noDecls
    where
    args = replicate (constrArity constr) (PatWildcard pos)
    conId = getConstrId constr


-- ----------------------------------------------------------------------------

deriveBounded :: Pos 
              -> [Context TraceId] -> TraceId -> Instance TraceId 
              -> [Constr TraceId] 
              -> Decl TraceId
deriveBounded pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (if all (== 0) (map constrArity constrs) 
      then
        (DeclsParse
          [DeclFun pos (modArity (dropModule tTokenMinBound) 0)
            [Fun [] (Unguarded (ExpCon pos (getConstrId (head constrs)))) 
              noDecls]
          ,DeclFun pos (modArity (dropModule tTokenMaxBound) 0)
            [Fun [] (Unguarded (ExpCon pos (getConstrId (last constrs)))) 
              noDecls]])
      else {- exactly one constructor -}
        let [constr] = constrs in
          (DeclsParse
            [DeclFun pos (modArity (dropModule tTokenMinBound) 0)
              [Fun [] (Unguarded 
                  (ExpApplication pos 
                    (ExpCon pos (getConstrId (head constrs)) 
                    : replicate (constrArity constr) 
                        (ExpVar pos tTokenMinBound)) )) 
              noDecls]
            ,DeclFun pos (modArity (dropModule tTokenMaxBound) 0)
              [Fun [] (Unguarded 
                  (ExpApplication pos 
                    (ExpCon pos (getConstrId (head constrs)) 
                    : replicate (constrArity constr) 
                        (ExpVar pos tTokenMaxBound)) )) 
              noDecls]]))

-- ----------------------------------------------------------------------------

deriveEnum :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveEnum pos contexts cls ty constrs =
  -- assert: all (== 0) (map constrArity constrs) 
  DeclInstance pos contexts cls [ty]
    (DeclsParse
      [DeclFun pos (modArity (dropModule tTokenFromEnum) 1)
        (zipWith funFromEnum constrs [0..])
      ,DeclFun pos (modArity (dropModule tTokenToEnum) 1)
        (zipWith funToEnum constrs [0..] ++ 
          [Fun [PatWildcard pos] (Unguarded 
            (ExpApplication pos 
              [ExpVar pos tTokenError
              ,ExpLit pos (LitString Boxed "toEnum: argument out of bounds")]))
            noDecls])
      ,DeclFun pos (modArity (dropModule tTokenEnumFrom) 1)
        [Fun [var1] (Unguarded 
          (ExpApplication pos
            [ExpVar pos tTokenEnumFromTo,var1
            ,ExpCon pos (getConstrId (last constrs))]))
          noDecls]
      ,DeclFun pos (modArity (dropModule tTokenEnumFromThen) 2)
        [Fun [var1,var2] (Unguarded
          (ExpApplication pos
            [ExpVar pos tTokenEnumFromThenTo,var1,var2
            ,ExpIf pos
              (ExpApplication pos 
                [ExpVar pos tTokenGreaterEqual
                ,ExpApplication pos [ExpVar pos tTokenFromEnum,var1]
                ,ExpApplication pos [ExpVar pos tTokenFromEnum,var2]])
              (ExpCon pos (getConstrId (last constrs)))
              (ExpCon pos (getConstrId (head constrs)))]))
          noDecls]
      ])
  where
  var1:var2:_ = traceVars pos
  funFromEnum constr num =
    Fun [ExpCon pos (getConstrId constr)] 
      (Unguarded (ExpLit pos (LitInteger Boxed num)))
      noDecls
  funToEnum constr num =
    Fun [ExpLit pos (LitInteger Boxed num)] 
      (Unguarded (ExpCon pos (getConstrId constr)))
      noDecls

-- ----------------------------------------------------------------------------

deriveShow :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveShow pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (DeclsParse
      [DeclFun pos (modArity (dropModule tTokenShowsPrec) 2)
        (map funShowsPrec constrs)])
  where
  precVar:vars = traceVars pos
  funShowsPrec constr =
    Fun [precVar,ExpApplication pos (ExpCon pos conId:args)] (Unguarded
      (if arity == 0 
         then showStringExp (getUnqualified conId)
         else
           case getUnqualified conId of
             ':':_ -> -- is defined in infix style (and arity == 2)
               ExpApplication pos 
                 [ExpVar pos tTokenShowParen
                 ,ExpApplication pos 
                   [ExpVar pos tTokenGreater,precVar
                   ,ExpLit pos 
                     (LitInteger Boxed priority)]
                 ,  showPrec priorityPlus1 (head args) 
                  `compose`
                    showStringExp (' ' : getUnqualified conId ++ " ")
                  `compose`
                    showPrec priorityPlus1 (args!!1)
                 ]
             _ ->
               let labels = map snd . getConstrLabels $ constr in
               if null labels 
                 then
                   ExpApplication pos 
                     [ExpVar pos tTokenShowParen
                     ,ExpApplication pos 
                       [ExpVar pos tTokenGreater,precVar
                       ,ExpLit pos (LitInteger Boxed priority)]
                     ,showStringExp (getUnqualified conId ++ " ")
                        `compose`
                       foldr1 composeSpace (map (showPrec priorityPlus1) args)
                     ]
                 else
                   showStringExp (getUnqualified conId ++ "{")
                     `compose`
                     foldr1 composeComma (zipWith showField labels args)
                     `compose`
                     showCharExp '}'
      ))
      noDecls
    where
    showStringExp s = 
      ExpApplication pos 
        [ExpVar pos tTokenShowString
        ,ExpLit pos (LitString Boxed s)]
    showCharExp c = 
      ExpApplication pos 
        [ExpVar pos tTokenShowChar
        ,ExpLit pos (LitChar Boxed c)]
    e1 `compose` e2 = ExpApplication pos [ExpVar pos tTokenCompose,e1,e2]
    e1 `composeSpace` e2 = e1 `compose` showCharExp ' ' `compose` e2
    e1 `composeComma` e2 = e1 `compose` showCharExp ',' `compose` e2
    showField label e = 
      showStringExp (getUnqualified label) `compose` showCharExp '=' 
        `compose` showPrec 0 e 
    showPrec d e = 
      ExpApplication pos 
        [ExpVar pos tTokenShowsPrec,ExpLit pos (LitInteger Boxed d),e]
    args = take arity vars
    conId = getConstrId constr
    arity = constrArity constr
    priority = toInteger (tPriority conId)
    priorityPlus1 = priority+1 

      
-- ----------------------------------------------------------------------------

deriveRead :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveRead pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (DeclsParse
      [DeclFun pos (modArity (dropModule tTokenReadsPrec) 1)
        [Fun [precVar] (Unguarded (foldr1 alt . map expReadsPrec $ constrs)) 
          noDecls]])
  where
  precVar:_ = traceVars pos
  e1 `alt` e2 = ExpApplication pos [ExpVar pos tTokenAlt,e1,e2]
  expReadsPrec constr =
    if arity == 0 
      then readParen (ExpCon pos tTokenFalse) 
             (yield (ExpCon pos conId) `thenLex` getUnqualified conId)
      else
        case getUnqualified conId of
          ':':_ -> -- is defined in infix style (and arity == 2)
            readParen precGreaterPriority
              (yield (ExpCon pos conId) `thenAp`
                readsArg `thenLex` getUnqualified conId `thenAp` readsArg)
          _ ->
           let labels = map snd . getConstrLabels $ constr in
           if null labels 
             then
               readParen precGreaterPriority
                 (foldl thenAp 
                    (yield (ExpCon pos conId) `thenLex` getUnqualified conId)
                    (replicate arity readsArg))
             else
               (foldl thenCommaField
                 (yield (ExpCon pos conId) `thenLex` getUnqualified conId 
                   `thenLex` "{" `thenField` (head labels))
                 (tail labels))
                 `thenLex` "}" 
    where
    infixl 6 `thenAp`,`thenLex`, `thenField`
    readParen c e = ExpApplication pos [ExpVar pos tTokenReadParen,c,e]
    yield e = ExpApplication pos [ExpVar pos tTokenYield,e]
    e1 `thenLex` s = ExpApplication pos [ExpVar pos tTokenThenLex,e1,string s]
    e1 `thenAp` e2 = ExpApplication pos [ExpVar pos tTokenThenAp,e1,e2]
    precGreaterPriority = 
      ExpApplication pos 
        [ExpVar pos tTokenGreater,precVar
        ,ExpLit pos (LitInteger Boxed priority)]
    string s = ExpLit pos (LitString Boxed s)
    readsArg = 
      ExpApplication pos 
        [ExpVar pos tTokenReadsPrec
        ,ExpLit pos (LitInteger Boxed priorityPlus1)]
    readsArg0 = 
      ExpApplication pos 
        [ExpVar pos tTokenReadsPrec
        ,ExpLit pos (LitInteger Boxed 0)]
    p `thenField` label = 
      p `thenLex` getUnqualified label `thenLex` "=" 
        `thenAp` readsArg0 
    p `thenCommaField` label = p `thenLex` "," `thenField` label
    conId = getConstrId constr
    arity = constrArity constr
    priority = toInteger (tPriority conId)
    priorityPlus1 = priority+1 

-- ----------------------------------------------------------------------------

deriveIx :: Pos 
         -> [Context TraceId] -> TraceId -> Instance TraceId 
         -> [Constr TraceId] 
         -> Decl TraceId
deriveIx pos contexts cls ty constrs =
  DeclInstance pos contexts cls [ty]
    (if all (== 0) (map constrArity constrs) 
      then
        (DeclsParse
          [DeclFun pos (modArity (dropModule tTokenRange) 1)
            [Fun [ExpApplication pos [ExpCon pos tTokenTuple2,lvar,uvar]] 
              (Unguarded 
                (ExpApplication pos 
                  [ExpVar pos tTokenMap,toEnumVar
                  ,ExpApplication pos 
                    [ExpVar pos tTokenEnumFromTo
                    ,ExpApplication pos [fromEnumVar,lvar]
                    ,ExpApplication pos [fromEnumVar,uvar]]]))
              (DeclsParse (declsToEnum ++ declsFromEnum))]
          ,DeclFun pos (modArity (dropModule tTokenIndex) 2)
            [Fun [ExpApplication pos [ExpCon pos tTokenTuple2,lvar,uvar],ivar] 
              (Unguarded 
                (ExpApplication pos 
                  [ExpVar pos tTokenMinus
                  ,ExpApplication pos [fromEnumVar,ivar]
                  ,ExpApplication pos [fromEnumVar,lvar]]))
              (DeclsParse declsFromEnum)]
          ,DeclFun pos (modArity (dropModule tTokenInRange) 2)
            [Fun [ExpApplication pos [ExpCon pos tTokenTuple2,lvar,uvar],ivar] 
              (Unguarded 
                (ExpApplication pos 
                  [ExpVar pos tTokenInRange
                  ,ExpApplication pos 
                    [ExpCon pos tTokenTuple2
                    ,ExpApplication pos [fromEnumVar,lvar]
                    ,ExpApplication pos [fromEnumVar,uvar]]
                  ,ExpApplication pos [fromEnumVar,ivar]]))
              (DeclsParse declsFromEnum)]
          ])
      else {- exactly one constructor -}
        (DeclsParse
          [DeclFun pos (modArity (dropModule tTokenRange) 1)
            [Fun 
              [ExpApplication pos [ExpCon pos tTokenTuple2,conLvars,conUvars]]
              (Unguarded 
                (foldr ($) 
                  (ExpApplication pos [ExpVar pos tTokenReturn,conIvars]) 
                  (zipWith3 rangeComb lvars uvars ivars)))
              noDecls]
          ,DeclFun pos (modArity (dropModule tTokenIndex) 2)
            [Fun 
              [ExpApplication pos [ExpCon pos tTokenTuple2,conLvars,conUvars]
              ,conIvars] 
              (Unguarded
                (foldl (flip ($))
                  (indexExp (head lvars) (head uvars) (head ivars)) 
                  (tail (zipWith3 indexComb lvars uvars ivars))))
              noDecls]
          ,DeclFun pos (modArity (dropModule tTokenInRange) 2)
            [Fun 
              [ExpApplication pos [ExpCon pos tTokenTuple2,conLvars,conUvars]
              ,conIvars] 
              (Unguarded
                (foldr1 andExp (zipWith3 inRangeExp lvars uvars ivars))) 
              noDecls]
          ]))
  where
  -- for enumeration type
  lvar:uvar:ivar:_ = traceVars pos
  fromEnumVar = ExpVar pos tTokenLocalFromEnum
  toEnumVar = ExpVar pos tTokenLocalToEnum
  declsFromEnum :: [Decl TraceId]
  declsFromEnum = 
    [DeclVarsType [(pos,tTokenLocalFromEnum)] [] 
      (TypeCons pos tTokenFun [ty,TypeCons pos tTokenInt []])
    ,DeclFun pos tTokenLocalFromEnum (zipWith funFromEnum constrs [0..])] 
  declsToEnum = 
    [DeclVarsType [(pos,tTokenLocalToEnum)] [] 
      (TypeCons pos tTokenFun [TypeCons pos tTokenInt [],ty])
    ,DeclFun pos tTokenLocalToEnum (zipWith funToEnum constrs [0..])]
  funFromEnum constr num =
    Fun [ExpCon pos (getConstrId constr)] 
      (Unguarded (ExpLit pos (LitInteger Boxed num))) noDecls
  funToEnum constr num =
    Fun [ExpLit pos (LitInteger Boxed num)] 
      (Unguarded (ExpCon pos (getConstrId constr))) noDecls
  -- for single constructor type
  [constr] = constrs
  conId = getConstrId constr
  arity = constrArity constr
  (lvars,vars1) = splitAt arity (traceVars pos)
  (uvars,vars2) = splitAt arity vars1
  ivars = take arity vars2
  conLvars = ExpApplication pos (ExpCon pos conId:lvars)
  conUvars = ExpApplication pos (ExpCon pos conId:uvars)
  conIvars = ExpApplication pos (ExpCon pos conId:ivars)
  rangeComb :: Exp TraceId -> Exp TraceId -> Exp TraceId -> Exp TraceId 
            -> Exp TraceId
  rangeComb l u i cont = 
    ExpApplication pos 
      [ExpVar pos tTokenGtGtEq
      ,ExpApplication pos 
        [ExpVar pos tTokenRange
        ,ExpApplication pos [ExpCon pos tTokenTuple2,l,u]]
      ,ExpLambda pos [i] cont]
  indexExp l u i =
    ExpApplication pos 
      [ExpVar pos tTokenIndex
      ,ExpApplication pos [ExpCon pos tTokenTuple2,l,u]
      ,i]
  indexComb :: Exp TraceId -> Exp TraceId -> Exp TraceId -> Exp TraceId 
            -> Exp TraceId
  indexComb l u i e =
    ExpApplication pos 
      [ExpVar pos tTokenPlus
      ,indexExp l u i
      ,ExpApplication pos 
        [ExpVar pos tTokenTimes
        ,ExpApplication pos 
          [ExpVar pos tTokenRangeSize
          ,ExpApplication pos [ExpCon pos tTokenTuple2,l,u]]
        ,e]]
  inRangeExp l u i =
    ExpApplication pos 
      [ExpVar pos tTokenInRange
      ,ExpApplication pos [ExpCon pos tTokenTuple2,l,u]
      ,i]
  andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
  andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]

-- ----------------------------------------------------------------------------
-- helper functions

-- infinite list of variables
-- only need not conflict with names of standard class methods
traceVars :: Pos -> [Exp TraceId]
traceVars pos = 
  map (ExpVar pos . mkLambdaBound defaultAuxInfo . mkUnqualifiedTokenId . 
        ('y':) . show) [1..]









