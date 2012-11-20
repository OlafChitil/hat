-- Derive instances of standard classes.
-- Thus given a type declaration with a non-empty derive clause,
-- produce all the instances demanded.
module Derive (derive) where

import Language.Haskell.Exts.Annotated 
import Wired (qNamePreludeIdent
             ,mkExpPreludeEqualEqual,mkExpPreludeAndAnd,mkExpTrue,mkExpFalse)
import SynHelp (Id(getId),appN,tyAppN,litInt,conDeclName,conDeclArity
               ,instHeadQName,declHeadName,declHeadTyVarBinds,tyVarBind2Type
               ,combineMaybeContexts)

-- ----------------------------------------------------------------------------

-- Derive instances for all given classes for a data/newtype
derive :: Decl l -> [Decl l]
derive (DataDecl l dataOrNew maybeContext declHead qualConDecls maybeDeriving) =
  case maybeDeriving of
    Nothing -> []
    Just (Deriving _ instHeads) -> 
      map (deriveClass l maybeContext instTy tyVars conDecls . instHeadQName) 
        instHeads
      where
      tyVars = map tyVarBind2Type (declHeadTyVarBinds declHead)
      nameTy = declHeadName declHead
      instTy = tyAppN (TyCon l (UnQual l nameTy) : tyVars)
      conDecls = map getConDecl qualConDecls
        

getConDecl :: QualConDecl l -> ConDecl l
getConDecl (QualConDecl _ Nothing Nothing conDecl) = conDecl
getConDecl (QualConDecl _ _ _ _) = 
  error "Derive.getConDecl: Cannot derive class instance for existentially quantified data constructor."


-- Produce a class instance.
deriveClass :: l -> 
  Maybe (Context l) -> -- context of the data type (should be empty)
  (Type l) ->          -- type constructor with variable args to be made instance
  [Type l] ->          -- type variables args of above
  [ConDecl l] ->       -- constructor of data type
  QName l ->           -- names of class to derive
  Decl l
deriveClass l maybeContext instTy tyVars conDecls className 
  | getId className == "Eq" = deriveEq l maybeContext' instTy conDecls 
  | getId className == "Ord" = deriveEq l maybeContext' instTy conDecls
  | otherwise = error "Derive.deriveClass: unknown class"
  where
  -- this is a HACK that covers only the common cases
  -- for correct result would need to implement full context reduction
  -- and take the least fixpoint
  maybeContext' = 
    combineMaybeContexts maybeContext
      (Just (CxTuple l (map (\ty -> ClassA l className [ty]) tyVars)))


-- ----------------------------------------------------------------------------

deriveEq :: l -> Maybe (Context l) -> (Type l) -> [ConDecl l] -> Decl l
deriveEq l maybeContext instTy conDecls =
  InstDecl l maybeContext (IHead l (qNamePreludeIdent "Eq" l) [instTy]) 
    (Just [InsDecl l (FunBind l (
      map matchEqConstr conDecls ++
      [Match l (Symbol l "==") [PWildCard l, PWildCard l] 
        (UnGuardedRhs l (mkExpFalse l))
        Nothing]))])
  where
  names = newNames l
  -- mkExpEqual :: Exp l -> Exp l -> Exp l
  mkExpEqual e1 e2 = App l (App l (mkExpPreludeEqualEqual l) e1) e2
  -- matchEqConstr :: ConDecl l -> Match l
  matchEqConstr conDecl =
    Match l (Symbol l "==") 
      [PApp l (UnQual l conName) patALs, PApp l (UnQual l conName) patARs] 
      (UnGuardedRhs l 
        (foldr mkExpAnd (mkExpTrue l) (zipWith mkExpEqual expALs expARs)))
      Nothing
    where
    conName = conDeclName conDecl
    arity = conDeclArity conDecl
    (namesL, namesRest) = splitAt arity names
    namesR = take arity namesRest
    patALs = map (PVar l) namesL
    patARs = map (PVar l) namesR
    expALs = map (Var l . UnQual l) namesL
    expARs = map (Var l . UnQual l) namesR
    

-- ----------------------------------------------------------------------------

deriveOrd :: l -> Maybe (Context l) -> (Type l) -> [ConDecl l] -> Decl l
deriveOrd l maybeContext instTy conDecls =
  InstDecl l maybeContext (IHead l (qNamePreludeIdent "Ord" l) [instTy])
    (Just [InsDecl l (FunBind l (
      concatMap matchCompareEqConstr conDecls ++
      [Match l nameCompare [PVar l nameL, PVar l nameR]
        (UnGuardedRhs l
          (App l 
             (App l (Var l (qNamePreludeIdent "compare" l))
                (App l (Var l (UnQual l nameLocalFromEnum))
                   (Var l (UnQual l nameL))))
             (App l (Var l (UnQual l nameLocalFromEnum))
                (Var l (UnQual l nameR)))))
        (Just (BDecls l [FunBind l (zipWith matchLocalFromEnum conDecls [0..])]))
      ]))])
  where
  nameL : nameR : names = newNames l
  nameCompare = Ident l "compare"
  nameLocalFromEnum = Ident l "localFromEnum"
  matchCompareEqConstr conDecl =
    if arity == 0 then [] else
      [Match l nameCompare 
        [PApp l (UnQual l conName) patALs, PApp l (UnQual l conName) patARs] 
      (UnGuardedRhs l 
        (foldr1 mkExpCase (zipWith mkExpCompare expALs expARs)))
      Nothing]
    where
    conName = conDeclName conDecl
    arity = conDeclArity conDecl
    (namesL, namesRest) = splitAt arity names
    namesR = take arity namesRest
    patALs = map (PVar l) namesL
    patARs = map (PVar l) namesR
    expALs = map (Var l . UnQual l) namesL
    expARs = map (Var l . UnQual l) namesR
  -- mkExpCase :: Exp l -> Exp l -> Exp l
  mkExpCase e1 e2 =
    Case l e1 
      [Alt l (PApp l (qNamePreludeIdent "EQ" l) []) (UnGuardedAlt l e2) Nothing
      ,Alt l (PVar l nameL) (UnGuardedAlt l (Var l (UnQual l nameL))) Nothing]
  -- mkExpCompare :: Exp l -> Exp l -> Exp l
  mkExpCompare e1 e2 =
    App l (App l (Var l (qNamePreludeIdent "compare" l)) e1) e2
  -- matchLocalFromEnum :: ConDecl l -> Int -> Match l
  matchLocalFromEnum conDecl num =
    Match l nameLocalFromEnum [PApp l (UnQual l conName) args] 
      (UnGuardedRhs l (litInt l num)) Nothing
    where
    conName = conDeclName conDecl
    args = replicate (conDeclArity conDecl) (PWildCard l)

-- ----------------------------------------------------------------------------

deriveBounded :: l -> Maybe (Context l) -> (Type l) -> [ConDecl l] -> Decl l
deriveBounded l maybeContext instTy conDecls =
  InstDecl l maybeContext (IHead l (qNamePreludeIdent "Bounded" l) [instTy])
    (if all (== 0) (map conDeclArity conDecls)
      then -- all constructors have no arguments (enumeration)
        (Just 
          [InsDecl l (PatBind l 
            (PVar l (Ident l "minBound")) Nothing
            (UnGuardedRhs l (Con l (UnQual l (conDeclName (head conDecls)))))
            Nothing)
          ,InsDecl l (PatBind l 
            (PVar l (Ident l "maxBound")) Nothing
            (UnGuardedRhs l (Con l (UnQual l (conDeclName (last conDecls)))))
            Nothing)])
      else -- exactly one constructor
        let [conDecl] = conDecls in
          (Just
            [InsDecl l (PatBind l 
              (PVar l (Ident l "minBound")) Nothing
              (UnGuardedRhs l 
                (appN 
                  (Con l (UnQual l (conDeclName conDecl))
                  :replicate (conDeclArity conDecl) 
                    (Var l (qNamePreludeIdent "minBound" l)))))
              Nothing)
            ,InsDecl l (PatBind l 
              (PVar l (Ident l "maxBound")) Nothing
              (UnGuardedRhs l 
                (appN
                  (Con l (UnQual l (conDeclName conDecl))
                  :replicate (conDeclArity conDecl)
                    (Var l (qNamePreludeIdent "maxBound" l)))))
              Nothing)]))





-- ----------------------------------------------------------------------------

-- Infinite list of parameter names in derived code.
-- Only need to avoid conflict with names of the methods of derived classes.
newNames :: l -> [Name l]
newNames l = map (Ident l . ('y':) . show) [1..]

-- syntax helpers:

mkExpAnd :: Exp l -> Exp l -> Exp l
mkExpAnd e1 e2 = App l (App l (mkExpPreludeAndAnd l) e1) e2
  where 
  l = ann e1

