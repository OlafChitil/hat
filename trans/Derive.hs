-- Derive instances of standard classes.
-- Thus given a type declaration with a non-empty derive clause,
-- produce all the instances demanded.
module Derive (derive) where

import Language.Haskell.Exts.Annotated 
import Wired (qNamePreludeEq,qNamePreludeOrd,qNamePreludeCompare
             ,mkExpPreludeEqualEqual,mkExpPreludeAndAnd,mkExpTrue,mkExpFalse)
import SynHelp (conDeclName,conDeclArity)

-- ----------------------------------------------------------------------------

-- derive instances for all given classes for a data/newtype
derive :: Decl l -> [Decl l]
derive (DataDecl l dataOrNew maybeContext declHead qualConDecls maybeDeriving) =
  case maybeDeriving of
    Nothing -> []
    Just (Deriving _ instHeads) -> 
      map (deriveClass l maybeContext     . instHeadQName) instHeads


-- derive instances for all given classes
deriveClasses :: l -> 
                 [Asst l] ->  -- context of the data type (should be empty)
                 Name l ->  -- data type name
                 [TyVarBind l] -> -- data type parameter variables
                 [ConDecl l] ->  -- constructors of data type
                 [QName l] ->  -- names of classes to derive
                 [Decl l]
deriveClasses l asst dataName tyVarBinds conDecls classNames =



deriveClass :: l -> Maybe (Context l) -> 
               (Type l) -> [Type l] -> [ConDecl l] -> QName l ->
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
      Just (CxTuple l (map (\ty -> ClassA l className [ty]) tyVars))


-- ----------------------------------------------------------------------------

deriveEq :: l -> Maybe (Context l) -> (Type l) -> [ConDecl l] -> Decl l
deriveEq l maybeContext instTy conDecls =
  InstDecl l maybeContext (IHead l (qNamePreludeEq l) [instTy]) 
    (Just (InsDecl l (FunBind l (
      map matchEqConstr conDecls ++
      Match l (Symbol l "==") [PWildCard l, PWildCard l] 
        (UnGuardedRhs (mkExpFalse l))
        Nothing))))
  where
  names = newNames l
  -- mkExpEqual :: Exp l -> Exp l -> Exp l
  mkExpEqual e1 e2 = App l (App l (mkExpPreludeEqualEqual l) e1) e2
  -- matchEqConstr :: ConDecl l -> Match l
  matchEqConstr conDecl =
    Match l (Symbol l "==") 
      [PApp l (UnQual l conName) [patAL], PApp l (UnQual l conName) [patAR]] 
      (UnGuardedRhs l 
        (foldr (mkExpAnd l) (mkExpTrue l) (zipWith mkExpEqual expAL expAR)))
      Nothing
    where
    conName = conDeclName conDecl
    arity = conDeclArity conDecl
    (namesL, namesRest) = splitAt arity names
    namesR = take arity namesRest
    patAL = map (PVar l) namesL
    patAR = map (PVar l) namesR
    expAL = map (Var l . UnQual l) namesL
    expAR = map (Var l . UnQual l) namesR
    

-- ----------------------------------------------------------------------------

-- Infinite list of parameter names in derived code.
-- Only need to avoid conflict with names of the methods of derived classes.
newNames :: l -> Name l
newNames l = map (Ident l . ('y':) . show) [1..]

mkExpAnd :: Exp l -> Exp l -> Exp l
mkExpAnd e1 e2 = App l (App l (mkExpPreludeAndAnd l) e1) e2
  where 
  l = ann e1

