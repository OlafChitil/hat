module SyntaxPos(Pos,HasPos(..)) where

import Extra(Pos,noPos,mergePos,mergePoss)
import Syntax

class HasPos a where
        getPos :: a -> Pos

instance HasPos (Decls a) where
     getPos (DeclsParse decls) = getPos decls
     getPos (DeclsScc decls) = getPos decls

instance HasPos (DeclsDepend a) where
     getPos (DeclsNoRec decl) = getPos decl
     getPos (DeclsRec   decls) = getPos decls

instance HasPos (Decl a) where
  getPos (DeclType simple ty) = mergePos (getPos simple) (getPos ty)
  getPos (DeclDataPrim pos _ _) = pos
  getPos (DeclData _ ctx simple constrs derives) = 
    mergePoss [getPos ctx,getPos simple,getPos constrs,getPosList derives]
  getPos (DeclConstrs pos _ _) = pos
  getPos (DeclClass pos _ _ _ _ _) = pos
  getPos (DeclInstance pos _ _ _ _) = pos
  getPos (DeclDefault tys) = getPos tys
  getPos (DeclVarsType ((pos,_):_) _ ty) = mergePos pos (getPos ty)
  getPos (DeclFun pos fun funs)       = pos
  getPos (DeclPrimitive pos fun a t)  = pos
  getPos (DeclForeignImp pos _ s fun a c t _) = pos
  getPos (DeclForeignExp pos _ s fun t) = pos
  getPos (DeclPat alt)                = getPos alt
  getPos (DeclIgnore str)             = noPos
  getPos (DeclError str)              = noPos

instance HasPos (Entity a) where
    getPos (EntityVar        pos _)   = pos
    getPos (EntityConClsAll  pos _)   = pos
    getPos (EntityConClsSome pos _ _) = pos

instance HasPos (Alt a) where
    getPos (Alt pat rhs locals) = 
      mergePoss [getPos pat,getPos rhs,getPos locals]

instance HasPos (Fun a) where
    getPos (Fun pats rhs locals) = 
      mergePoss [getPos pats,getPos rhs,getPos locals]

instance HasPos (Rhs a) where
    getPos (Unguarded e) = getPos e
    getPos (Guarded gdes) = 
      mergePos (getPos (fst (head gdes))) (getPos (snd (last gdes)))

instance HasPos (Stmt a) where
  getPos (StmtExp exp) = getPos exp
  getPos (StmtBind pat exp) = mergePos (getPos pat) (getPos exp)
  getPos (StmtLet decls) = getPos decls

instance HasPos (Exp a) where
  getPos (ExpDict        exp)       = getPos exp
  getPos (ExpScc         str exp)   = getPos exp
  getPos (ExpLambda      pos _ _)   = pos
  getPos (ExpLet         pos _ _)   = pos
  getPos (ExpDo 	 pos _)	    = pos
  getPos (ExpCase        pos _ _)   = pos
  getPos (ExpFail)	 	    = error "No position for ExpFail"
  getPos (ExpIf          pos _ _ _) = pos
  getPos (ExpType        pos _ _ _) = pos
  getPos (ExpRecord      exp fdefs) = mergePos (getPos exp) (getPos fdefs)
  getPos (ExpApplication pos _ )    = pos
  getPos (ExpInfixList   pos _)     = pos
  getPos (ExpVar         pos _)     = pos
  getPos (ExpCon         pos _)     = pos
  getPos (ExpVarOp       pos _)     = pos
  getPos (ExpConOp       pos _)     = pos
  getPos (ExpLit         pos _)     = pos
  getPos (ExpList        pos _)     = pos
  getPos (Exp2           pos i1 i2) = pos
  getPos (PatAs          pos _ _)   = pos
  getPos (PatWildcard    pos)       = pos
  getPos (PatIrrefutable pos _)     = pos
  getPos (PatNplusK      pos _ _ _ _ _) = pos


instance HasPos a => HasPos [a] where
    -- assumes that first and last element have proper positions
    getPos [] = noPos
    getPos xs = mergePos (getPos (head xs)) (getPos (last xs))

instance (HasPos a,HasPos b) => HasPos (a,b) where  -- used on GdExp
    getPos (a,b) = mergePos (getPos a) (getPos b)

instance HasPos (Simple a) where
    getPos (Simple pos _ _) = pos

instance HasPos (Type a) where
    getPos (TypeApp  t1 t2) = mergePos (getPos t1) (getPos t2)
    -- pos is position of constructor, not whole type, which shall be returned
    getPos (TypeCons pos _ (t:ts)) = mergePos (min pos (getPos t)) (getPos ts)
    getPos (TypeCons pos _ ts) = mergePos pos (getPos ts)
    getPos (TypeVar   pos _)   = pos
    getPos (TypeStrict  pos _)   = pos

instance HasPos (Context a) where
    getPos (Context pos _ _) = pos

instance HasPos (FixId a) where
    getPos (FixCon pos a) = pos
    getPos (FixVar pos a) = pos

instance HasPos (Field a) where
    getPos (FieldExp pos _ _) = pos
    getPos (FieldPun pos _) = pos

instance HasPos (Constr a) where
    getPos (Constr pos _ _) = pos
    getPos (ConstrCtx _ _ pos _ _) = pos


getPosList :: [(Pos,a)] -> Pos
getPosList [] = noPos
getPosList xs = mergePos (fst (head xs)) (fst (last xs))