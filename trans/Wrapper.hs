{- --------------------------------------------------------------------------- 
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
   * in each class instance, generate "f.i.h" functions for each method
   * delete all imports, except those that bring a needed type into scope
   * add a new import of the original untraced version of this module
   * delete all other decls

 We do need to chase imports to resolve references to (datatype)
 identifiers, but mainly so that we can delete most imports, keeping
 only those that bring needed datatypes into scope.  Thus, we end up
 with two import-chasing phases, once before wrapping, and another
 afterwards (much reduced).

 After preparation, the module can be passed through the normal tracing
 transformation, which already knows how to handle "foreign import haskell".
 (Except for the new type-wrapping functions, which get added without
 transformation.)

 *** Need to think about when to create .hx file.  In this traversal?
     Or do we gather imported environment as usual (after traversal?).

--------------------------------------------------------------------------- -}
module Wrapper
  ( prepareWrapping
  ) where

import Data.Maybe
import Syntax
import TokenId
import Extra

prepareWrapping :: String	-- filename of module
		-> Module TokenId
		-> Module TokenId

prepareWrapping filename
    (Module pos modId exports impDecls fixDecls decls)
  =  Module pos modId exports impDecls'  []     decls'
  where impDecls' = mkImportOriginal modId : impDecls
        decls'    = traverse modId exports decls

mkImportOriginal :: TokenId -> ImpDecl TokenId
mkImportOriginal modId =
  ImportQ (noPos, mkUnqualifiedTokenId ("NotHat."++show modId)) (Hiding [])

traverse :: TokenId -> Maybe [Export TokenId] -> Decls TokenId -> Decls TokenId
traverse modId exports (DeclsParse ds) = DeclsParse (concatMap walk ds)
  where
    walk (DeclType lhs rhs) = []				-- nyi
    walk (DeclClass pos ctxs cid tyvars fundeps decls) = []	-- nyi
    walk (DeclInstance pos ctxs cid insts decls) = []		-- nyi
    walk (DeclData newty ctxs lhs constrs posids) | exported simple lhs =
          DeclData Nothing [] lhs [newconstr] [] : []
      where simple (Simple _ tycon tyvars) = Just tycon
            newconstr = Constr noPos (fromJust (simple lhs))
                                     [(Nothing, qualify (simpleToType lhs))]
            qualify (TypeCons pos tcId tvars) =
                     TypeCons pos (mkQualifiedTokenId (show modId) (show tcId))
                                  tvars
    walk (DeclForeignImp pos conv cname var int safe typ hname)
         | exported Just var = mkForeignHaskell modId [] typ (pos,var): []
    walk (DeclVarsType posids ctxs typ) | not (null vars) =
          map (mkForeignHaskell modId ctxs typ) vars
      where vars = filter (exported (\ (_,t)->Just t)) posids
    walk (DeclPat alt) = []		-- delete pattern decls
    walk (DeclFun pos var funs) = []	-- delete function decls
    walk (DeclFixity (fixity,prec,vars)) | not (null expvars) =
          DeclFixity (fixity,prec,expvars): []
      where expvars = filter (exported varop) vars
            varop (FixVar _ id) = Just id
            varop _             = Nothing
    walk _ = []

    exported :: (a -> Maybe TokenId) -> a -> Bool
    exported f x = case exports of
                     Nothing   -> True
                     Just exps -> case f x of
                                    Nothing -> False
                                    Just v  -> (v `within`) `any` exps
      where tid `within` (ExportEntity _ (EntityVar _ t))       = tid==t
            tid `within` (ExportEntity _ (EntityConClsAll _ t)) = tid==t
            tid `within` (ExportEntity _ (EntityConClsSome _ t ss)) =
					 tid==t || tid `elem` map snd ss
            tid `within` (ExportModid _ _) = False	-- inaccurate...

-- TO DO: merge ctxs into typ for FFI signature
mkForeignHaskell :: TokenId -> [Context TokenId] -> Type TokenId
                 -> (Pos,TokenId) -> Decl TokenId
mkForeignHaskell mod ctxs typ (pos,var) =
    DeclForeignImp pos Haskell (show mod++'.':show var) var 0 Safe typ var
