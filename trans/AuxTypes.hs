module AuxTypes where

import Data.Char (isDigit,isUpper)
import Data.List (isPrefixOf)
import Syntax
import AssocTree
import TokenId (TokenId(Qualified),tPrelude,visImport,t_Tuple)
import SysDeps (packString)

-- AuxiliaryInfo is the extra information we need to know about identifiers.
data AuxiliaryInfo = 
         Value {- variable or constructor -}
	   { args     :: Int
	   , fixity   :: Fixity
	   , priority :: Int
	   , letBound :: Bool 
           , traced   :: Bool}
       | TyCls TyCls -- needed for im/export of (..)
       deriving (Show,Read)
data Fixity = L | R | Pre String | Def | None deriving (Eq,Show,Read)
data TyCls = Ty [String]{- data constructors -} [String]{- field labels -}
           | Cls [String]{- methods -}
           | Syn Int {- no. helper type syns -} TySynBody
  deriving (Show,Read)
data TySynBody = 
  TApp TySynBody TySynBody | TFun | THelper | TVar Int {- arg no. -}
  deriving (Show,Read)


-- information needed for creating an AuixiliaryInfo
-- currently only flag if original identifier definition is traced
newtype InitAuxInfo = InitAuxInfo Bool

defaultAuxInfo :: InitAuxInfo
defaultAuxInfo = InitAuxInfo False

initAuxInfo :: Bool {-traced-} -> InitAuxInfo
initAuxInfo = InitAuxInfo

untracedEmptyAux :: AuxiliaryInfo
untracedEmptyAux =
  Value { args=(-1), fixity=Def, priority=9, letBound=True, traced=False}

emptyAux :: InitAuxInfo -> AuxiliaryInfo
emptyAux (InitAuxInfo tr) = 
  Value { args=(-1), fixity=Def, priority=9, letBound=True, traced=tr}

patternAux :: InitAuxInfo -> AuxiliaryInfo
patternAux (InitAuxInfo tr) = 
  Value { args=(-1), fixity=Def, priority=9, letBound=False, traced=tr}

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con TypeSort String{-type-} String{-con-}
		| Field String{-type-} String{-field-}
		| Method String{-class-} String{-method-}
                | TypeClass String
	deriving (Show,Read,Eq,Ord)
data TypeSort = Data | Newtype deriving (Show,Read,Eq,Ord)

qual :: Identifier -> String -> Identifier
qual (Var v) m = Var (m++v)
qual (Con s t c) m = Con s t (m++c)
qual (Field t f) m = Field t (m++f)
qual (Method t f) m = Method t (m++f)
qual (TypeClass t) _ = TypeClass t

subTid :: Identifier -> TokenId
subTid (Var v)        = qualTid v
subTid (Con t _ c)    = possTuple c
subTid (Field t f)    = qualTid f
subTid (Method c m)   = qualTid m
subTid (TypeClass tc) = possTuple tc

qualTid i | isUpper (head i) && '.' `elem` i =
                  let (v,m) = break (=='.') (reverse i) in	-- approximation
                  Qualified (packString (tail m)) (packString v)
          | otherwise = visImport i

possTuple "()" = t_Tuple 0
possTuple s | "Prelude." `isPrefixOf` s =
    let nm = drop 8 s in
    if isDigit (head nm) then t_Tuple (read nm)
    else visImport s
possTuple s = visImport s

-- The main Environment is composed of two pieces...
type Environment = (AuxTree,IdentMap)

-- AuxTree is an environment, associating each identifier with a unique
-- AuxiliaryInfo.
type AuxTree = AssocTree Identifier AuxiliaryInfo

-- IdentMap is an environment associating each constructor/field with
-- its type, and each method with its class.  We can encounter a
-- constructor (or method) without its type (or class) in a fixity decl,
-- but we then need to know its type (or class) to know whether it
-- is exported or not.  If an entity is neither a known constructor/field
-- nor a known method, we assume it is just an ordinary variable.
-- Types and classes are not stored in an IdentMap
type IdentMap = AssocTree TokenId{-con, var, or method-} Identifier

-- `mkIdentMap' makes a little lookup table from data constructors and field
-- names to their type name, and methods to their class.  Additionally, it
-- builds a list of all defined types, plus synonyms and class names, used
-- to check that all exports have a referent.
mkIdentMap :: [Decl TokenId] -> (IdentMap,[TokenId])
mkIdentMap decls =
    let dataDecls  = concatMap dataDecl decls
        classDecls = concatMap classDecl decls
    in ( foldr addMethod (foldr addCon initAT dataDecls) classDecls
       , map (\(x,_,_)->x) dataDecls ++ map fst classDecls 
         ++ concatMap typeSyn decls)
  where
    dataDecl (DeclData Nothing _ (Simple _ typ _) tycons _)  = 
      [(typ,Newtype,tycons)]
    dataDecl (DeclData (Just _) _ (Simple _ typ _) tycons _)  = 
      [(typ,Data,tycons)]
    dataDecl _ = []

    classDecl (DeclClass _ _ cls _ _ (DeclsParse decls)) = [(cls,decls)]
    classDecl _ = []

    typeSyn (DeclType (Simple pos id vars) _) = [id]
    typeSyn _ = []

    addCon :: (TokenId,TypeSort,[Constr TokenId]) -> IdentMap -> IdentMap
    addCon (typ,typeSort,tycons) t = foldr doCon t tycons
	where
        doCon (Constr _ c fs) t        = conAndFields c fs t
        doCon (ConstrCtx _ _ _ c fs) t = conAndFields c fs t
        conAndFields c fs t = addFields (addAT t const c
                                          (Con typeSort styp (show c)))
                                          styp fs
        styp = show typ

    addFields :: IdentMap -> String -> [(Maybe [(t, TokenId)],b)] -> IdentMap
    addFields t typ [] = t
    addFields t typ ((Nothing,_):_) = t
    addFields t typ ((Just posids,_):cs) = foldr doField (rest t) posids
        where
            doField (_,f) t = addAT t const f (Field typ (show f))
            rest t = addFields t typ cs

    addMethod :: (TokenId,[Decl TokenId]) -> IdentMap -> IdentMap
    addMethod (cls, decls) t = foldr doMethod t decls
	where
	    doMethod (DeclVarsType pis ctxs typ) t = foldr pId t pis
	    doMethod _ t = t
	    pId (pos,meth) t = addAT t const meth
                                     (Method (show cls) (show meth))

useIdentMap :: IdentMap -> TokenId -> Identifier
useIdentMap m v =
      case lookupAT m v of
	Just tc -> tc
	Nothing -> Var (show v)

-- The following hack is needed, because identMap only knows about global
-- identifiers and because variables, field labels and methods share the
-- same name space.
useEnvironment :: Environment -> TokenId -> Maybe AuxiliaryInfo
useEnvironment (env,identMap) id =
  let v = useIdentMap identMap id in
  case v of
    -- a field or method may be shadowed by a normal variable
    -- hence have to search for such a variable first
    Field _ name -> case lookupAT env (Var name) of
                      Just info -> Just info
                      Nothing -> lookupAT env v
    Method _ name -> case lookupAT env (Var name) of
                       Just info -> Just info
                       Nothing -> lookupAT env v
    _ -> lookupAT env v
