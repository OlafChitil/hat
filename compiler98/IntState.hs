{- ---------------------------------------------------------------------------
Internal state of the compiler 
used from the renaming pass until code generation
-}
module IntState(module IntState, module Info) where

import AssocTree
import NT
import TokenId(mkQual3,mkQualD,dropM)
import Extra
import SysDeps(PackedString,packString)
import Info
import MergeSort(group)
import Reduce(Reduce)
import Id(Id)


data IntState = 
      IntState
	Int				-- unique
	(Int,PackedString)		-- modid
	(AssocTree Int Info)		-- symboltable (unique int -> info)
	[String]			-- errors

dummyIntState = IntState 0 (0,packString "<Dummy>") initAT []

-- -===== State

getInfo :: Id -> a -> IntState -> (Info,IntState)

getInfo i down state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just info -> (info,state)


addDefaultMethod :: a -> (Id,Id) -> IntState -> IntState

addDefaultMethod tidcls (iMethod,iDefault) state@(IntState unique irps@(_,rps) st errors) =
  case lookupIS state iMethod of
    Just (InfoMethod u tid ie fix nt' annot iClass) ->
      IntState unique irps 
        (addAT st fstOf iDefault 
          (InfoDMethod  iDefault (mkQualD rps tid) nt' annot iClass)) 
        errors


getUnique :: a -> IntState -> (Id,IntState)

getUnique down state@(IntState unique rps st errors) =
  (unique,IntState (unique+1) rps st errors)


addInstMethod :: TokenId -> TokenId -> TokenId -> NewType -> Id 
              -> a -> IntState -> (Int,IntState)

addInstMethod  tidcls tidtyp tidMethod nt iMethod down state@(IntState unique rps st errors) =
  case lookupIS state iMethod of
    Just (InfoMethod u tid ie fix nt' (Just arity) iClass) -> 
      (unique,IntState (unique+1) rps (addAT st (error "adding twice!") unique (InfoIMethod unique (mkQual3 tidcls tidtyp tidMethod) nt (Just arity) iMethod)) errors)

-- -====== State0

updInstMethodNT :: TokenId -> TokenId -> Int -> NewType -> Int 
                -> a -> IntState -> IntState

updInstMethodNT tidcls tidtyp i nt iMethod  down state@(IntState unique rps st errors) =
  case lookupAT st iMethod of
    Just (InfoMethod _ _ _ _ _ annots _) ->
      case lookupAT st i of
	Just (InfoIMethod u tid' _ _ _) ->
	    let tid = mkQual3 tidcls tidtyp (dropM tid')
	    in IntState unique rps (addAT st fstOf i (InfoIMethod u tid nt annots iMethod)) errors


addInstance :: Int -> Int -> [Int] -> [(Int,Int)] -> a -> IntState -> IntState

addInstance cls con free ctxs down state@(IntState unique rps st errors) =
  let st' = updateAT st cls (addInstanceI con free ctxs)
  in  IntState unique rps st' errors


addNewLetBound :: Int -> TokenId -> a -> IntState -> IntState

addNewLetBound i tid down state =
  addIS i (InfoVar i tid IEnone (InfixDef,9) NoType Nothing) state

-- -==== Reduce

{-
Adds type of variable to symbol table of internal state.
Assumes that variable is already in symbol table.
Adds error message, if there exists already a type for the variable
in the symbol table or type is badly formed (duplicate predicates in context). 
-}
updVarNT :: Pos -> Int -> NewType -> Reduce IntState IntState

updVarNT pos i nt state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just (InfoVar u tid exp fix NoType annots) ->
      case checkNT pos (strIS state) nt of
        Nothing -> IntState unique rps 
                     (addAT st fstOf i (InfoVar u tid exp fix nt annots)) 
                     errors
	Just err -> IntState unique rps st (err :errors)
    Just (InfoVar u tid exp fix nt' annots) ->
      IntState unique rps st 
        (("New type signature for " ++ show tid ++ " at " ++ strPos pos)
         : errors)


{-
Adds arity of variable to symbol table of internal state 
(any old arity is overwritten).
Assumes that variable is already in symbol table.
-}
updVarArity :: Pos -> Int -> Int -> Reduce IntState IntState

updVarArity pos i arity state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just (InfoVar  u tid exp fix nt _) ->  
      -- Always update, might change arity for redefined import in Prelude
      IntState unique rps 
        (addAT st fstOf i (InfoVar u tid exp fix nt (Just arity))) errors
    _ -> state   
      -- Ignore arity for methods, methods instances and methods default


-- -==== Stand alone

{- Add new info for identifier to the symbol table -}
addIS :: Int -> Info -> IntState -> IntState

addIS u info state@(IntState unique rps st errors) =
  IntState unique rps (addAT st combInfo u info) errors


{- Lookup identifier in symbol table -} 
lookupIS :: IntState -> Id -> Maybe Info

lookupIS (IntState unique rps st errors) i = lookupAT st i


{- Update info for identifier in symbol table -}
updateIS :: IntState -> Id -> (Info -> Info) -> IntState

updateIS (IntState unique rps st errors) i upd =
  IntState unique rps (updateAT st i upd) errors


{- Obtain a new unique and hence also a new internal state -}
uniqueIS :: IntState -> (Id,IntState)

uniqueIS (IntState unique rps st errors) = (unique,IntState (unique+1) rps st errors)


{- Associate new uniques with given list of entities; hence also new internal
state -}
uniqueISs :: IntState -> [a] -> ([(a,Int)],IntState)

uniqueISs (IntState unique rps st errors) l =
   (zip l [unique..],IntState (unique+(length l)) rps st errors)


{- Give printable string for identifier -}
strIS :: IntState -> Int -> String

strIS state i =
   case lookupIS state i of
     Just info -> show (tidI info)
     Nothing -> 'v':show i


{- Give token of identifier -}
tidIS :: IntState -> Id -> TokenId

tidIS state i =
   case lookupIS state i of
     Just info -> tidI info

getErrors :: IntState -> (IntState,[String])
getErrors (IntState unique rps st errors) = (IntState unique rps st [], errors)

getErrorsIS :: IntState -> Either [String] IntState
getErrorsIS is@(IntState unique rps st errors)
    | null errors = Right is
    | otherwise   = Left errors

addError :: IntState -> [Char] -> IntState
addError (IntState unique rps st errors) err = 
  IntState unique rps st (err:errors)

getSymbolTable :: IntState -> AssocTree Int Info
getSymbolTable (IntState unique rps st errors) = st

mrpsIS (IntState unique (i,rps) st errors) = rps
miIS (IntState unique (i,rps) st errors) = i

getIndDataIS state indDataI =
  case constrsI indDataI of
    (c:_) ->   -- Can only be one constructor 
      case lookupIS state c of
	(Just infoCon) ->
          case ntI infoCon of
	    (NewType ctx free [] (NTcons con _ _:_)) -> con


globalIS :: IntState -> Id -> Bool

globalIS state i =
  case lookupIS state i of
    Nothing -> False
    Just info -> globalI info
  where
  globalI (InfoData   unique tid ie nt dk) = isExported ie
  globalI (InfoClass  unique tid ie nt ms ds insts) = isExported ie
  globalI (InfoVar     unique tid IEsel fix nt annot) = True
  globalI (InfoVar     unique tid ie fix nt annot) = isExported ie
  globalI (InfoConstr  unique tid ie fix nt fields iType) = 
    globalI' (lookupIS state iType) 
  globalI (InfoField   unique tid ie icon_offs iData iSel) = 
    globalI' (lookupIS state iData) 
  globalI (InfoMethod  unique tid ie fix nt annot iClass) = True
  globalI (InfoIMethod  unique tid nt annot iMethod) = True
  globalI (InfoDMethod  unique tid nt annot iClass) = True
  globalI (InfoName  unique tid arity ptid _) = False --PHtprof

  globalI' (Just (InfoData   unique tid IEall nt dk)) = True
  globalI' _ = False


arityIS :: IntState -> Id -> Int
-- arity with context

arityIS state i =
  case lookupIS state i of 
    Just (InfoIMethod  unique tid (NewType _ [] ctxs [NTcons tcon _ _]) (Just arity) iMethod) ->
	case lookupIS state iMethod of
	  Just (InfoMethod  unique tid ie fix (NewType _ [] ictxs _) (Just iarity) iClass) ->
	       length ictxs + iarity + (length . snd . dropJust . lookupAT ((instancesI . dropJust . lookupIS state) iClass)) tcon

--	       length ctxs + arity + (length . snd . dropJust . lookupAT ((instancesI . dropJust . lookupIS state) iClass)) tcon
--	       length ictxs + length ctxs + arity 
    Just info@(InfoIMethod  unique tid _ _ iMethod) -> error ("arityIS " ++ show info)
    Just info -> arityI info
    _ -> error ("arityIS in IntState.hs couldn't find " ++ show i)


{-
Tests if context of type has duplicate predicates.
Second argument converts class identifier to string.
*** This module not a good place for this function.
-}
checkNT :: Pos -> (Int -> String) -> NewType -> Maybe String

checkNT pos strFun (NewType free [] ctxs nts) =
  case (filter ((1/=) . length) . group) ctxs of
    [] -> Nothing
    [x] -> Just ("Multiple occurences of " ++ (strFun . fst . head) x ++
                 " with identical type variable in context close to " ++ 
                 strPos pos) 
    xs -> Just ("Multiple occurences of " 
                ++ mixCommaAnd (map (strFun . fst . head) xs) ++
                " with identical type variables in context close to " ++ 
                strPos pos) 

{- End Module IntState ------------------------------------------------------}
