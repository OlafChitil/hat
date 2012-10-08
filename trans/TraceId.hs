module TraceId
  ( TraceId		-- abstract type
  , Fixity(L,R,Pre,Def,None)
  , TyCls(Ty,Cls,Syn)
			-- constructors:
  , mkLambdaBound      	-- :: InitAuxInfo -> TokenId -> TraceId
  , mkModule      	-- :: InitAuxInfo -> TokenId -> TraceId
  , mkClass      	-- :: InitAuxInfo -> TokenId -> TraceId
  , mkTyVar      	-- :: InitAuxInfo -> TokenId -> TraceId
  , mkTyCon             -- :: InitAuxInfo -> TokenId -> TraceId
  , mkField             -- :: InitAuxInfo -> TokenId -> TraceId
  , plus		-- :: TokenId -> AuxiliaryInfo -> TraceId
                        -- modifiers
  , dropModule          -- :: TraceId -> TraceId
  , modLambdaBound      -- :: InitAuxInfo -> TraceId -> TraceId
  , modLetBound         -- :: TraceId -> TraceId
  , modArity            -- :: TraceId -> Int -> TraceId
			-- selectors:
  , tokenId		-- :: TraceId -> TokenId
  , arity		-- :: TraceId -> Maybe Int
  , isLambdaBound	-- :: TraceId -> Bool
  , fixPriority		-- :: TraceId -> Int
  , tFixity             -- :: TraceId -> Fixity
  , tPriority           -- :: TraceId -> Int {0-9}
  , getUnqualified      -- :: TraceId -> String
  , hasInfo             -- :: TraceId -> Bool
  , hasValueInfo        -- :: TraceId -> Bool
  , tyClsInfo           -- :: TraceId -> TyCls
  , isExpandableTypeSynonym -- :: TraceId -> Bool
  , typeSynonymBody     -- :: TraceId -> Maybe TySynBody
  , isTraced            -- :: TraceId -> Bool
  , tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
  , tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenGreater,tTokenMinus
  , tTokenTrue,tTokenFalse,tTokenEQ,tTokenCompare
  ,tTokenLocalFromEnum,tTokenInt,tTokenMinBound,tTokenMaxBound
  ,tTokenFromEnum,tTokenToEnum,tTokenEnumFrom,tTokenEnumFromThen
  ,tTokenEnumFromTo,tTokenEnumFromThenTo,tTokenError
  ,tTokenCompose,tTokenShowsPrec,tTokenShowParen,tTokenShowChar
  ,tTokenShowString,tTokenReadsPrec,tTokenReadParen,tTokenYield
  ,tTokenAlt,tTokenThenAp,tTokenThenLex,tTokenRange,tTokenIndex,tTokenInRange
  ,tTokenMap,tTokenLocalToEnum,tTokenTuple2,tTokenFun
  ,tTokenRangeSize,tTokenReturn,tTokenPlus,tTokenTimes -- :: TraceId
  ) where

import TokenId 
  (TokenId,mkQualifiedTokenId,extractV,dropM,t_Colon,t_List,t_gtgt,t_gtgteq
  ,tfail,t_andand,t_equalequal,t_greater,t_greaterequal,tminus,tTrue
  ,tFalse,tEQ,tcompare,visImport,tInt,tminBound,tmaxBound
  ,tfromEnum,ttoEnum,tenumFrom,tenumFromThen,tenumFromTo
  ,tenumFromThenTo,t_error,t_dot,tshowsPrec,tshowParen,tshowChar
  ,tshowString,treadsPrec,treadParen,trange,tindex,tinRange,t_Tuple,t_Arrow)
import AuxTypes (AuxiliaryInfo(..),InitAuxInfo(..),Fixity(..),TyCls(..)
                ,TySynBody,emptyAux,untracedEmptyAux)
import Data.Maybe (isJust)
import SysDeps (unpackPS)


data TraceId = TI TokenId (Maybe AuxiliaryInfo)

instance Eq TraceId where
  TI t1 _ == TI t2 _ = t1 == t2


-- construction functions

mkLambdaBound :: InitAuxInfo -> TokenId -> TraceId
mkLambdaBound (InitAuxInfo tr) t = 
  TI t (Just (Value{ args=(-1), fixity=Def, priority=9, letBound=False
		   , traced=tr}))

plus :: TokenId -> AuxiliaryInfo -> TraceId
t `plus` aux = TI t (Just aux)

mkModule :: InitAuxInfo -> TokenId -> TraceId
mkModule = mkLambdaBound

mkClass :: InitAuxInfo -> TokenId -> TraceId
mkClass = mkLambdaBound

mkTyVar :: InitAuxInfo -> TokenId -> TraceId
mkTyVar = mkLambdaBound

mkTyCon :: InitAuxInfo -> TokenId -> TraceId
mkTyCon = mkLambdaBound

mkField :: InitAuxInfo -> TokenId -> TraceId
mkField = mkLambdaBound



-- modification functions

-- drop qualifier
dropModule :: TraceId -> TraceId
dropModule (TI tokenId aux) = TI (dropM tokenId) aux

modLambdaBound :: InitAuxInfo -> TraceId -> TraceId
modLambdaBound iai (TI token (Just aux@Value{})) = 
  TI token (Just aux{letBound=False})
modLambdaBound iai (TI token _) = mkLambdaBound iai token

modLetBound :: TraceId -> TraceId
modLetBound (TI token (Just aux)) = TI token (Just aux{letBound=True,args=0})
modLetBound (TI token Nothing) = error "modLetBound"

modArity :: TraceId -> Int -> TraceId
modArity (TI token (Just aux)) arity = TI token (Just aux{args=arity})
modArity (TI token Nothing) arity = error "modArity"

-- selection functions

tokenId :: TraceId -> TokenId
tokenId (TI t _) = t

arity :: TraceId -> Maybe Int
arity (TI _ (Just Value{args=a})) = Just a
arity (TI _ _)  = Nothing
 
isLambdaBound :: TraceId -> Bool
isLambdaBound (TI _ (Just Value{letBound=b})) = not b
isLambdaBound (TI _ Nothing) = 
  error "TraceId.isLambdaBound: no aux information"

tFixity :: TraceId -> Fixity
tFixity (TI _ Nothing) = Def
tFixity (TI _ (Just info)) = fixity info

tPriority :: TraceId -> Int {- 0-9 -}
tPriority (TI _ Nothing) = 9
tPriority (TI _ (Just info)) = priority info

fixPriority :: TraceId -> Int
fixPriority (TI _ Nothing) = 3	-- default fixity and priority
fixPriority (TI _ (Just info)) = encode (fixity info) (priority info)
  where
    encode Def     _ = 3
    encode L       n = 2 + (n*4)
    encode R       n = 1 + (n*4)
    encode None    n = 0 + (n*4)
    encode (Pre _) n = 0 + (n*4)

getUnqualified :: TraceId -> String
getUnqualified = reverse . unpackPS . extractV . tokenId

hasInfo :: TraceId -> Bool
hasInfo (TI _ aux) = isJust aux

hasValueInfo :: TraceId -> Bool
hasValueInfo (TI _ (Just Value{})) = True
hasValueInfo (TI _ _) = False

tyClsInfo :: TraceId -> TyCls
tyClsInfo (TI _ (Just (TyCls tyCls))) = tyCls

isExpandableTypeSynonym :: TraceId -> Bool
isExpandableTypeSynonym (TI _ (Just (TyCls (Syn n _)))) = n > 0
isExpandableTypeSynonym _ = False

typeSynonymBody :: TraceId -> Maybe TySynBody
typeSynonymBody (TI _ (Just (TyCls (Syn _ body)))) = Just body
typeSynonymBody _ = Nothing

typeSynonymHelpers :: TraceId -> Maybe Int
typeSynonymHelpers (TI _ (Just (TyCls (Syn helpers _)))) = Just helpers
typeSynonymHelpers _ = Nothing

isTraced :: TraceId -> Bool
isTraced (TI _ (Just Value{traced=tr})) = tr

-- TraceId versions of some hardcoded tokens 

tTokenCons :: TraceId
tTokenCons = t_Colon `plus` untracedEmptyAux{args=2}

tTokenNil :: TraceId
tTokenNil = t_List `plus` untracedEmptyAux{args=0}

tTokenGtGt :: TraceId
tTokenGtGt = t_gtgt `plus` untracedEmptyAux

tTokenGtGtEq :: TraceId
tTokenGtGtEq = t_gtgteq `plus` untracedEmptyAux

tTokenFail :: TraceId
tTokenFail = tfail `plus` untracedEmptyAux

tTokenAndAnd :: TraceId
tTokenAndAnd = t_andand `plus` untracedEmptyAux{args=2}

tTokenEqualEqual :: TraceId
tTokenEqualEqual = t_equalequal `plus` untracedEmptyAux

tTokenGreater :: TraceId
tTokenGreater = t_greater `plus` untracedEmptyAux

tTokenGreaterEqual :: TraceId
tTokenGreaterEqual = t_greaterequal `plus` untracedEmptyAux

tTokenMinus :: TraceId
tTokenMinus = tminus `plus` untracedEmptyAux

tTokenTrue :: TraceId
tTokenTrue = tTrue `plus` untracedEmptyAux{args=0}

tTokenFalse :: TraceId
tTokenFalse = tFalse `plus` untracedEmptyAux{args=0}

tTokenEQ :: TraceId
tTokenEQ = tEQ `plus` untracedEmptyAux{args=0}

tTokenCompare :: TraceId
tTokenCompare = tcompare `plus` untracedEmptyAux

tTokenLocalFromEnum :: TraceId
tTokenLocalFromEnum = visImport "localFromEnum" `plus` untracedEmptyAux{args=1}

tTokenInt :: TraceId
tTokenInt = tInt `plus` untracedEmptyAux

tTokenMinBound :: TraceId
tTokenMinBound = tminBound `plus` untracedEmptyAux

tTokenMaxBound :: TraceId
tTokenMaxBound = tmaxBound `plus` untracedEmptyAux

tTokenFromEnum :: TraceId
tTokenFromEnum = tfromEnum `plus` untracedEmptyAux

tTokenToEnum :: TraceId
tTokenToEnum = ttoEnum `plus` untracedEmptyAux

tTokenEnumFrom :: TraceId
tTokenEnumFrom = tenumFrom `plus` untracedEmptyAux

tTokenEnumFromTo :: TraceId
tTokenEnumFromTo = tenumFromTo `plus` untracedEmptyAux

tTokenEnumFromThen :: TraceId
tTokenEnumFromThen = tenumFromThen `plus` untracedEmptyAux

tTokenEnumFromThenTo :: TraceId
tTokenEnumFromThenTo = tenumFromThenTo `plus` untracedEmptyAux

tTokenError :: TraceId
tTokenError = t_error `plus` untracedEmptyAux{args=1}

tTokenCompose :: TraceId
tTokenCompose = t_dot `plus` untracedEmptyAux{args=3}

tTokenShowsPrec :: TraceId
tTokenShowsPrec = tshowsPrec `plus` untracedEmptyAux

tTokenShowParen :: TraceId
tTokenShowParen = tshowParen `plus` untracedEmptyAux{args=2}

tTokenShowString :: TraceId
tTokenShowString = tshowString `plus` untracedEmptyAux{args=0}

tTokenShowChar :: TraceId
tTokenShowChar = tshowChar `plus` untracedEmptyAux{args=0}

tTokenReadsPrec :: TraceId
tTokenReadsPrec = treadsPrec `plus` untracedEmptyAux

tTokenReadParen :: TraceId
tTokenReadParen = treadParen `plus` untracedEmptyAux{args=2}

tTokenYield :: TraceId
tTokenYield = 
  mkQualifiedTokenId "PreludeBasic" "yield" `plus` untracedEmptyAux{args=2}

tTokenAlt :: TraceId
tTokenAlt = 
  mkQualifiedTokenId "PreludeBasic" "alt" `plus` untracedEmptyAux{args=3}

tTokenThenLex :: TraceId
tTokenThenLex = 
  mkQualifiedTokenId "PreludeBasic" "thenLex" `plus` untracedEmptyAux{args=2}

tTokenThenAp :: TraceId
tTokenThenAp = 
  mkQualifiedTokenId "PreludeBasic" "thenAp" `plus` untracedEmptyAux{args=0}

tTokenRange :: TraceId
tTokenRange = trange `plus` untracedEmptyAux

tTokenIndex :: TraceId
tTokenIndex = tindex `plus` untracedEmptyAux

tTokenInRange :: TraceId
tTokenInRange = tinRange `plus` untracedEmptyAux

tTokenMap :: TraceId
tTokenMap = 
  mkQualifiedTokenId "PreludeBasic" "map" `plus` untracedEmptyAux{args=2}

tTokenLocalToEnum :: TraceId
tTokenLocalToEnum = visImport "localToEnum" `plus` untracedEmptyAux{args=1}

tTokenTuple2 :: TraceId
tTokenTuple2 = t_Tuple 2 `plus` untracedEmptyAux{args=2}

tTokenFun :: TraceId
tTokenFun = t_Arrow `plus` untracedEmptyAux

tTokenRangeSize :: TraceId
tTokenRangeSize = mkQualifiedTokenId "Ix" "rangeSize" `plus` untracedEmptyAux

tTokenReturn :: TraceId
tTokenReturn = mkQualifiedTokenId "PreludeBasic" "return" `plus` untracedEmptyAux

tTokenPlus :: TraceId
tTokenPlus = mkQualifiedTokenId "PreludeBasic" "+" `plus` untracedEmptyAux

tTokenTimes :: TraceId
tTokenTimes = mkQualifiedTokenId "PreludeBasic" "*" `plus` untracedEmptyAux
