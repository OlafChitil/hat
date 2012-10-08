-- Tracing version of types that are not definable in Haskell
-- or are used by functions that are not definable in Haskell
module Hat.PreludeBuiltinTypes
  (Fun(Fun) -- reexported from Hat
  ,Bool(True,False),IOError 
  ,aTrue,aFalse
  ,String  -- here for convenience
  ,module Hat.PreludeBuiltinTypes
  ,gerror,gundefined
  ) where

import Hat.Hat as T
import Prelude hiding (IO,String)
import qualified Prelude

-- beside the types that cannot be defined within Haskell
-- also have to define here the types that are used by primitive functions:
-- Bool(,List),String,Tuple0,Tuple2


-- ----------------------------------------------------------------------------
-- types:

-- Bool constructors
aTrue = T.mkConstructor tPrelude 0 0 3 0 "True"
aFalse = T.mkConstructor tPrelude 0 0 3 0 "False"

type String = List Char


-- ----------------------------------------------------------------------------
-- type conversion functions:

-- function type is contravariant
toFun :: (RefExp -> c -> R a) -> (RefExp -> R b -> d) -> RefExp 
      -> R (Fun a b) -> (c -> d)
toFun f g h (R (Fun x) _) = g h . x h . f h 

-- function type is contravariant
fromFun :: (RefExp -> R a -> b) -> (RefExp -> c -> R d) -> RefExp
        -> (b -> c) -> R (Fun a d)
fromFun f g h x = R (Fun (const (g h . x . f h))) h 

toBool :: RefExp -> R Bool -> Bool
toBool h (R b _) = b

fromBool :: RefExp -> Bool -> R Bool
fromBool t b = con0 mkNoSrcPos t b (if b then aTrue else aFalse)


toList :: (RefExp -> R a -> b) -> RefExp -> R (List a) -> [b]
toList f h (R (Cons x xs) _) = f h x : toList f h xs
toList f h (R List _) = []

fromList :: (RefExp -> a -> R b) -> RefExp -> [a] -> R (List b)
fromList f h = fromList'
  where
  fromList' [] = con0 mkNoSrcPos h List aList
  fromList' (x:xs) = 
    con2 mkNoSrcPos h Cons aCons (T.wrapForward h (f h x)) 
      (T.wrapForward h (fromList' xs))

toString :: RefExp -> R String -> Prelude.String
toString = toList toChar

fromString :: RefExp -> Prelude.String -> R String
fromString = fromList fromChar

-- toPolyList :: R (List a) -> [R a]
-- toPolyList = toList id

-- fromPolyList :: RefExp -> [R a] -> R (List a)
-- fromPolyList = fromList (\_ x -> x)

toIOError :: RefExp -> R IOError -> Prelude.IOError
toIOError h (R e _) = e

fromIOError :: RefExp -> Prelude.IOError -> R IOError
fromIOError h e = R e (T.mkValueUse h mkNoSrcPos aIOError)

aIOError :: RefAtom
aIOError = T.mkAbstract "IOError"


-- primitive functions where the trace is handled specially

-- error calls exits with primitive function hatError

gerror perror jerror = T.fun1 aerror perror jerror herror 

herror :: R String -> RefExp -> a
herror z1error kerror = T.hatError kerror (toString kerror z1error)

aerror = T.mkVariable tPrelude 0 0 3 1 "error" Prelude.False

-- hack so that context of undefined is its proper parent 
gundefined :: T.RefSrcPos -> T.RefExp -> T.R a
gundefined pundefined p = cUse
  where
  cUse@(T.R _ cRef) = T.uconstUse pundefined p sundefined
  sundefined = T.uconstDef p aundefined (const (hundefined cRef))

-- gundefined pundefined jundefined = T.constDef jundefined aundefined hundefined

hundefined :: RefExp -> a
hundefined kundefined = T.hatError kundefined "Prelude.undefined"

aundefined = T.mkVariable tPrelude 0 0 3 0 "undefined" Prelude.False





