{-# LANGUAGE CPP #-}
{-# OPTIONS -#include "hat-c.h" #-}
-- ----------------------------------------------------------------------------
-- Library used by all programs transformed for tracing by Hat.
-- Combinators in Haskell and interfaces to C-functions.

module Hat.Hat
  (RefModule,RefSrcPos,RefExp,RefAtom
  ,Loc,Fixity,Arity
  ,R(R)
  ,tPrelude
  ,toId,fromId
  ,Char,Int,Integer,Float,Double
  ,toChar,fromChar,toInt,fromInt,toInteger,fromInteger,toFloat,fromFloat
  ,toDouble,fromDouble
  ,Fun(Fun),IO,toIO,fromIO
  ,Tuple0(Tuple0),aTuple0,Tuple2(Tuple2),aTuple2,Tuple3(Tuple3),aTuple3
  ,Tuple4(Tuple4),aTuple4,Tuple5(Tuple5),aTuple5,Tuple6(Tuple6),aTuple6
  ,Tuple7(Tuple7),aTuple7,Tuple8(Tuple8),aTuple8,Tuple9(Tuple9),aTuple9
  ,Tuple10(Tuple10),aTuple10,Tuple11(Tuple11),aTuple11
  ,Tuple12(Tuple12),aTuple12,Tuple13(Tuple13),aTuple13
  ,Tuple14(Tuple14),aTuple14,Tuple15(Tuple15),aTuple15
  ,toTuple0,fromTuple0,toTuple2,fromTuple2
  ,List(Cons,List),aCons,aList
  ,ap1,ap2,ap3,ap4,ap5,ap6,ap7,ap8,ap9,ap10,ap11,ap12,ap13,ap14,ap15
  ,fun1,fun2,fun3,fun4,fun5,fun6,fun7,fun8,fun9,fun10,fun11,fun12,fun13,fun14
  ,fun15
  ,uap1,uap2,uap3,uap4,uap5,uap6,uap7,uap8
  ,ufun1,ufun2,ufun3,ufun4,ufun5,ufun6,ufun7,ufun8
  ,app1,app2,app3,app4,app5,uapp1,uapp2,uapp3,uapp4,uapp5,uwrapForward
  ,con0,con1,con2,con3,con4,con5,con6,con7,con8,con9,con10,con11,con12,con13
  ,con14,con15,pa0,pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8
  ,cn1,cn2,cn3,cn4,cn5,cn6,cn7,cn8,cn9,cn10,cn11,cn12
  ,cguard,ucguard,cif,ucif,ccase,uccase
  ,constUse,uconstUse,constDef,uconstDef
  ,projection
  ,WrapVal(wrapVal),update1,update2,uupdate
  ,conChar,conInteger,mkAtomRational
  ,fromExpList,fromLitString
  ,traceIO,hatError,fatal,outputTrace
  ,wrapForward
  ,mkModule,mkSrcPos,mkNoSrcPos
  ,mkRoot,mkValueUse
  ,mkValueApp1,mkValueApp2,mkValueApp3,mkValueApp4,mkValueApp5,mkValueApp6
  ,mkValueApp7,mkValueApp8,mkValueApp9,mkValueApp10,mkValueApp11,mkValueApp12
  ,mkValueApp13,mkValueApp14,mkValueApp15
  ,mkApp1,mkApp2,mkApp3,mkApp4,mkApp5,mkApp6,mkApp7,mkApp8,mkApp9,mkApp10
  ,mkApp11,mkApp12,mkApp13,mkApp14,mkApp15
  ,mkLambda,mkVariable,mkConstructor,mkConstructorWFields,mkAbstract
  ,mkDoLambda
  ) where

-- hack for only compiler currently supported:
-- #define __GLASGOW_HASKELL__ 700

import Prelude hiding (IO,fromInteger,toInteger,catch)
import qualified Prelude (IO,fromInteger,toInteger)
import qualified System.IO.Error (ioeGetErrorString)
import Data.Ratio (numerator,denominator)
#if defined (__GLASGOW_HASKELL__) || defined(__HUGS__)
--import IOExts (unsafePerformIO,IORef,newIORef,readIORef,writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
#endif
#if defined(__NHC__)
#if __NHC__ >= 115
import NHC.IOExtras (unsafePerformIO,IORef,newIORef,readIORef,writeIORef)
#else
import IOExtras (unsafePerformIO,IORef,newIORef,readIORef,writeIORef)
#endif
#endif
#if defined (__GLASGOW_HASKELL__) || defined(__HUGS__)
-- import Storable(Storable)
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.C.String(CString,withCString)
#endif
#if defined(__NHC__)
#if __NHC__ >= 115
import NHC.FFI (Storable,Ptr,castPtr,withArray,CString,withCString) 
#else
import FFI (Storable,Ptr,castPtr,withArray,CString,withCString) 
#endif
#endif
#if defined(__GLASGOW_HASKELL__)
#  if  __GLASGOW_HASKELL__ >= 600
import Control.OldException (Exception,ioErrors,catch)  -- also catches black holes
#  else
import Exception (ioErrors,catch)  -- also catches black holes
#  endif
#endif
#if defined(__NHC__)
import Prelude (catch)
#endif

-- #if defined(__NHC__) && __NHC__ >= 115
-- #define FOREIGN(x) foreign import ccall unsafe x
-- #elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 504
-- #define FOREIGN(x) foreign import ccall unsafe x
-- #else
-- #define FOREIGN(x) foreign import ccall x unsafe
-- #endif
-- Haskell 2010 standard:
#define FOREIGN(x) foreign import ccall unsafe x


-- ----------------------------------------------------------------------------
-- Types of the ART trace

newtype RefModule = RM Int
newtype RefSrcPos = RSP Int
newtype RefExp = RT Int deriving Eq -- a hidden Exp has a set bit
newtype RefAtom = RA Int

mkNoSrcPos :: RefSrcPos
mkNoSrcPos = RSP 0

type Loc = Int  -- line * 10000 + column
type Fixity = Int
type Arity = Int

-- ----------------------------------------------------------------------------
-- The wrapper type

-- Invariant: the trace argument of R is always fully evaluated.
-- Trace arguments that are passed to functions are always fully evaluated.
-- No, not for pap: pap will force evaluation immediately.
data R a = R a RefExp

-- data constructor R strict in trace argument
mkR :: a -> RefExp -> R a
mkR x t = t `seq` R x t

refExp :: R a -> RefExp
refExp (R a t) = t

forceExp :: R a -> R a
forceExp r@(R _ t) = t `seq` r

-- ----------------------------------------------------------------------------
-- transformed primitive types (undefinable and with special syntax)
-- with conversion functions

-- module name:
tPrelude = mkModule "Prelude" "Prelude.hs" False


toId :: RefExp -> R a -> R a
toId h x = x

fromId :: RefExp -> R a -> R a
fromId h x = x

toChar :: RefExp -> R Char -> Prelude.Char 
toChar p (R c _) = c

fromChar :: RefExp -> Prelude.Char -> R Char
fromChar h c = conChar mkNoSrcPos h c

toInt :: RefExp -> R Int -> Prelude.Int
toInt p (R i _) = i

fromInt :: RefExp -> Int -> R Int
fromInt p i = R i (mkInt p mkNoSrcPos i)

toInteger :: RefExp -> R Integer -> Integer
toInteger p (R i _) = i

fromInteger :: RefExp -> Integer -> R Integer
fromInteger p i = conInteger mkNoSrcPos p i

toFloat :: RefExp -> R Float -> Float
toFloat p (R f _) = f

fromFloat :: RefExp -> Float -> R Float
fromFloat p f = R f (mkFloat p mkNoSrcPos f)

toDouble :: RefExp -> R Double -> Double
toDouble p (R d _) = d

fromDouble :: RefExp -> Double -> R Double
fromDouble p d = R d (mkDouble p mkNoSrcPos d)


newtype Fun a b = Fun (RefExp -> R a -> R b)

-- arity could be stored additionally;
-- different constructors for different arity impossible (arity type args)
-- alternative idea (more complex):
-- newtype Fun a b = Fun1 (RefExp -> R a -> R b) | Funn (R a -> R b)
-- Funn implies that 'b' is again a Fun-type.

newtype IO a = IO (Prelude.IO (R a))

toIO :: (RefExp -> R a -> b) -> RefExp -> R (IO a) -> Prelude.IO b 
toIO f h (R (IO io) _) = fmap (f h) io

fromIO :: (RefExp -> a -> R b) 
       -> RefExp -> Prelude.IO a -> R (IO b)
fromIO f h io = R (IO (fmap (f h) io)) (mkValueUse h mkNoSrcPos aIO)

aIO :: RefAtom
aIO = mkAbstract "IO"


-- type constructors and data constructors need to have same name,
-- because transformation doesn't distinguish the two
data Tuple0 = Tuple0  -- () would do, but this way like other tuples  
data Tuple2 a b = Tuple2 (R a) (R b) -- not type Tuple2 a b = (R a,R b)
data Tuple3 a b c = Tuple3 (R a) (R b) (R c)
data Tuple4 a b c d = Tuple4 (R a) (R b) (R c) (R d)
data Tuple5 a b c d e = Tuple5 (R a) (R b) (R c) (R d) (R e)
data Tuple6 a b c d e f = Tuple6 (R a) (R b) (R c) (R d) (R e) (R f)
data Tuple7 a b c d e f g = Tuple7 (R a) (R b) (R c) (R d) (R e) (R f) (R g)
data Tuple8 a b c d e f g h = 
  Tuple8 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h)
data Tuple9 a b c d e f g h i = 
  Tuple9 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i)
data Tuple10 a b c d e f g h i j = 
  Tuple10 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j)
data Tuple11 a b c d e f g h i j k = 
  Tuple11 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j) (R k)
data Tuple12 a b c d e f g h i j k l = 
  Tuple12 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j) (R k) 
    (R l)
data Tuple13 a b c d e f g h i j k l m = 
  Tuple13 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j) (R k) 
    (R l) (R m)
data Tuple14 a b c d e f g h i j k l m n = 
  Tuple14 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j) (R k) 
    (R l) (R m) (R n)
data Tuple15 a b c d e f g h i j k l m n o = 
  Tuple15 (R a) (R b) (R c) (R d) (R e) (R f) (R g) (R h) (R i) (R j) (R k) 
    (R l) (R m) (R n) (R o)

aTuple0 = mkConstructor tPrelude 0 0 3 0 ""
aTuple2 = mkConstructor tPrelude 0 0 3 2 "," 
aTuple3 = mkConstructor tPrelude 0 0 3 3 ",,"
aTuple4 = mkConstructor tPrelude 0 0 3 4 ",,,"
aTuple5 = mkConstructor tPrelude 0 0 3 5 ",,,,"
aTuple6 = mkConstructor tPrelude 0 0 3 6 ",,,,,"
aTuple7 = mkConstructor tPrelude 0 0 3 7 ",,,,,,"
aTuple8 = mkConstructor tPrelude 0 0 3 8 ",,,,,,,"
aTuple9 = mkConstructor tPrelude 0 0 3 9 ",,,,,,,,"
aTuple10 = mkConstructor tPrelude 0 0 3 10 ",,,,,,,,,"
aTuple11 = mkConstructor tPrelude 0 0 3 11 ",,,,,,,,,,"
aTuple12 = mkConstructor tPrelude 0 0 3 12 ",,,,,,,,,,,"
aTuple13 = mkConstructor tPrelude 0 0 3 13 ",,,,,,,,,,,,"
aTuple14 = mkConstructor tPrelude 0 0 3 14 ",,,,,,,,,,,,,"
aTuple15 = mkConstructor tPrelude 0 0 3 15 ",,,,,,,,,,,,,,"

toTuple0 :: RefExp -> R Tuple0 -> ()
toTuple0 h (R Tuple0 _) = ()

fromTuple0 :: RefExp -> () -> R Tuple0
fromTuple0 t () = con0 mkNoSrcPos t Tuple0 aTuple0

toTuple2 :: (RefExp -> R a -> c) -> (RefExp -> R b -> d) 
         -> RefExp -> R (Tuple2 a b) -> (c,d)
toTuple2 f g h (R (Tuple2 x y) _) = (f h x,g h y)

fromTuple2 :: (RefExp -> a -> R c) -> (RefExp -> b -> R d) 
           -> RefExp -> (a,b) -> R (Tuple2 c d)
fromTuple2 f g h (x,y) = 
  con2 mkNoSrcPos h Tuple2 aTuple2 
    (wrapForward h (f h x)) (wrapForward h (g h y))


data List a = Cons (R a) (R (List a)) | List  
  -- type constructor and empty list constructor need to have same name,
  -- because transformation doesn't distinguish between the two
aCons = mkConstructor tPrelude 0 0 21 2 ":"
aList = mkConstructor tPrelude 0 0 3 0 "[]"

-- ----------------------------------------------------------------------------
-- Main combinator

traceIO :: String -> R (IO a) -> Prelude.IO ()
traceIO filename gmain = do
  openTrace filename
  catch (toIO toId mkRoot (forceExp gmain)) exceptionHandler 
  closeTrace


#if defined (__GLASGOW_HASKELL__)
#  if __GLASGOW_HASKELL__ < 600
exceptionHandler :: IOError -> Prelude.IO a
#  else
exceptionHandler :: Exception -> Prelude.IO a
#  endif
exceptionHandler exception = do
  hatAborted (case ioErrors exception of
                Just ioError -> System.IO.Error.ioeGetErrorString ioError	-- exception
                Nothing -> show exception)
  return undefined
#endif
#if defined (__NHC__)
exceptionHandler :: IOError -> Prelude.IO a
exceptionHandler exception = do
  hatAborted (IO.ioeGetErrorString exception)
  return undefined
#endif

hatError :: RefExp -> String -> a
hatError p msg =
  errorTraceExit msg p 1 `seq` undefined

fatal :: RefExp -> a
fatal r = hatError r "No match in pattern."


-- ----------------------------------------------------------------------------
-- Combinators

-- helpers

-- updates result of a RefExp (except Forward)
wrapResult :: RefSrcPos -> RefExp -> R a -> R a
wrapResult sr r x =
  R (entResult r sr `seq` case x of R y ry -> resResult r ry sr `seq` y) r

wrapForward :: RefExp -> R a -> R a
wrapForward h x =
  let for = mkForward h  
  in R (entForward for h `seq` case x of R y ry -> resForward for ry `seq` y) 
       for 

-- Attention: loses information if first value is demanded and then RefExp
-- Then RefExp is just the hidden exp, because writing of potentially
-- cyclic expression to trace is impossible.

{- -- looks nicer but 5% less efficient with nhc98, 25% worse with ghc.
uwrapForward :: RefExp -> R a -> R a
uwrapForward h x =
  unsafePerformIO (do
    statusRef <- newIORef (Hidden h)
    return $
      R (unsafePerformIO (do
          status <- readIORef statusRef
          case status of
            Hidden _ -> 
              case x of 
                R rv tv -> writeIORef statusRef Eval >> return rv
            Forward for -> 
              return $ entForward for h `seq` 
                         case x of 
                           R y ry -> resForward for ry `seq` y))
        (unsafePerformIO (do
          status <- readIORef statusRef
          case status of
            Hidden h -> do
                          let for = mkForward h 
                          writeIORef statusRef (Forward for) 
                          return for
            Eval -> return h)))
-}

uwrapForward :: RefExp -> R a -> R a
uwrapForward h x =
  let status = unsafePerformIO (newIORef (Hidden h))
  in status `seq`
     R (case unsafePerformIO (readIORef status) of
          Hidden _ -> 
            case x of 
              R rv tv -> unsafePerformIO (writeIORef status Eval) `seq` rv
          Forward for -> entForward for h `seq` 
                           case x of 
                             R y ry -> resForward for ry `seq` y)
       (case unsafePerformIO (readIORef status) of
          Hidden h ->
              let for = mkForward h in
              unsafePerformIO (writeIORef status (Forward for)) `seq` for
          Eval -> {- unsafePerformIO (putStrLn "No!") `seq` -} h)

        
data Status = Hidden RefExp  -- neither value nor RefExp yet demanded
            | Forward RefExp -- RefExp demanded, value not yet
            | Eval           -- value demanded, RefExp not (yet?)     

-- insert a hidden Exp if it is not already there (called from traced code)
hide :: RefExp -> (RefExp -> R z) -> R z
hide p f =
  if hidden p 
    then f p
    else let r = mkHidden p in wrapResult mkNoSrcPos r (f r)

-- combinators for n-ary traced application.

ap1 :: RefSrcPos -> RefExp -> R (Fun a z) -> R a -> R z 
ap1 sr p (R (Fun f) rf) a@(R _ ra) = 
  let r = mkApp1 p sr rf ra
  in  wrapResult sr r (f r a)

ap2 :: RefSrcPos -> RefExp -> R (Fun a (Fun b z)) -> R a -> R b -> R z
ap2 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) = 
  let r = mkApp2 p sr rf ra rb
  in  wrapResult sr r (pap1 sr p r (f r a) b)

ap3 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c z))) -> R a -> R b -> R c -> R z
ap3 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) = 
  let r = mkApp3 p sr rf ra rb rc
  in  wrapResult sr r (pap2 sr p r (f r a) b c) 

ap4 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d z)))) -> R a -> R b -> R c -> R d -> R z
ap4 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) = 
  let r = mkApp4 p sr rf ra rb rc rd
  in  wrapResult sr r (pap3 sr p r (f r a) b c d)

ap5 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e z))))) 
    -> R a -> R b -> R c -> R d -> R e -> R z
ap5 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) =
  let r = mkApp5 p sr rf ra rb rc rd re
  in  wrapResult sr r (pap4 sr p r (f r a) b c d e)

ap6 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g z)))))) 
    -> R a -> R b -> R c -> R d -> R e -> R g -> R z
ap6 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  g@(R _ rg) =
  let r = mkApp6 p sr rf ra rb rc rd re rg
  in  wrapResult sr r (pap5 sr p r (f r a) b c d e g)

ap7 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h z)))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R z
ap7 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  g@(R _ rg) h@(R _ rh) =
  let r = mkApp7 p sr rf ra rb rc rd re rg rh
  in  wrapResult sr r (pap6 sr p r (f r a) b c d e g h)

ap8 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i z)))))))) 
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R z
ap8 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  g@(R _ rg) h@(R _ rh) i@(R _ ri) =
  let r = mkApp8 p sr rf ra rb rc rd re rg rh ri
  in  wrapResult sr r (pap7 sr p r (f r a) b c d e g h i)

ap9 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j z)))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R z
ap9 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) =
  let r = mkApp9 p sr rf ra rb rc rd re rg rh ri rj
  in  wrapResult sr r (pap8 sr p r (f r a) b c d e g h i j)

ap10 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k z))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R z
ap10 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) =
  let r = mkApp10 p sr rf ra rb rc rd re rg rh ri rj rk
  in  wrapResult sr r (pap9 sr p r (f r a) b c d e g h i j k)

ap11 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k (Fun l z)))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R l -> R z
ap11 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) =
  let r = mkApp11 p sr rf ra rb rc rd re rg rh ri rj rk rl
  in  wrapResult sr r (pap10 sr p r (f r a) b c d e g h i j k l)

ap12 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k (Fun l (Fun m z))))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R l -> R m -> R z
ap12 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) =
  let r = mkApp12 p sr rf ra rb rc rd re rg rh ri rj rk rl rm
  in  wrapResult sr r (pap11 sr p r (f r a) b c d e g h i j k l m)

ap13 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k (Fun l (Fun m (Fun n z)))))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R l -> R m -> R n -> R z
ap13 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) n@(R _ rn) =
  let r = mkApp13 p sr rf ra rb rc rd re rg rh ri rj rk rl rm rn
  in  wrapResult sr r (pap12 sr p r (f r a) b c d e g h i j k l m n)

ap14 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k (Fun l (Fun m (Fun n (Fun o z))))))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R l -> R m -> R n -> R o -> R z
ap14 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) n@(R _ rn) o@(R _ ro) =
  let r = mkApp14 p sr rf ra rb rc rd re rg rh ri rj rk rl rm rn ro
  in  wrapResult sr r (pap13 sr p r (f r a) b c d e g h i j k l m n o)

ap15 :: RefSrcPos -> RefExp 
    -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
         (Fun j (Fun k (Fun l (Fun m (Fun n (Fun o (Fun q z)))))))))))))))
    -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
    -> R l -> R m -> R n -> R o -> R q -> R z
ap15 sr p (R (Fun f) rf) a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) 
  e@(R _ re) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) n@(R _ rn) o@(R _ ro) q@(R _ rq) =
  let r = mkApp15 p sr rf ra rb rc rd re rg rh ri rj rk rl rm rn ro rq
  in  wrapResult sr r (pap14 sr p r (f r a) b c d e g h i j k l m n o q)

-- helper functions for ap*

pap1 :: RefSrcPos -> RefExp -> RefExp -> R (Fun a z) -> R a -> R z
pap1 sr p r wf@(R (Fun f) rf) a =
  if r == rf 
    then f r a
    else ap1 sr p wf a

pap2 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b z)) -> R a -> R b -> R z
pap2 sr p r wf@(R (Fun f) rf) a b =
  if r == rf 
    then pap1 sr p r (f r a) b
    else ap2 sr p wf a b

pap3 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c z))) -> R a -> R b -> R c -> R z
pap3 sr p r wf@(R (Fun f) rf) a b c =
  if r == rf 
    then pap2 sr p r (f r a) b c
    else ap3 sr p wf a b c

pap4 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d z)))) -> R a -> R b -> R c -> R d -> R z
pap4 sr p r wf@(R (Fun f) rf) a b c d =
  if r == rf 
    then pap3 sr p r (f r a) b c d
    else ap4 sr p wf a b c d

pap5 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e z))))) 
     -> R a -> R b -> R c -> R d -> R e -> R z
pap5 sr p r wf@(R (Fun f) rf) a b c d e =
  if r == rf 
    then pap4 sr p r (f r a) b c d e
    else ap5 sr p wf a b c d e

pap6 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g z)))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R z
pap6 sr p r wf@(R (Fun f) rf) a b c d e g =
  if r == rf 
    then pap5 sr p r (f r a) b c d e g
    else ap6 sr p wf a b c d e g

pap7 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h z))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R z
pap7 sr p r wf@(R (Fun f) rf) a b c d e g h =
  if r == rf 
    then pap6 sr p r (f r a) b c d e g h 
    else ap7 sr p wf a b c d e g h

pap8 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i z)))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R z
pap8 sr p r wf@(R (Fun f) rf) a b c d e g h i =
  if r == rf 
    then pap7 sr p r (f r a) b c d e g h i
    else ap8 sr p wf a b c d e g h i

pap9 :: RefSrcPos -> RefExp -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
        (Fun j z))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R z
pap9 sr p r wf@(R (Fun f) rf) a b c d e g h i j =
  if r == rf 
    then pap8 sr p r (f r a) b c d e g h i j
    else ap9 sr p wf a b c d e g h i j

pap10 :: RefSrcPos -> RefExp -> RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i (Fun j 
         (Fun k z)))))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
      -> R z
pap10 sr p r wf@(R (Fun f) rf) a b c d e g h i j k =
  if r == rf 
    then pap9 sr p r (f r a) b c d e g h i j k
    else ap10 sr p wf a b c d e g h i j k

pap11 :: RefSrcPos -> RefExp -> RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i (Fun j 
         (Fun k (Fun l z)))))))))))
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
      -> R l -> R z
pap11 sr p r wf@(R (Fun f) rf) a b c d e g h i j k l =
  if r == rf 
    then pap10 sr p r (f r a) b c d e g h i j k l
    else ap11 sr p wf a b c d e g h i j k l

pap12 :: RefSrcPos -> RefExp -> RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i (Fun j 
         (Fun k (Fun l (Fun m z)))))))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
      -> R l -> R m -> R z
pap12 sr p r wf@(R (Fun f) rf) a b c d e g h i j k l m =
  if r == rf 
    then pap11 sr p r (f r a) b c d e g h i j k l m
    else ap12 sr p wf a b c d e g h i j k l m

pap13 :: RefSrcPos -> RefExp -> RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i (Fun j
         (Fun k (Fun l (Fun m (Fun n z))))))))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
      -> R l -> R m -> R n -> R z
pap13 sr p r wf@(R (Fun f) rf) a b c d e g h i j k l m n =
  if r == rf 
    then pap12 sr p r (f r a) b c d e g h i j k l m n
    else ap13 sr p wf a b c d e g h i j k l m n

pap14 :: RefSrcPos -> RefExp -> RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i (Fun j
         (Fun k (Fun l (Fun m (Fun n (Fun o z)))))))))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k 
      -> R l -> R m -> R n -> R o -> R z
pap14 sr p r wf@(R (Fun f) rf) a b c d e g h i j k l m n o =
  if r == rf 
    then pap13 sr p r (f r a) b c d e g h i j k l m n o
    else ap14 sr p wf a b c d e g h i j k l m n o

-- combinators for n-ary traced abstraction

fun1 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> RefExp -> R z)
     -> R (Fun a z)
fun1 var sr p f =
  let rf = mkValueUse p sr var
  in R (-- entValueUse rf sr `seq` 
         Fun (\t a -> 
                if hidden t 
                  then let r = mkApp1 t mkNoSrcPos rf (refExp a) 
                       in  recordChild t r `seq` 
                           wrapResult mkNoSrcPos r (f a r)
                  else f a t))
       rf 

fun2 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> R b -> RefExp -> R z)
     -> R (Fun a (Fun b z))
fun2 var sr p f =
  let rf = mkValueUse p sr var
  in R (-- entValueUse rf sr `seq` 
         Fun (\t a -> 
           R (Fun (\t b ->
                     if hidden t 
                       then let r = mkApp2 t mkNoSrcPos rf (refExp a) 
                                      (refExp b)
                            in  recordChild t r `seq` 
                                wrapResult mkNoSrcPos r (f a b r)
                       else f a b t))
             t))
       rf 

fun3 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> R b -> R c -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c z)))
fun3 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           if hidden t 
             then let r = mkApp3 t mkNoSrcPos rf (refExp a) 
                            (refExp b) (refExp c)
                  in  recordChild t r `seq` 
                      wrapResult mkNoSrcPos r (f a b c r)
             else f a b c t))
           t))
         t))
       rf 

fun4 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d z))))
fun4 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             if hidden t 
               then let r = mkApp4 t mkNoSrcPos rf (refExp a) 
                              (refExp b) (refExp c) (refExp d)
                    in  recordChild t r `seq` 
                        wrapResult mkNoSrcPos r (f a b c d r)
               else f a b c d t))
             t))
           t))
         t))
       rf 

fun5 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e z)))))
fun5 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               if hidden t 
                 then let r = mkApp5 t mkNoSrcPos rf (refExp a) (refExp b) 
                                (refExp c) (refExp d) (refExp e)
                      in  recordChild t r `seq` 
                          wrapResult mkNoSrcPos r (f a b c d e r)
                 else f a b c d e t))
               t))
             t))
           t))
         t))
       rf 


fun6 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R g -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g z))))))
fun6 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 if hidden t 
                   then let r = mkApp6 t mkNoSrcPos rf (refExp a) (refExp b) 
                                  (refExp c) (refExp d) (refExp e) (refExp g)
                        in  recordChild t r `seq` 
                            wrapResult mkNoSrcPos r (f a b c d e g r)
                   else f a b c d e g t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun7 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h z)))))))
fun7 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   if hidden t 
                     then let r = mkApp7 t mkNoSrcPos rf (refExp a) (refExp b) 
                                    (refExp c) (refExp d) (refExp e) (refExp g)
                                    (refExp h)
                          in  recordChild t r `seq` 
                              wrapResult mkNoSrcPos r (f a b c d e g h r)
                     else f a b c d e g h t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun8 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i z))))))))
fun8 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     if hidden t 
                       then let r = mkApp8 t mkNoSrcPos rf (refExp a) 
                                      (refExp b) (refExp c) (refExp d) 
                                      (refExp e) (refExp g) (refExp h)
                                      (refExp i)
                            in  recordChild t r `seq` 
                                wrapResult mkNoSrcPos r (f a b c d e g h i r)
                       else f a b c d e g h i t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun9 :: RefAtom -> RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j 
         -> RefExp -> R z)
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
          (Fun j z)))))))))
fun9 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       if hidden t 
                         then let r = mkApp9 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j)
                              in  recordChild t r `seq`
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j r)
                         else f a b c d e g h i j t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun10 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k z))))))))))
fun10 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                       if hidden t 
                         then let r = mkApp10 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k r)
                         else f a b c d e g h i j k t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun11 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> R l -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k (Fun l z)))))))))))
fun11 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                         R (Fun (\t l ->
                       if hidden t 
                         then let r = mkApp11 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                                        (refExp l)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k l r)
                         else f a b c d e g h i j k l t))
                           t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun12 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> R l -> R m -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k (Fun l (Fun m z))))))))))))
fun12 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                         R (Fun (\t l ->
                           R (Fun (\t m ->
                       if hidden t 
                         then let r = mkApp12 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                                        (refExp l) (refExp m)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k l m r)
                         else f a b c d e g h i j k l m t))
                             t))
                           t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun13 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> R l -> R m -> R n -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k (Fun l (Fun m (Fun n z)))))))))))))
fun13 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                         R (Fun (\t l ->
                           R (Fun (\t m ->
                             R (Fun (\t n ->
                       if hidden t 
                         then let r = mkApp13 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                                        (refExp l) (refExp m) (refExp n)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k l m n r)
                         else f a b c d e g h i j k l m n t))
                               t))
                             t))
                           t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun14 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> R l -> R m -> R n -> R o -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k (Fun l (Fun m (Fun n (Fun o z))))))))))))))
fun14 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                         R (Fun (\t l ->
                           R (Fun (\t m ->
                             R (Fun (\t n ->
                               R (Fun (\t o ->
                       if hidden t 
                         then let r = mkApp14 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                                        (refExp l) (refExp m) (refExp n)
                                        (refExp o)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k l m n o r)
                         else f a b c d e g h i j k l m n o t))
                                 t))
                               t))
                             t))
                           t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 

fun15 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R j -> R k
          -> R l -> R m -> R n -> R o -> R q -> RefExp -> R z)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i 
           (Fun j (Fun k (Fun l (Fun m (Fun n (Fun o (Fun q z)))))))))))))))
fun15 var sr p f =
  let rf = mkValueUse p sr var
  in R (Fun (\t a -> 
       R (Fun (\t b ->
         R (Fun (\t c ->
           R (Fun (\t d -> 
             R (Fun (\t e -> 
               R (Fun (\t g ->
                 R (Fun (\t h ->
                   R (Fun (\t i ->
                     R (Fun (\t j ->
                       R (Fun (\t k ->
                         R (Fun (\t l ->
                           R (Fun (\t m ->
                             R (Fun (\t n ->
                               R (Fun (\t o ->
                                 R (Fun (\t q ->
                       if hidden t 
                         then let r = mkApp15 t mkNoSrcPos rf (refExp a) 
                                        (refExp b) (refExp c) (refExp d) 
                                        (refExp e) (refExp g) (refExp h)
                                        (refExp i) (refExp j) (refExp k)
                                        (refExp l) (refExp m) (refExp n)
                                        (refExp o) (refExp q)
                              in  recordChild t r `seq` 
                                  wrapResult mkNoSrcPos r 
                                    (f a b c d e g h i j k l m n o q r)
                         else f a b c d e g h i j k l m n o q t))
                                   t))
                                 t))
                               t))
                             t))
                           t))
                         t))
                       t))
                     t))
                   t))
                 t))
               t))
             t))
           t))
         t))
       rf 


-- combinators for n-ary untraced applications

uap1 :: RefSrcPos -> RefExp -> R (Fun a z) -> R a -> R z
uap1 sr h (R (Fun f) _) a = uwrapForward h (f h a)

uap2 :: RefSrcPos -> RefExp -> R (Fun a (Fun b z)) -> R a -> R b -> R z
uap2 sr h f a b = uwrapForward h $
  case uap1 sr h f a of
    R (Fun f) _ -> f h b

uap3 :: RefSrcPos -> RefExp -> R (Fun a (Fun b (Fun c z))) 
     -> R a -> R b -> R c -> R z
uap3 sr h f a b c = uwrapForward h $
  case uap2 sr h f a b of
    R (Fun f) _ -> f h c

uap4 :: RefSrcPos -> RefExp -> R (Fun a (Fun b (Fun c (Fun d z)))) 
     -> R a -> R b -> R c -> R d -> R z
uap4 sr h f a b c d = uwrapForward h $
  case uap3 sr h f a b c of
    R (Fun f) _ -> f h d

uap5 :: RefSrcPos -> RefExp -> R (Fun a (Fun b (Fun c (Fun d (Fun e z))))) 
     -> R a -> R b -> R c -> R d -> R e -> R z
uap5 sr h f a b c d e = uwrapForward h (upap5 h f a b c d e)

uap6 :: RefSrcPos -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g z)))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R z
uap6 sr h f a b c d e g = uwrapForward h (upap6 h f a b c d e g)

uap7 :: RefSrcPos -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h z))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R z
uap7 sr p f a b c d e g h = uwrapForward p (upap7 p f a b c d e g h)

uap8 :: RefSrcPos -> RefExp 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i z)))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R z
uap8 sr p f a b c d e g h i = uwrapForward p (upap8 p f a b c d e g h i)


upap1 :: RefExp -> R (Fun a z) -> R a -> R z
upap1 h (R (Fun f) _) a = f h a

upap2 :: RefExp -> R (Fun a (Fun b z)) -> R a -> R b -> R z
upap2 h f a b = 
  case upap1 h f a of
    R (Fun f) _ -> f h b

upap3 :: RefExp -> R (Fun a (Fun b (Fun c z))) 
      -> R a -> R b -> R c -> R z
upap3 h f a b c = 
  case upap2 h f a b of
    R (Fun f) _ -> f h c

upap4 :: RefExp -> R (Fun a (Fun b (Fun c (Fun d z)))) 
      -> R a -> R b -> R c -> R d -> R z
upap4 h f a b c d =
  case upap3 h f a b c of
    R (Fun f) _ -> f h d

upap5 :: RefExp -> R (Fun a (Fun b (Fun c (Fun d (Fun e z))))) 
      -> R a -> R b -> R c -> R d -> R e -> R z
upap5 h f a b c d e = 
  case upap4 h f a b c d of
    R (Fun f) _ -> f h e 

upap6 :: RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g z)))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R z
upap6 h f a b c d e g = 
  case upap5 h f a b c d e of
    R (Fun f) _ -> f h g

upap7 :: RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h z))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R z
upap7 p f a b c d e g h =
  case upap6 p f a b c d e g of
    R (Fun f) _ -> f p h

upap8 :: RefExp 
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i z)))))))) 
      -> R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i -> R z
upap8 p f a b c d e g h i =
  case upap7 p f a b c d e g h of
    R (Fun f) _ -> f p i


-- combinators for n-ary untraced abstraction (function construction)


ufun1 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> RefExp -> R r) 
      -> R (Fun a r)
ufun1 at sr p f = R (Fun (\r a -> hide r (f a))) (mkValueUse p sr at)

ufun2 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> R b -> RefExp -> R r)
      -> R (Fun a (Fun b r))
ufun2 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b -> hide r (f a b)))
      r))
    (mkValueUse p sr at)

ufun3 :: RefAtom -> RefSrcPos -> RefExp -> (R a -> R b -> R c -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c r)))
ufun3 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> hide r (f a b c)))
        r))
      r))
    (mkValueUse p sr at)

ufun4 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c (Fun d r))))
ufun4 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> 
        R (Fun (\r d -> hide r (f a b c d)))
          r))
        r))
      r))
    (mkValueUse p sr at)

ufun5 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e r)))))
ufun5 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> 
        R (Fun (\r d -> 
          R (Fun (\r e -> hide r (f a b c d e)))
            r))
          r))
        r))
      r))
    (mkValueUse p sr at)

ufun6 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g r))))))
ufun6 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> 
        R (Fun (\r d -> 
          R (Fun (\r e -> 
            R (Fun (\r g -> hide r (f a b c d e g)))
              r))
            r))
          r))
        r))
      r))
    (mkValueUse p sr at)

ufun7 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h r)))))))
ufun7 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> 
        R (Fun (\r d -> 
          R (Fun (\r e -> 
            R (Fun (\r g -> 
              R (Fun (\r h -> hide r (f a b c d e g h)))
                r))
              r))
            r))
          r))
        r))
      r))
    (mkValueUse p sr at)

ufun8 :: RefAtom -> RefSrcPos -> RefExp 
      -> (R a -> R b -> R c -> R d -> R e -> R g -> R h -> R i 
          -> RefExp -> R r)
      -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun g (Fun h (Fun i r))))))))
ufun8 at sr p f =
  R (Fun (\r a ->
    R (Fun (\r b ->
      R (Fun (\r c -> 
        R (Fun (\r d -> 
          R (Fun (\r e -> 
            R (Fun (\r g -> 
              R (Fun (\r h -> 
                R (Fun (\r i -> hide r (f a b c d e g h i)))
                  r))
                r))
              r))
            r))
          r))
        r))
      r))
    (mkValueUse p sr at)

-- Combinators for saturated n-ary applications of data constructors

con0 :: RefSrcPos -> RefExp -> z -> RefAtom -> R z
con0 sr p cn at =
  R cn (mkValueUse p sr at)

con1 :: RefSrcPos -> RefExp -> (R a -> z) -> RefAtom -> R a -> R z
con1 sr p cn at a@(R _ ra) =
  R (cn a) (mkValueApp1 p sr at ra)

con2 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> z) -> RefAtom -> R a -> R b -> R z
con2 sr p cn at a@(R _ ra) b@(R _ rb) =
  R (cn a b) (mkValueApp2 p sr at ra rb)

con3 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> z) -> RefAtom -> R a -> R b -> R c -> R z
con3 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) =
  R (cn a b c) (mkValueApp3 p sr at ra rb rc)

con4 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> z) -> RefAtom 
     -> R a -> R b -> R c -> R d -> R z
con4 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) =
  R (cn a b c d) (mkValueApp4 p sr at ra rb rc rd)

con5 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> z) -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R z
con5 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) =
  R (cn a b c d e) (mkValueApp5 p sr at ra rb rc rd re)

con6 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> z) -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R z
con6 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) =
  R (cn a b c d e f) (mkValueApp6 p sr at ra rb rc rd re rf)

con7 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> z) -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R z
con7 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) =
  R (cn a b c d e f g) 
    (mkValueApp7 p sr at ra rb rc rd re rf rg)

con8 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> z) -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R z
con8 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) =
  R (cn a b c d e f g h) 
    (mkValueApp8 p sr at ra rb rc rd re rf rg rh)

con9 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R z
con9 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) =
  R (cn a b c d e f g h i) 
    (mkValueApp9 p sr at ra rb rc rd re rf rg rh ri)

con10 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R z
con10 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) =
  R (cn a b c d e f g h i j) 
    (mkValueApp10 p sr at ra rb rc rd re rf rg rh ri rj)

con11 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> R k -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R k -> R z
con11 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) =
  R (cn a b c d e f g h i j k) 
    (mkValueApp11 p sr at ra rb rc rd re rf rg rh ri rj rk)

con12 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> R k -> R l -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R k -> R l -> R z
con12 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) =
  R (cn a b c d e f g h i j k l) 
    (mkValueApp12 p sr at ra rb rc rd re rf rg rh ri rj rk rl)

con13 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> R k -> R l -> R m -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R k -> R l -> R m -> R z
con13 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) =
  R (cn a b c d e f g h i j k l m) 
    (mkValueApp13 p sr at ra rb rc rd re rf rg rh ri rj rk rl rm)

con14 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> R k -> R l -> R m -> R n -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R k -> R l -> R m -> R n -> R z
con14 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) n@(R _ rn) =
  R (cn a b c d e f g h i j k l m n) 
    (mkValueApp14 p sr at ra rb rc rd re rf rg rh ri rj rk rl rm rn)

con15 :: RefSrcPos -> RefExp 
     -> (R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
         -> R k -> R l -> R m -> R n -> R o -> z) 
     -> RefAtom 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R k -> R l -> R m -> R n -> R o -> R z
con15 sr p cn at a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) 
  f@(R _ rf) g@(R _ rg) h@(R _ rh) i@(R _ ri) j@(R _ rj) k@(R _ rk) 
  l@(R _ rl) m@(R _ rm) n@(R _ rn) o@(R _ ro) =
  R (cn a b c d e f g h i j k l m n o) 
    (mkValueApp15 p sr at ra rb rc rd re rf rg rh ri rj rk rl rm rn ro)


-- Create application node for function that is partially applied to i
-- arguments and transform partial application with given function.
-- Used for partially applied data constructors.

pa0 :: b -> (b -> RefExp -> c) -> RefSrcPos -> RefExp -> RefAtom -> c
pa0 c cni sr p at =
  cni c (mkValueUse p sr at) 


pa1 :: (R a1 -> b) -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom -> R a1 -> c
pa1 c cni sr p at a1@(R _ r1) =
  cni (c a1) (mkValueApp1 p sr at r1)


pa2 :: (R a1 -> R a2 -> b) -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> c
pa2 c cni sr p at a1@(R _ r1) a2@(R _ r2) =
  cni (c a1 a2) (mkValueApp2 p sr at r1 r2)


pa3 :: (R a1 -> R a2 -> R a3 -> b) -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> c
pa3 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) =
  cni (c a1 a2 a3) (mkValueApp3 p sr at r1 r2 r3)


pa4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> R a4 -> c
pa4 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) a4@(R _ r4) =
  cni (c a1 a2 a3 a4) (mkValueApp4 p sr at r1 r2 r3 r4)

pa5 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> b) -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> c
pa5 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) a4@(R _ r4) a5@(R _ r5) =
  cni (c a1 a2 a3 a4 a5) (mkValueApp5 p sr at r1 r2 r3 r4 r5)

pa6 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> b) 
    -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> c
pa6 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) a4@(R _ r4) a5@(R _ r5)
  a6@(R _ r6) =
  cni (c a1 a2 a3 a4 a5 a6) (mkValueApp6 p sr at r1 r2 r3 r4 r5 r6)

pa7 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> b) 
    -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> c
pa7 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) a4@(R _ r4) a5@(R _ r5)
  a6@(R _ r6) a7@(R _ r7) =
  cni (c a1 a2 a3 a4 a5 a6 a7) (mkValueApp7 p sr at r1 r2 r3 r4 r5 r6 r7)

pa8 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> b) 
    -> (b -> RefExp -> c) 
    -> RefSrcPos -> RefExp -> RefAtom
    -> R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> c
pa8 c cni sr p at a1@(R _ r1) a2@(R _ r2) a3@(R _ r3) a4@(R _ r4) a5@(R _ r5)
  a6@(R _ r6) a7@(R _ r7) a8@(R _ r8) =
  cni (c a1 a2 a3 a4 a5 a6 a7 a8) (mkValueApp8 p sr at r1 r2 r3 r4 r5 r6 r7 r8)

-- Transform a function into a wrapped function with the given RefExp.
-- The application Exp is recorded by the application combinator.
-- Used for partially applied constructors

cn1 :: (R a1 -> b) -> RefExp -> R (Fun a1 b)
cn1 f rf = R (Fun (\r a ->
             R (f a)
               (if hidden r 
                  then mkResApp1 r mkNoSrcPos rf (refExp a)
                  else r)))
             rf

cn2 :: (R a1 -> R a2 -> b) -> RefExp -> R (Fun a1 (Fun a2 b))
cn2 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (f a b)
                 (if hidden r 
                    then mkResApp2 r mkNoSrcPos rf (refExp a) (refExp b)
                    else r)))
               r))
             rf

cn3 :: (R a1 -> R a2 -> R a3 -> b) -> RefExp -> R (Fun a1 (Fun a2 (Fun a3 b)))
cn3 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (f a b c)
                   (if hidden r 
                      then mkResApp3 r mkNoSrcPos rf (refExp a) (refExp b) 
                             (refExp c)
                      else r)))
                 r))r))rf

cn4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 b))))
cn4 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (f a b c d)
                     (if hidden r 
                        then mkResApp4 r mkNoSrcPos rf (refExp a) (refExp b) 
                               (refExp c) (refExp d)
                        else r)))
                   r))r))r))rf

cn5 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> b) -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 b)))))
cn5 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (Fun (\r e ->
                     R (f a b c d e)
                       (if hidden r 
                          then mkResApp5 r mkNoSrcPos rf (refExp a) (refExp b) 
                                 (refExp c) (refExp d) (refExp e)
                          else r)))
                     r))r))r))r))rf

cn6 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> b) -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 b))))))
cn6 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (Fun (\r e ->
                     R (Fun (\r g ->
                       R (f a b c d e g)
                         (if hidden r 
                            then mkResApp6 r mkNoSrcPos rf (refExp a) 
                                   (refExp b) (refExp c) (refExp d) (refExp e)
                                   (refExp g)
                            else r)))
                         r))r))r))r))r))rf

cn7 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> b) -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 b)))))))
cn7 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (Fun (\r e ->
                     R (Fun (\r g ->
                       R (Fun (\r h ->
                         R (f a b c d e g h)
                           (if hidden r 
                              then mkResApp7 r mkNoSrcPos rf (refExp a) 
                                     (refExp b) (refExp c) (refExp d) 
                                     (refExp e) (refExp g) (refExp h)
                              else r)))
                         r))r))r))r))r))r))rf

cn8 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> b) 
    -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 (Fun a8 
         b))))))))
cn8 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (Fun (\r e ->
                     R (Fun (\r g ->
                       R (Fun (\r h ->
                         R (Fun (\r i ->
                           R (f a b c d e g h i)
                             (if hidden r 
                                then mkResApp8 r mkNoSrcPos rf (refExp a) 
                                       (refExp b) (refExp c) (refExp d) 
                                       (refExp e) (refExp g) (refExp h) 
                                       (refExp i)
                                else r)))
                           r))r))r))r))r))r))r))rf

cn9 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> R a9 
        -> b) 
    -> RefExp 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 (Fun a8 
         (Fun a9 b)))))))))
cn9 f rf = R (Fun (\r a ->
             R (Fun (\r b ->
               R (Fun (\r c ->
                 R (Fun (\r d ->
                   R (Fun (\r e ->
                     R (Fun (\r g ->
                       R (Fun (\r h ->
                         R (Fun (\r i ->
                           R (Fun (\r j ->
                             R (f a b c d e g h i j)
                               (if hidden r 
                                  then mkResApp9 r mkNoSrcPos rf (refExp a) 
                                         (refExp b) (refExp c) (refExp d) 
                                         (refExp e) (refExp g) (refExp h) 
                                         (refExp i) (refExp j)
                                  else r)))
                             r))r))r))r))r))r))r))r))rf

cn10 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> R a9 
         -> R a10 -> b) 
     -> RefExp 
     -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 (Fun a8 
          (Fun a9 (Fun a10 b))))))))))
cn10 f rf = R (Fun (\r a ->
              R (Fun (\r b ->
                R (Fun (\r c ->
                  R (Fun (\r d ->
                    R (Fun (\r e ->
                      R (Fun (\r g ->
                        R (Fun (\r h ->
                          R (Fun (\r i ->
                            R (Fun (\r j ->
                              R (Fun (\r k ->
                                R (f a b c d e g h i j k)
                                  (if hidden r 
                                    then mkResApp10 r mkNoSrcPos rf (refExp a) 
                                           (refExp b) (refExp c) (refExp d) 
                                           (refExp e) (refExp g) (refExp h) 
                                           (refExp i) (refExp j) (refExp k)
                                    else r)))
                                r))r))r))r))r))r))r))r))r))rf

cn11 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> R a9 
         -> R a10 -> R a11 -> b) 
     -> RefExp 
     -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 (Fun a8 
          (Fun a9 (Fun a10 (Fun a11 b)))))))))))
cn11 f rf = R (Fun (\r a ->
              R (Fun (\r b ->
                R (Fun (\r c ->
                  R (Fun (\r d ->
                    R (Fun (\r e ->
                      R (Fun (\r g ->
                        R (Fun (\r h ->
                          R (Fun (\r i ->
                            R (Fun (\r j ->
                              R (Fun (\r k ->
                                R (Fun (\r l ->
                                  R (f a b c d e g h i j k l)
                                    (if hidden r 
                                       then mkResApp11 r mkNoSrcPos rf 
                                              (refExp a) (refExp b) (refExp c)
                                              (refExp d) (refExp e) (refExp g)
                                              (refExp h) (refExp i) (refExp j)
                                              (refExp k) (refExp l)
                                        else r)))
                                  r))r))r))r))r))r))r))r))r))r))rf

cn12 :: (R a1 -> R a2 -> R a3 -> R a4 -> R a5 -> R a6 -> R a7 -> R a8 -> R a9 
         -> R a10 -> R a11 -> R a12 -> b) 
     -> RefExp 
     -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 (Fun a5 (Fun a6 (Fun a7 (Fun a8 
          (Fun a9 (Fun a10 (Fun a11 (Fun a12 b))))))))))))
cn12 f rf = R (Fun (\r a ->
              R (Fun (\r b ->
                R (Fun (\r c ->
                  R (Fun (\r d ->
                    R (Fun (\r e ->
                      R (Fun (\r g ->
                        R (Fun (\r h ->
                          R (Fun (\r i ->
                            R (Fun (\r j ->
                              R (Fun (\r k ->
                                R (Fun (\r l ->
                                  R (Fun (\r m ->
                                    R (f a b c d e g h i j k l m)
                                      (if hidden r 
                                         then mkResApp12 r mkNoSrcPos rf 
                                                (refExp a) (refExp b) 
                                                (refExp c) (refExp d) 
                                                (refExp e) (refExp g)
                                                (refExp h) (refExp i) 
                                                (refExp j) (refExp k) 
                                                (refExp l) (refExp m)
                                        else r)))
                                    r))r))r))r))r))r))r))r))r))r))r))rf

-- Combinators for constant definitions

constUse :: RefSrcPos -> RefExp -> R a -> R a
constUse sr p wv@(R v rv) =
  let constUseRefExp = mkConstUse p sr rv
  in wrapResult sr constUseRefExp wv
  -- Wrapping only done so that a black hole is correctly located
  -- i.e. to ensure that entering of black hole is marked before
  -- the runtime system notices the black hole.

uconstUse :: RefSrcPos -> RefExp -> R a -> R a
uconstUse sr p wv@(R v rv) =
  if hidden p
    then -- called from trusted code
       wv
    else
       R v (mkConstUse p sr rv)
       -- assume there is no black hole in trusted code

constDef :: RefExp -> RefAtom -> (RefExp -> R a) -> R a
constDef context var f =
  let constDefRefExp = mkConstDef context var
  in wrapResult mkNoSrcPos constDefRefExp (f constDefRefExp)

uconstDef :: RefExp -> RefAtom -> (RefExp -> R a) -> R a
uconstDef context var f =
  if hidden context 
    then 
      -- do not record local constant
      -- need Forward, because constUse pattern matches on R and
      -- definition of constant may be cyclic, e.g. xs = x:xs.
      uwrapForward context (f context)
    else
      -- ConstDef Exp always recorded; even if not called from traced code
      let constDefRefExp = mkConstDef context var
      in wrapResult mkNoSrcPos constDefRefExp (hide constDefRefExp f)



-- Combinators for control flow constructs

cguard :: RefSrcPos -> RefExp 
       -> R Bool -> (RefExp -> R a) -> (RefExp -> R a) -> R a
cguard sr parent (R c rc) e1 e2 =
  let guardRefExp = mkGuard parent sr rc
  in wrapResult sr guardRefExp (if c then e1 guardRefExp else e2 guardRefExp)

ucguard :: R Bool -> R a -> R a -> R a
-- no wrapForward necessary, because a guard is always surounded by
-- the wrapForward of the function (there is no unevaluated guard).
ucguard (R True _) e1 e2 = e1
ucguard (R False _) e1 e2 = e2

cif :: RefSrcPos -> RefExp 
    -> R Bool -> (RefExp -> R a) -> (RefExp -> R a) -> R a
cif sr parent (R c rc) e1 e2 =
  let ifRefExp = mkIf parent sr rc
  in wrapResult sr ifRefExp (if c then e1 ifRefExp else e2 ifRefExp)

ucif :: RefExp -> R Bool -> R a -> R a -> R a
ucif hidden cond e1 e2 = uwrapForward hidden 
  (case cond of 
    R True _ -> e1
    R False _ -> e2)

ccase :: RefSrcPos -> RefExp -> (R a -> RefExp -> R b) -> R a -> R b
ccase sr parent fun arg@(R _ rarg) =
  let caseRefExp = mkCase parent sr rarg
  in wrapResult sr caseRefExp (fun arg caseRefExp)

uccase :: RefSrcPos -> RefExp -> (R a -> RefExp -> R b) -> R a -> R b
uccase _ hidden fun arg = uwrapForward hidden (fun arg hidden) 

-- Combinators for field updating

-- wraps a data constructor or data constructor application,
-- recording the correct expression in the trace.
-- Instance generated for every data type by tracing transformation.
class WrapVal a where
  wrapVal :: RefSrcPos -> a -> RefExp -> R a

update1 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> R b -> R a
update1 sr p (R v rv) modify f1 (R _ r1) =
  let p' = mkFieldUpdate1 p sr rv f1 r1
  in wrapResult sr p' (wrapVal sr (modify v) p')

update2 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> R b -> R c -> R a
update2 sr p (R v rv) modify f1 f2 (R _ r1) (R _ r2) =
  let p' = mkFieldUpdate2 p sr rv f1 r1 f2 r2
  in wrapResult sr p' (wrapVal sr (modify v) p')

update3 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> R b -> R c -> R d -> R a
update3 sr p (R v rv) modify f1 f2 f3 (R _ r1) (R _ r2) (R _ r3) =
  let p' = mkFieldUpdate3 p sr rv f1 r1 f2 r2 f3 r3
  in wrapResult sr p' (wrapVal sr (modify v) p')

update4 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R a
update4 sr p (R v rv) modify f1 f2 f3 f4 (R _ r1) (R _ r2) (R _ r3) (R _ r4) =
  let p' = mkFieldUpdate4 p sr rv f1 r1 f2 r2 f3 r3 f4 r4
  in wrapResult sr p' (wrapVal sr (modify v) p')

update5 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R a
update5 sr p (R v rv) modify f1 f2 f3 f4 f5
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) =
  let p' = mkFieldUpdate5 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5
  in wrapResult sr p' (wrapVal sr (modify v) p')

update6 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R g -> R a
update6 sr p (R v rv) modify f1 f2 f3 f4 f5 f6
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) (R _ r6) =
  let p' = mkFieldUpdate6 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5 f6 r6
  in wrapResult sr p' (wrapVal sr (modify v) p')

update7 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R a
update7 sr p (R v rv) modify f1 f2 f3 f4 f5 f6 f7
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) (R _ r6) (R _ r7) =
  let p' = mkFieldUpdate7 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5 f6 r6 f7 r7
  in wrapResult sr p' (wrapVal sr (modify v) p')

update8 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R a
update8 sr p (R v rv) modify f1 f2 f3 f4 f5 f6 f7 f8
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) (R _ r6)
        (R _ r7) (R _ r8) =
  let p' = mkFieldUpdate8 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5
                                  f6 r6 f7 r7 f8 r8
  in wrapResult sr p' (wrapVal sr (modify v) p')

update9 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j -> R a
update9 sr p (R v rv) modify f1 f2 f3 f4 f5 f6 f7 f8 f9
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) (R _ r6)
        (R _ r7) (R _ r8) (R _ r9) =
  let p' = mkFieldUpdate9 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5
                                  f6 r6 f7 r7 f8 r8 f9 r9
  in wrapResult sr p' (wrapVal sr (modify v) p')

update10 :: WrapVal a => RefSrcPos -> RefExp -> R a -> (a -> a) 
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> RefAtom -> RefAtom -> RefAtom -> RefAtom -> RefAtom
        -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j -> R k
        -> R a
update10 sr p (R v rv) modify f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
        (R _ r1) (R _ r2) (R _ r3) (R _ r4) (R _ r5) (R _ r6)
        (R _ r7) (R _ r8) (R _ r9) (R _ r10) =
  let p' = mkFieldUpdate10 p sr rv f1 r1 f2 r2 f3 r3 f4 r4 f5 r5
                                   f6 r6 f7 r7 f8 r8 f9 r9 f10 r10
  in wrapResult sr p' (wrapVal sr (modify v) p')

-- ...

uupdate :: WrapVal a => RefExp -> R a -> (a -> a) -> R a
uupdate h (R v rv) modify = uwrapForward h (wrapVal mkNoSrcPos (modify v) h)


-- Combinator for projection (lambda-bound var in projective context)

projection :: RefSrcPos -> RefExp -> R a -> R a
projection sr p (R v rv) = R v (mkProjection p sr rv)

-- Combinators for literals

conChar :: RefSrcPos -> RefExp -> Char -> R Char
conChar sr p c = R c (mkChar p sr c)

conInteger :: RefSrcPos -> RefExp -> Integer -> R Integer
conInteger sr p i = 
  R i (if Prelude.toInteger (minBound::Int) <= i && 
          i <= Prelude.toInteger (maxBound::Int) 
         then mkInt p sr (Prelude.fromInteger i) 
         else mkInteger p sr (show i))

mkAtomRational :: RefSrcPos -> RefExp -> Rational -> RefExp
mkAtomRational sr p r =
  if Prelude.toInteger (minBound::Int) <= num && 
     num <= Prelude.toInteger (maxBound::Int) &&
     Prelude.toInteger (minBound::Int) <= denom && 
     denom <= Prelude.toInteger (maxBound::Int)
    then mkRat p sr (Prelude.fromInteger num) (Prelude.fromInteger denom)
    else mkRational p sr (show num) (show denom)
  where
  num = numerator r
  denom = denominator r

-- conRational undefinable
-- because original and transformed Rational are different

fromExpList :: RefSrcPos -> RefExp -> [R a] -> R (List a)
fromExpList sr h xs = 
  foldr (\y ys -> con2 sr h Cons aCons y ys) (con0 sr h List aList) xs

fromLitString :: RefSrcPos -> RefExp -> Prelude.String -> R (List Char)
fromLitString sr h xs = 
  foldr (\x xs -> con2 sr h Cons aCons (conChar sr h x) xs) 
    (con0 sr h List aList) xs


-- Create node directly with result
-- (to do: create efficient direct C-functions)

mkResApp2 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
mkResApp2 p sr f a1 a2 = 
  let ra = mkApp2 p sr f a1 a2
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp3 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp 
mkResApp3 p sr f a1 a2 a3 = 
  let ra = mkApp3 p sr f a1 a2 a3
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp4 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp 
mkResApp4 p sr f a1 a2 a3 a4 = 
  let ra = mkApp4 p sr f a1 a2 a3 a4
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp5 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp 
mkResApp5 p sr f a1 a2 a3 a4 a5 = 
  let ra = mkApp5 p sr f a1 a2 a3 a4 a5
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp6 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp 
mkResApp6 p sr f a1 a2 a3 a4 a5 a6 = 
  let ra = mkApp6 p sr f a1 a2 a3 a4 a5 a6
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp7 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
mkResApp7 p sr f a1 a2 a3 a4 a5 a6 a7 = 
  let ra = mkApp7 p sr f a1 a2 a3 a4 a5 a6 a7
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp8 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
mkResApp8 p sr f a1 a2 a3 a4 a5 a6 a7 a8 = 
  let ra = mkApp8 p sr f a1 a2 a3 a4 a5 a6 a7 a8
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp9 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp  
          -> RefExp 
mkResApp9 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 = 
  let ra = mkApp9 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp10 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp
mkResApp10 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = 
  let ra = mkApp10 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp11 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp
mkResApp11 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = 
  let ra = mkApp11 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp12 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp
mkResApp12 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = 
  let ra = mkApp12 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp13 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
mkResApp13 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = 
  let ra = mkApp13 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp14 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
mkResApp14 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 = 
  let ra = mkApp14 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 

mkResApp15 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp 
             -> RefExp
mkResApp15 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 = 
  let ra = mkApp15 p sr f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
  in entResult ra sr `seq` resResult ra ra sr `seq` ra 


-- optimising combinators for application of functions of known arity

-- traced function in traced or trusted application
app1 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
     -> (R a -> RefExp -> R z) -> R a -> R z
app1 sr srFun p af hf a@(R _ ra) =
  let rf = mkValueUse p srFun af
      r = mkApp1 p sr rf ra
  in wrapResult sr r (hf a r)

app2 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
     -> (R a -> R b -> RefExp -> R z) -> R a -> R b -> R z
app2 sr srFun p af hf a@(R _ ra) b@(R _ rb) =
  let rf = mkValueUse p srFun af
      r = mkApp2 p sr rf ra rb
  in wrapResult sr r (hf a b r)

app3 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
     -> (R a -> R b -> R c -> RefExp -> R z) -> R a -> R b -> R c -> R z
app3 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) =
  let rf = mkValueUse p srFun af
      r = mkApp3 p sr rf ra rb rc
  in wrapResult sr r (hf a b c r)

app4 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
     -> (R a -> R b -> R c -> R d -> RefExp -> R z) 
     -> R a -> R b -> R c -> R d -> R z
app4 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) =
  let rf = mkValueUse p srFun af
      r = mkApp4 p sr rf ra rb rc rd
  in wrapResult sr r (hf a b c d r)

app5 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
     -> (R a -> R b -> R c -> R d -> R e -> RefExp -> R z) 
     -> R a -> R b -> R c -> R d -> R e -> R z
app5 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) =
  let rf = mkValueUse p srFun af
      r = mkApp5 p sr rf ra rb rc rd re
  in wrapResult sr r (hf a b c d e r)

-- trusted function in traced application
uapp1 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
      -> (R a -> RefExp -> R z) -> R a -> R z
uapp1 sr srFun p af hf a@(R _ ra) =
  let rf = mkValueUse p srFun af
      r = mkApp1 p sr rf ra 
      h = mkHidden r
  in wrapResult sr r (wrapResult mkNoSrcPos h (hf a h))

uapp2 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
      -> (R a -> R b -> RefExp -> R z) -> R a -> R b -> R z
uapp2 sr srFun p af hf a@(R _ ra) b@(R _ rb) =
  let rf = mkValueUse p srFun af
      r = mkApp2 p sr rf ra rb
      h = mkHidden r
  in wrapResult sr r (wrapResult mkNoSrcPos h (hf a b h))

uapp3 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
      -> (R a -> R b -> R c -> RefExp -> R z) -> R a -> R b -> R c -> R z
uapp3 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) =
  let rf = mkValueUse p srFun af
      r = mkApp3 p sr rf ra rb rc
      h = mkHidden r
  in wrapResult sr r (wrapResult mkNoSrcPos h (hf a b c h))

uapp4 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
      -> (R a -> R b -> R c -> R d -> RefExp -> R z) 
      -> R a -> R b -> R c -> R d -> R z
uapp4 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) =
  let rf = mkValueUse p srFun af
      r = mkApp4 p sr rf ra rb rc rd
      h = mkHidden r
  in wrapResult sr r (wrapResult mkNoSrcPos h (hf a b c d h))

uapp5 :: RefSrcPos -> RefSrcPos -> RefExp -> RefAtom 
      -> (R a -> R b -> R c -> R d -> R e -> RefExp -> R z) 
      -> R a -> R b -> R c -> R d -> R e -> R z
uapp5 sr srFun p af hf a@(R _ ra) b@(R _ rb) c@(R _ rc) d@(R _ rd) e@(R _ re) =
  let rf = mkValueUse p srFun af
      r = mkApp5 p sr rf ra rb rc rd re
      h = mkHidden r
  in wrapResult sr r (wrapResult mkNoSrcPos h (hf a b c d e h))

-- for trusted function in trusted application no new combinator

-- ---------------------------------------------------------------------------
-- Interface to foreign C-functions for writing the ART trace.

openTrace :: String -> Prelude.IO ()
openTrace progname = withCString progname openTrace'

FOREIGN("hat-c.h hat_Open")
  openTrace' :: CString -> Prelude.IO ()

FOREIGN("hat-c.h hat_Close")
  closeTrace :: Prelude.IO ()

hatAborted :: String -> Prelude.IO ()
hatAborted msg = withCString msg hatAbort'

FOREIGN("hat-c.h hat_Abort")
  hatAbort' :: CString -> Prelude.IO ()

errorTraceExit :: String -> RefExp -> Int -> ()
errorTraceExit msg ref no = 
  unsafePerformIO $
    withCString msg (\msg'->errorTraceExit' msg' ref no)

FOREIGN("hat-c.h hat_ErrorExit")
  errorTraceExit' :: CString -> RefExp -> Int -> Prelude.IO ()

outputTrace :: RefExp -> String -> Prelude.IO ()
outputTrace trace output = withCString output (outputTrace' trace)

FOREIGN("hat-c.h hat_OutputTrace")
  outputTrace' :: RefExp -> CString -> Prelude.IO ()

FOREIGN("hat-c.h hat_Hidden")
  hidden :: RefExp -> Bool

FOREIGN("hat-c.h mkRoot")
  mkRoot :: RefExp

mkModule :: String -> String -> Bool -> RefModule
mkModule modName srcFile traced = 
  unsafePerformIO $
    withCString modName (\modName' ->
    withCString srcFile (\srcFile' ->
      mkModule' modName' srcFile' traced))

FOREIGN("hat-c.h mkModule")
  mkModule' :: CString -> CString -> Bool -> Prelude.IO RefModule

FOREIGN("hat-c.h mkSrcPos")
  mkSrcPos :: RefModule -> Loc -> Loc -> RefSrcPos

-- Exp nodes

FOREIGN("hat-c.h mkResApp1")
  mkResApp1 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp1")
  mkApp1 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp2")
  mkApp2 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp3")
  mkApp3 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp

FOREIGN("hat-c.h mkApp4")
  mkApp4 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp5")
  mkApp5 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp6")
  mkApp6 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp7")
  mkApp7 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp8")
  mkApp8 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp9")
  mkApp9 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp10")
  mkApp10 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp

FOREIGN("hat-c.h mkApp11")
  mkApp11 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp12")
  mkApp12 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp13")
  mkApp13 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp14")
  mkApp14 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkApp15")
  mkApp15 :: RefExp -> RefSrcPos -> RefExp -> RefExp -> RefExp -> RefExp 
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
          -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp1")
  mkValueApp1 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp2")
  mkValueApp2 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp3")
  mkValueApp3 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp

FOREIGN("hat-c.h mkValueApp4")
  mkValueApp4 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp5")
  mkValueApp5 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp6")
  mkValueApp6 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp7")
  mkValueApp7 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp8")
  mkValueApp8 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp9")
  mkValueApp9 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp10")
  mkValueApp10 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp

FOREIGN("hat-c.h mkValueApp11")
  mkValueApp11 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp12")
  mkValueApp12 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp13")
  mkValueApp13 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp14")
  mkValueApp14 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkValueApp15")
  mkValueApp15 :: RefExp -> RefSrcPos -> RefAtom -> RefExp -> RefExp -> RefExp 
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp
         -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp -> RefExp

FOREIGN("hat-c.h mkChar")
  mkChar :: RefExp -> RefSrcPos -> Char -> RefExp

FOREIGN("hat-c.h mkInt")
  mkInt :: RefExp -> RefSrcPos -> Int -> RefExp

mkInteger :: RefExp -> RefSrcPos -> String -> RefExp
mkInteger parent use integer = 
  unsafePerformIO $
    withCString integer (\integer' -> mkInteger' parent use integer')

FOREIGN("hat-c.h mkInteger")
  mkInteger' :: RefExp -> RefSrcPos -> CString -> Prelude.IO RefExp

FOREIGN("hat-c.h mkRat")
  mkRat :: RefExp -> RefSrcPos -> Int -> Int -> RefExp

mkRational :: RefExp -> RefSrcPos -> String -> String -> RefExp
mkRational parent use num denom = 
  unsafePerformIO $
    withCString num (\num' ->
    withCString denom (\denom' -> mkRational' parent use num' denom'))

FOREIGN("hat-c.h mkRational")
  mkRational' :: RefExp -> RefSrcPos -> CString -> CString -> Prelude.IO RefExp

FOREIGN("hat-c.h mkFloat")
  mkFloat :: RefExp -> RefSrcPos -> Float -> RefExp

FOREIGN("hat-c.h mkDouble")
  mkDouble :: RefExp -> RefSrcPos -> Double -> RefExp

FOREIGN("hat-c.h mkValueUse")
  mkValueUse :: RefExp -> RefSrcPos -> RefAtom -> RefExp

FOREIGN("hat-c.h mkConstUse")
  mkConstUse :: RefExp -> RefSrcPos -> RefExp -> RefExp

FOREIGN("hat-c.h mkConstDef")
  mkConstDef :: RefExp -> RefAtom -> RefExp

FOREIGN("hat-c.h mkGuard")
  mkGuard :: RefExp -> RefSrcPos -> RefExp -> RefExp

FOREIGN("hat-c.h mkCase")
  mkCase :: RefExp -> RefSrcPos -> RefExp -> RefExp

FOREIGN("hat-c.h mkIf")
  mkIf :: RefExp -> RefSrcPos -> RefExp -> RefExp

FOREIGN("hat-c.h mkFieldUpdate1")
  mkFieldUpdate1 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp 
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate2")
  mkFieldUpdate2 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate3")
  mkFieldUpdate3 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate4")
  mkFieldUpdate4 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate5")
  mkFieldUpdate5 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate6")
  mkFieldUpdate6 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate7")
  mkFieldUpdate7 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate8")
  mkFieldUpdate8 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate9")
  mkFieldUpdate9 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkFieldUpdate10")
  mkFieldUpdate10 :: RefExp -> RefSrcPos -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp -> RefAtom -> RefExp -> RefAtom -> RefExp
                 -> RefAtom -> RefExp
                 -> RefExp

FOREIGN("hat-c.h mkProjection")
  mkProjection :: RefExp -> RefSrcPos -> RefExp -> RefExp

FOREIGN("hat-c.h mkHidden")
  mkHidden :: RefExp -> RefExp

FOREIGN("hat-c.h mkForward")
  mkForward :: RefExp -> RefExp

FOREIGN("hat-c.h mkDoStmt")
  mkDoStmt :: RefExp -> RefExp

FOREIGN("hat-c.h mkLambda")
  mkLambda :: RefAtom

FOREIGN("hat-c.h mkDoLambda")
  mkDoLambda :: RefAtom

mkVariable :: RefModule -> Loc -> Loc -> Fixity -> Arity -> String 
           -> Bool -> RefAtom
mkVariable mod begin end fixity arity name local =
  unsafePerformIO $
    withCString name 
      (\name' -> mkVariable' mod begin end fixity arity name' local)

FOREIGN("hat-c.h mkVariable")
  mkVariable' :: RefModule -> Loc -> Loc -> Fixity -> Arity -> CString -> Bool 
              -> Prelude.IO RefAtom

mkConstructor :: RefModule -> Loc -> Loc -> Fixity -> Arity 
              -> String -> RefAtom
mkConstructor mod begin end fixity arity name =
  unsafePerformIO $
    withCString name 
      (\name' -> mkConstructor' mod begin end fixity arity name')

FOREIGN("hat-c.h mkConstructor")
  mkConstructor' :: RefModule -> Loc -> Loc -> Fixity -> Arity -> CString 
                 -> Prelude.IO RefAtom

mkConstructorWFields :: RefModule -> Loc -> Loc -> Fixity -> Arity -> String
                     -> [RefAtom] -> RefAtom
mkConstructorWFields mod begin end fixity arity name labels =
  unsafePerformIO $
    withCString name (\name' ->
    withArray (map (\(RA i) -> i) labels) (\labels' ->
      mkConstructorWFields' mod begin end fixity arity name' labels'))

FOREIGN("hat-c.h mkConstructorWFields")
  mkConstructorWFields' :: RefModule -> Loc -> Loc -> Fixity -> Arity 
                        -> CString -> Ptr Int -> Prelude.IO RefAtom

mkAbstract :: String -> RefAtom
mkAbstract txt = unsafePerformIO $
  withCString txt (\txt' -> mkAbstract' txt')

FOREIGN("hat-c.h mkAbstract")
  mkAbstract' :: CString -> Prelude.IO RefAtom

-- Update node that it was entered

FOREIGN("hat-c.h entResult")
  entResult :: RefExp -> RefSrcPos -> ()

FOREIGN("hat-c.h entForward")
  entForward :: RefExp -> RefExp -> ()

-- Update node with result

FOREIGN("hat-c.h resResult")
  resResult :: RefExp -> RefExp -> RefSrcPos -> ()

FOREIGN("hat-c.h resForward")
  resForward :: RefExp -> RefExp -> ()

FOREIGN("hat-c.h recordChild")
  recordChild :: RefExp -> RefExp -> ()

-- ----------------------------------------------------------------------------
-- End
