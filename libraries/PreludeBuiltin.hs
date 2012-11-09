-- Contains foreign haskell declarations for all functions
-- not (portably) definable in Haskell.
module PreludeBuiltin (
  -- reexport from module PreludeBuiltinTypes   
  (->),Bool(True,False),Char,Int,Integer,Float,Double,IOError 
  ,[]((:),[]),IO
  ,()(())
  ,(,)((,)), (,,)((,,)),(,,,)((,,,)),(,,,,)((,,,,)),(,,,,,)((,,,,,))
  ,(,,,,,,)((,,,,,,)),(,,,,,,,)((,,,,,,,)),(,,,,,,,,)((,,,,,,,,))
  ,(,,,,,,,,,)((,,,,,,,,,)),(,,,,,,,,,,)((,,,,,,,,,,))
  ,(,,,,,,,,,,,)((,,,,,,,,,,,)),(,,,,,,,,,,,,)((,,,,,,,,,,,,))
  ,(,,,,,,,,,,,,,)((,,,,,,,,,,,,,)),(,,,,,,,,,,,,,,)((,,,,,,,,,,,,,,))
  ,String,error,undefined 
  -- general (->) 
  ,seq
  -- char 
-- ,Char	-- already re-exported above
  ,isAscii,isLatin1,isControl,isPrint,isSpace,isUpper,isLower,isAlpha,isDigit
  ,isOctDigit,isHexDigit,isAlphaNum,toUpper,toLower
  ,primIntToChar,primCharToInt,primUnicodeMaxBound 
  -- numeric
-- ,Int,Integer,Float,Double -- (,) 	-- already re-exported above
  ,primIntMinBound,primIntMaxBound
  ,primIntEq,primIntNe,primIntLt,primIntLe,primIntGt,primIntGe
  ,primIntQuot,primIntRem,primIntPlus,primIntMinus,primIntTimes
  ,primIntNegate,primIntAbs,primIntSignum,primIntegerFromInt
  ,primIntFromInteger,primIntegerEq,primIntegerNe,primIntegerLt
  ,primIntegerLe,primIntegerGt,primIntegerGe,primIntegerQuot
  ,primIntegerRem,primIntegerQuotRem ,primIntegerAdd
  ,primIntegerSub,primIntegerMul,primIntegerNeg
  ,primFloatFromInteger,primFloatRadix,primFloatDigits,primFloatRange
  ,primDecodeFloat,primEncodeFloat,primFloatIsNaN,primFloatIsInfinite
  ,primFloatIsDenormalized,primFloatIsNegativeZero,primFloatIsIEEE
  ,primFloatEq,primFloatNe,primFloatLt,primFloatLe,primFloatGt
  ,primFloatGe,primFloatPi,primFloatExp,primFloatLog,primFloatSqrt
  ,primFloatSin,primFloatCos,primFloatTan,primFloatAsin
  ,primFloatAcos,primFloatAtan,primFloatDiv,primFloatAdd
  ,primFloatSub,primFloatMul,primFloatAbs,primFloatSignum
  ,primDoubleFromInteger,primDoubleRadix,primDoubleDigits,primDoubleRange
  ,primDecodeDouble,primEncodeDouble,primDoubleIsNaN,primDoubleIsInfinite
  ,primDoubleIsDenormalized,primDoubleIsNegativeZero,primDoubleIsIEEE
  ,primDoubleEq,primDoubleNe,primDoubleLt,primDoubleLe
  ,primDoubleGt,primDoubleGe,primDoublePi,primDoubleExp,primDoubleLog
  ,primDoubleSqrt,primDoubleSin,primDoubleCos,primDoubleTan
  ,primDoubleAsin,primDoubleAcos,primDoubleAtan,primDoubleDiv
  ,primDoubleAdd,primDoubleSub,primDoubleMul,primDoubleAbs
  ,primDoubleSignum 
  -- IO 
-- ,IO,IOError -- ()	-- already re-exported above
  ,primIOBind,primIOReturn
  ,ioError,userError,catch,putChar,getChar,getContents,readFile,writeFile
  ,appendFile,primIOErrorShow{- ,primIOErrorEq -} 
  ) where

import PreludeBuiltinTypes
import NotHat.Hat.Hat  -- for some primitive types
import qualified NotHat.Prelude  -- not to be transformed
import qualified NotHat.System.IO.Error  -- for catch
import qualified NotHat.Data.Char as NotHat.Char  -- not to be transformed

-- types appearing here
-- (->), String, Char, Bool, Int, Integer, Float, Double, (,), IO, IOError, ()

-- general:

infixr 0 `seq`

foreign import haskell "Prelude.seq"
  seq :: a -> b -> b

foreign import haskell "Char.isAscii"
  isAscii :: Char -> Bool
foreign import haskell "Char.isLatin1"
  isLatin1 :: Char -> Bool
foreign import haskell "Char.isControl"
  isControl :: Char -> Bool
foreign import haskell "Char.isPrint"
  isPrint :: Char -> Bool
foreign import haskell "Char.isSpace"
  isSpace :: Char -> Bool
foreign import haskell "Char.isUpper"
  isUpper :: Char -> Bool
foreign import haskell "Char.isLower"
  isLower :: Char -> Bool
foreign import haskell "Char.isAlpha"
  isAlpha :: Char -> Bool
foreign import haskell "Char.isDigit"
  isDigit :: Char -> Bool
foreign import haskell "Char.isOctDigit"
  isOctDigit :: Char -> Bool
foreign import haskell "Char.isHexDigit"
  isHexDigit :: Char -> Bool
foreign import haskell "Char.isAlphaNum"
  isAlphaNum :: Char -> Bool

foreign import haskell "Char.toUpper"
  toUpper :: Char -> Char
foreign import haskell "Char.toLower"
  toLower :: Char -> Char

-- Character code functions
foreign import haskell "Char.ord"
  primCharToInt :: Char -> Int
foreign import haskell "Char.chr"
  primIntToChar :: Int  -> Char

foreign import haskell "Prelude.maxBound"
  primUnicodeMaxBound :: Char


-- Numerics

-- system dependent
foreign import haskell "Prelude.minBound"
  primIntMinBound :: Int
foreign import haskell "Prelude.maxBound"
  primIntMaxBound :: Int

foreign import haskell "Prelude.=="
  primIntEq :: Int -> Int -> Bool
foreign import haskell "Prelude./="
  primIntNe :: Int -> Int -> Bool
foreign import haskell "Prelude.<"
  primIntLt :: Int -> Int -> Bool
foreign import haskell "Prelude.<="
  primIntLe :: Int -> Int -> Bool
foreign import haskell "Prelude.>"
  primIntGt :: Int -> Int -> Bool
foreign import haskell "Prelude.>="
  primIntGe :: Int -> Int -> Bool
foreign import haskell "Prelude.quot"
  primIntQuot   :: Int -> Int -> Int
foreign import haskell "Prelude.rem"
  primIntRem    :: Int -> Int -> Int
foreign import haskell "Prelude.+"
  primIntPlus   :: Int -> Int -> Int
foreign import haskell "Prelude.-"
  primIntMinus  :: Int -> Int -> Int
foreign import haskell "Prelude.*"
  primIntTimes  :: Int -> Int -> Int
foreign import haskell "Prelude.negate"
  primIntNegate :: Int -> Int
foreign import haskell "Prelude.abs"
  primIntAbs    :: Int -> Int
foreign import haskell "Prelude.signum"
  primIntSignum :: Int -> Int

foreign import haskell "Prelude.toInteger"
  primIntegerFromInt :: Int -> Integer
foreign import haskell "Prelude.fromInteger"
  primIntFromInteger :: Integer -> Int

foreign import haskell "Prelude.=="
  primIntegerEq :: Integer -> Integer -> Bool
foreign import haskell "Prelude./="
  primIntegerNe :: Integer -> Integer -> Bool
foreign import haskell "Prelude.<"
  primIntegerLt :: Integer -> Integer -> Bool
foreign import haskell "Prelude.<="
  primIntegerLe :: Integer -> Integer -> Bool
foreign import haskell "Prelude.>"
  primIntegerGt :: Integer -> Integer -> Bool
foreign import haskell "Prelude.>="
  primIntegerGe :: Integer -> Integer -> Bool
foreign import haskell "Prelude.quot"
  primIntegerQuot    :: Integer -> Integer -> Integer
foreign import haskell "Prelude.rem"
  primIntegerRem     :: Integer -> Integer -> Integer
foreign import haskell "Prelude.quotRem"
  primIntegerQuotRem :: Integer -> Integer -> (Integer,Integer)
foreign import haskell "Prelude.+"
  primIntegerAdd     :: Integer -> Integer -> Integer
foreign import haskell "Prelude.-"
  primIntegerSub     :: Integer -> Integer -> Integer
foreign import haskell "Prelude.*"
  primIntegerMul     :: Integer -> Integer -> Integer
foreign import haskell "Prelude.negate"
  primIntegerNeg     :: Integer -> Integer

foreign import haskell "Prelude.fromInteger"
  primFloatFromInteger  :: Integer -> Float
foreign import haskell "Prelude.floatRadix"
  primFloatRadix        :: Float -> Integer
foreign import haskell "Prelude.floatDigits"
  primFloatDigits       :: Float -> Int
foreign import haskell "Prelude.floatRange"
  primFloatRange        :: Float -> (Int,Int) 
foreign import haskell "Prelude.decodeFloat"
  primDecodeFloat       :: Float -> (Integer,Int)
foreign import haskell "Prelude.encodeFloat"
  primEncodeFloat       :: Integer -> Int -> Float
foreign import haskell "Prelude.isNaN"
  primFloatIsNaN   :: Float -> Bool
foreign import haskell "Prelude.isInfinite"
  primFloatIsInfinite   :: Float -> Bool
foreign import haskell "Prelude.isDenormalized"
  primFloatIsDenormalized   :: Float -> Bool
foreign import haskell "Prelude.isNegativeZero"
  primFloatIsNegativeZero   :: Float -> Bool
foreign import haskell "Prelude.isIEEE"
  primFloatIsIEEE   :: Float -> Bool

foreign import haskell "Prelude.=="
  primFloatEq :: Float -> Float -> Bool
foreign import haskell "Prelude./="
  primFloatNe :: Float -> Float -> Bool
foreign import haskell "Prelude.<"
  primFloatLt :: Float -> Float -> Bool
foreign import haskell "Prelude.<="
  primFloatLe :: Float -> Float -> Bool
foreign import haskell "Prelude.>"
  primFloatGt :: Float -> Float -> Bool
foreign import haskell "Prelude.>="
  primFloatGe :: Float -> Float -> Bool
foreign import haskell "Prelude.pi"
  primFloatPi :: Float
foreign import haskell "Prelude.exp"
  primFloatExp  :: Float -> Float
foreign import haskell "Prelude.log"
  primFloatLog  :: Float -> Float
foreign import haskell "Prelude.sqrt"
  primFloatSqrt :: Float -> Float
foreign import haskell "Prelude.sin"
  primFloatSin  :: Float -> Float
foreign import haskell "Prelude.cos"
  primFloatCos  :: Float -> Float
foreign import haskell "Prelude.tan"
  primFloatTan  :: Float -> Float
foreign import haskell "Prelude.asin"
  primFloatAsin :: Float -> Float
foreign import haskell "Prelude.acos"
  primFloatAcos :: Float -> Float
foreign import haskell "Prelude.atan"
  primFloatAtan :: Float -> Float
foreign import haskell "Prelude./"
  primFloatDiv  :: Float -> Float -> Float
foreign import haskell "Prelude.+"
  primFloatAdd  :: Float -> Float -> Float
foreign import haskell "Prelude.-"
  primFloatSub  :: Float -> Float -> Float
foreign import haskell "Prelude.*"
  primFloatMul  :: Float -> Float -> Float
foreign import haskell "Prelude.abs"
  primFloatAbs    :: Float -> Float
foreign import haskell "Prelude.signum"
  primFloatSignum :: Float -> Float

foreign import haskell "Prelude.fromInteger"
  primDoubleFromInteger :: Integer -> Double
foreign import haskell "Prelude.floatRadix"
  primDoubleRadix   :: Double -> Integer
foreign import haskell "Prelude.floatDigits"
  primDoubleDigits  :: Double -> Int
foreign import haskell "Prelude.floatRange"
  primDoubleRange   :: Double -> (Int,Int)
foreign import haskell "Prelude.decodeFloat"
  primDecodeDouble :: Double -> (Integer,Int)
foreign import haskell "Prelude.encodeFloat"
  primEncodeDouble :: Integer -> Int -> Double
foreign import haskell "Prelude.isNaN"
  primDoubleIsNaN   :: Double -> Bool
foreign import haskell "Prelude.isInfinite"
  primDoubleIsInfinite   :: Double -> Bool
foreign import haskell "Prelude.isDenormalized"
  primDoubleIsDenormalized   :: Double -> Bool
foreign import haskell "Prelude.isNegativeZero"
  primDoubleIsNegativeZero   :: Double -> Bool
foreign import haskell "Prelude.isIEEE"
  primDoubleIsIEEE   :: Double -> Bool

foreign import haskell "Prelude.=="
  primDoubleEq :: Double -> Double -> Bool
foreign import haskell "Prelude./="
  primDoubleNe :: Double -> Double -> Bool
foreign import haskell "Prelude.<"
  primDoubleLt :: Double -> Double -> Bool
foreign import haskell "Prelude.<="
  primDoubleLe :: Double -> Double -> Bool
foreign import haskell "Prelude.>"
  primDoubleGt :: Double -> Double -> Bool
foreign import haskell "Prelude.>="
  primDoubleGe :: Double -> Double -> Bool
foreign import haskell "Prelude.pi"
  primDoublePi :: Double
foreign import haskell "Prelude.exp"
  primDoubleExp  :: Double -> Double
foreign import haskell "Prelude.log"
  primDoubleLog  :: Double -> Double
foreign import haskell "Prelude.sqrt"
  primDoubleSqrt :: Double -> Double
foreign import haskell "Prelude.sin"
  primDoubleSin  :: Double -> Double
foreign import haskell "Prelude.cos"
  primDoubleCos  :: Double -> Double
foreign import haskell "Prelude.tan"
  primDoubleTan  :: Double -> Double
foreign import haskell "Prelude.asin"
  primDoubleAsin :: Double -> Double
foreign import haskell "Prelude.acos"
  primDoubleAcos :: Double -> Double
foreign import haskell "Prelude.atan"
  primDoubleAtan :: Double -> Double
foreign import haskell "Prelude./"
  primDoubleDiv  :: Double -> Double -> Double
foreign import haskell "Prelude.+"
  primDoubleAdd  :: Double -> Double -> Double
foreign import haskell "Prelude.-"
  primDoubleSub  :: Double -> Double -> Double
foreign import haskell "Prelude.*"
  primDoubleMul  :: Double -> Double -> Double
foreign import haskell "Prelude.abs"
  primDoubleAbs    :: Double -> Double
foreign import haskell "Prelude.signum"
  primDoubleSignum :: Double -> Double


-- IO

foreign import haskell "Prelude.>>="
  primIOBind :: IO a -> (a -> IO b) -> IO b
foreign import haskell "Prelude.return"
  primIOReturn :: a -> IO a


foreign import haskell "Prelude.show"
  primIOErrorShow :: IOError -> String

{- currently excluded because instance Eq IOError missing in ghc 5.02
foreign import haskell "Prelude.=="
  primIOErrorEq :: IOError -> IOError -> Bool
-}


foreign import haskell "Prelude.ioError"
  ioError ::  IOError -> IO a 

foreign import haskell "Prelude.userError"
  userError ::  String -> IOError

foreign import haskell "System.IO.Error.catchIOError"
  catch ::  IO a -> (IOError -> IO a) -> IO a 

-- foreign import haskell "Prelude.putChar"
--   putChar :: Char -> IO ()
-- HACK!
foreign import haskell 
  "(\\_ c -> T.outputTrace kputChar [c] Prelude.>> Prelude.putChar c) Prelude.True"
  putChar :: Char -> IO ()

foreign import haskell "Prelude.getChar"
  getChar          :: IO Char

foreign import haskell "Prelude.getContents"
  getContents      :: IO String

foreign import haskell "Prelude.readFile"
  readFile         :: String -> IO String

foreign import haskell "(\\_ n s -> T.outputTrace kwriteFile s Prelude.>> Prelude.writeFile n s) Prelude.True"
  writeFile        :: String -> String -> IO ()

foreign import haskell "(\\_ n s -> T.outputTrace kappendFile s Prelude.>> Prelude.appendFile n s) Prelude.True"
  appendFile       :: String -> String -> IO ()

