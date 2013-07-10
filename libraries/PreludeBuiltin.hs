-- Contains foreign haskell declarations for all functions
-- not (portably) definable in Haskell.
module PreludeBuiltin (
  -- reexport from module PreludeBuiltinTypes   
  -- (->)
  Bool(True,False),Char,Int,Integer,Float,Double,IOError 
  -- ,[]((:),[]),IO
  -- ,()(())
  -- ,(,)((,)), (,,)((,,)),(,,,)((,,,)),(,,,,)((,,,,)),(,,,,,)((,,,,,))
  -- ,(,,,,,,)((,,,,,,)),(,,,,,,,)((,,,,,,,)),(,,,,,,,,)((,,,,,,,,))
  -- ,(,,,,,,,,,)((,,,,,,,,,)),(,,,,,,,,,,)((,,,,,,,,,,))
  -- ,(,,,,,,,,,,,)((,,,,,,,,,,,)),(,,,,,,,,,,,,)((,,,,,,,,,,,,))
  -- ,(,,,,,,,,,,,,,)((,,,,,,,,,,,,,)),(,,,,,,,,,,,,,,)((,,,,,,,,,,,,,,))
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

foreign import ccall "NotHat.Prelude.seq"
  seq :: a -> b -> b

foreign import ccall "NotHat.Char.isAscii"
  isAscii :: Char -> Bool
foreign import ccall "NotHat.Char.isLatin1"
  isLatin1 :: Char -> Bool
foreign import ccall "NotHat.Char.isControl"
  isControl :: Char -> Bool
foreign import ccall "NotHat.Char.isPrint"
  isPrint :: Char -> Bool
foreign import ccall "NotHat.Char.isSpace"
  isSpace :: Char -> Bool
foreign import ccall "NotHat.Char.isUpper"
  isUpper :: Char -> Bool
foreign import ccall "NotHat.Char.isLower"
  isLower :: Char -> Bool
foreign import ccall "NotHat.Char.isAlpha"
  isAlpha :: Char -> Bool
foreign import ccall "NotHat.Char.isDigit"
  isDigit :: Char -> Bool
foreign import ccall "NotHat.Char.isOctDigit"
  isOctDigit :: Char -> Bool
foreign import ccall "NotHat.Char.isHexDigit"
  isHexDigit :: Char -> Bool
foreign import ccall "NotHat.Char.isAlphaNum"
  isAlphaNum :: Char -> Bool

foreign import ccall "NotHat.Char.toUpper"
  toUpper :: Char -> Char
foreign import ccall "NotHat.Char.toLower"
  toLower :: Char -> Char

-- Character code functions
foreign import ccall "NotHat.Char.ord"
  primCharToInt :: Char -> Int
foreign import ccall "NotHat.Char.chr"
  primIntToChar :: Int  -> Char

foreign import ccall "NotHat.Prelude.maxBound"
  primUnicodeMaxBound :: Char


-- Numerics

-- system dependent
foreign import ccall "NotHat.Prelude.minBound"
  primIntMinBound :: Int
foreign import ccall "NotHat.Prelude.maxBound"
  primIntMaxBound :: Int

foreign import ccall "NotHat.Prelude.=="
  primIntEq :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude./="
  primIntNe :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude.<"
  primIntLt :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude.<="
  primIntLe :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude.>"
  primIntGt :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude.>="
  primIntGe :: Int -> Int -> Bool
foreign import ccall "NotHat.Prelude.quot"
  primIntQuot   :: Int -> Int -> Int
foreign import ccall "NotHat.Prelude.rem"
  primIntRem    :: Int -> Int -> Int
foreign import ccall "NotHat.Prelude.+"
  primIntPlus   :: Int -> Int -> Int
foreign import ccall "NotHat.Prelude.-"
  primIntMinus  :: Int -> Int -> Int
foreign import ccall "NotHat.Prelude.*"
  primIntTimes  :: Int -> Int -> Int
foreign import ccall "NotHat.Prelude.negate"
  primIntNegate :: Int -> Int
foreign import ccall "NotHat.Prelude.abs"
  primIntAbs    :: Int -> Int
foreign import ccall "NotHat.Prelude.signum"
  primIntSignum :: Int -> Int

foreign import ccall "NotHat.Prelude.toInteger"
  primIntegerFromInt :: Int -> Integer
foreign import ccall "NotHat.Prelude.fromInteger"
  primIntFromInteger :: Integer -> Int

foreign import ccall "NotHat.Prelude.=="
  primIntegerEq :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude./="
  primIntegerNe :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude.<"
  primIntegerLt :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude.<="
  primIntegerLe :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude.>"
  primIntegerGt :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude.>="
  primIntegerGe :: Integer -> Integer -> Bool
foreign import ccall "NotHat.Prelude.quot"
  primIntegerQuot    :: Integer -> Integer -> Integer
foreign import ccall "NotHat.Prelude.rem"
  primIntegerRem     :: Integer -> Integer -> Integer
foreign import ccall "NotHat.Prelude.quotRem"
  primIntegerQuotRem :: Integer -> Integer -> (Integer,Integer)
foreign import ccall "NotHat.Prelude.+"
  primIntegerAdd     :: Integer -> Integer -> Integer
foreign import ccall "NotHat.Prelude.-"
  primIntegerSub     :: Integer -> Integer -> Integer
foreign import ccall "NotHat.Prelude.*"
  primIntegerMul     :: Integer -> Integer -> Integer
foreign import ccall "NotHat.Prelude.negate"
  primIntegerNeg     :: Integer -> Integer

foreign import ccall "NotHat.Prelude.fromInteger"
  primFloatFromInteger  :: Integer -> Float
foreign import ccall "NotHat.Prelude.floatRadix"
  primFloatRadix        :: Float -> Integer
foreign import ccall "NotHat.Prelude.floatDigits"
  primFloatDigits       :: Float -> Int
foreign import ccall "NotHat.Prelude.floatRange"
  primFloatRange        :: Float -> (Int,Int) 
foreign import ccall "NotHat.Prelude.decodeFloat"
  primDecodeFloat       :: Float -> (Integer,Int)
foreign import ccall "NotHat.Prelude.encodeFloat"
  primEncodeFloat       :: Integer -> Int -> Float
foreign import ccall "NotHat.Prelude.isNaN"
  primFloatIsNaN   :: Float -> Bool
foreign import ccall "NotHat.Prelude.isInfinite"
  primFloatIsInfinite   :: Float -> Bool
foreign import ccall "NotHat.Prelude.isDenormalized"
  primFloatIsDenormalized   :: Float -> Bool
foreign import ccall "NotHat.Prelude.isNegativeZero"
  primFloatIsNegativeZero   :: Float -> Bool
foreign import ccall "NotHat.Prelude.isIEEE"
  primFloatIsIEEE   :: Float -> Bool

foreign import ccall "NotHat.Prelude.=="
  primFloatEq :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude./="
  primFloatNe :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude.<"
  primFloatLt :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude.<="
  primFloatLe :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude.>"
  primFloatGt :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude.>="
  primFloatGe :: Float -> Float -> Bool
foreign import ccall "NotHat.Prelude.pi"
  primFloatPi :: Float
foreign import ccall "NotHat.Prelude.exp"
  primFloatExp  :: Float -> Float
foreign import ccall "NotHat.Prelude.log"
  primFloatLog  :: Float -> Float
foreign import ccall "NotHat.Prelude.sqrt"
  primFloatSqrt :: Float -> Float
foreign import ccall "NotHat.Prelude.sin"
  primFloatSin  :: Float -> Float
foreign import ccall "NotHat.Prelude.cos"
  primFloatCos  :: Float -> Float
foreign import ccall "NotHat.Prelude.tan"
  primFloatTan  :: Float -> Float
foreign import ccall "NotHat.Prelude.asin"
  primFloatAsin :: Float -> Float
foreign import ccall "NotHat.Prelude.acos"
  primFloatAcos :: Float -> Float
foreign import ccall "NotHat.Prelude.atan"
  primFloatAtan :: Float -> Float
foreign import ccall "NotHat.Prelude./"
  primFloatDiv  :: Float -> Float -> Float
foreign import ccall "NotHat.Prelude.+"
  primFloatAdd  :: Float -> Float -> Float
foreign import ccall "NotHat.Prelude.-"
  primFloatSub  :: Float -> Float -> Float
foreign import ccall "NotHat.Prelude.*"
  primFloatMul  :: Float -> Float -> Float
foreign import ccall "NotHat.Prelude.abs"
  primFloatAbs    :: Float -> Float
foreign import ccall "NotHat.Prelude.signum"
  primFloatSignum :: Float -> Float

foreign import ccall "NotHat.Prelude.fromInteger"
  primDoubleFromInteger :: Integer -> Double
foreign import ccall "NotHat.Prelude.floatRadix"
  primDoubleRadix   :: Double -> Integer
foreign import ccall "NotHat.Prelude.floatDigits"
  primDoubleDigits  :: Double -> Int
foreign import ccall "NotHat.Prelude.floatRange"
  primDoubleRange   :: Double -> (Int,Int)
foreign import ccall "NotHat.Prelude.decodeFloat"
  primDecodeDouble :: Double -> (Integer,Int)
foreign import ccall "NotHat.Prelude.encodeFloat"
  primEncodeDouble :: Integer -> Int -> Double
foreign import ccall "NotHat.Prelude.isNaN"
  primDoubleIsNaN   :: Double -> Bool
foreign import ccall "NotHat.Prelude.isInfinite"
  primDoubleIsInfinite   :: Double -> Bool
foreign import ccall "NotHat.Prelude.isDenormalized"
  primDoubleIsDenormalized   :: Double -> Bool
foreign import ccall "NotHat.Prelude.isNegativeZero"
  primDoubleIsNegativeZero   :: Double -> Bool
foreign import ccall "NotHat.Prelude.isIEEE"
  primDoubleIsIEEE   :: Double -> Bool

foreign import ccall "NotHat.Prelude.=="
  primDoubleEq :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude./="
  primDoubleNe :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude.<"
  primDoubleLt :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude.<="
  primDoubleLe :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude.>"
  primDoubleGt :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude.>="
  primDoubleGe :: Double -> Double -> Bool
foreign import ccall "NotHat.Prelude.pi"
  primDoublePi :: Double
foreign import ccall "NotHat.Prelude.exp"
  primDoubleExp  :: Double -> Double
foreign import ccall "NotHat.Prelude.log"
  primDoubleLog  :: Double -> Double
foreign import ccall "NotHat.Prelude.sqrt"
  primDoubleSqrt :: Double -> Double
foreign import ccall "NotHat.Prelude.sin"
  primDoubleSin  :: Double -> Double
foreign import ccall "NotHat.Prelude.cos"
  primDoubleCos  :: Double -> Double
foreign import ccall "NotHat.Prelude.tan"
  primDoubleTan  :: Double -> Double
foreign import ccall "NotHat.Prelude.asin"
  primDoubleAsin :: Double -> Double
foreign import ccall "NotHat.Prelude.acos"
  primDoubleAcos :: Double -> Double
foreign import ccall "NotHat.Prelude.atan"
  primDoubleAtan :: Double -> Double
foreign import ccall "NotHat.Prelude./"
  primDoubleDiv  :: Double -> Double -> Double
foreign import ccall "NotHat.Prelude.+"
  primDoubleAdd  :: Double -> Double -> Double
foreign import ccall "NotHat.Prelude.-"
  primDoubleSub  :: Double -> Double -> Double
foreign import ccall "NotHat.Prelude.*"
  primDoubleMul  :: Double -> Double -> Double
foreign import ccall "NotHat.Prelude.abs"
  primDoubleAbs    :: Double -> Double
foreign import ccall "NotHat.Prelude.signum"
  primDoubleSignum :: Double -> Double


-- IO

foreign import ccall "NotHat.Prelude.>>="
  primIOBind :: IO a -> (a -> IO b) -> IO b
foreign import ccall "NotHat.Prelude.return"
  primIOReturn :: a -> IO a


foreign import ccall "NotHat.Prelude.show"
  primIOErrorShow :: IOError -> String

{- currently excluded because instance Eq IOError missing in ghc 5.02
foreign import ccall "NotHat.Prelude.=="
  primIOErrorEq :: IOError -> IOError -> Bool
-}


foreign import ccall "NotHat.Prelude.ioError"
  ioError ::  IOError -> IO a 

foreign import ccall "NotHat.Prelude.userError"
  userError ::  String -> IOError

foreign import ccall "NotHat.System.IO.Error.catchIOError"
  catch ::  IO a -> (IOError -> IO a) -> IO a 

-- foreign import ccall "NotHat.Prelude.putChar"
--   putChar :: Char -> IO ()
-- HACK!
foreign import ccall 
  "NotHat.(\\_ c -> T.outputTrace kputChar [c] Prelude.>> Prelude.putChar c) Prelude.True"
  putChar :: Char -> IO ()

foreign import ccall "NotHat.Prelude.getChar"
  getChar          :: IO Char

foreign import ccall "NotHat.Prelude.getContents"
  getContents      :: IO String

foreign import ccall "NotHat.Prelude.readFile"
  readFile         :: String -> IO String

foreign import ccall "NotHat.(\\_ n s -> T.outputTrace kwriteFile s Prelude.>> Prelude.writeFile n s) Prelude.True"
  writeFile        :: String -> String -> IO ()

foreign import ccall "NotHat.(\\_ n s -> T.outputTrace kappendFile s Prelude.>> Prelude.appendFile n s) Prelude.True"
  appendFile       :: String -> String -> IO ()

