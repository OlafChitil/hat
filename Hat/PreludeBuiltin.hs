module Hat.PreludeBuiltin
       (Bool(True, False), aTrue, aFalse, Char, Int,
        Integer, Float, Double, IOError, IO, String, gerror,
        aerror, herror, gundefined, gseq, aseq, hseq,
        gisAscii, aisAscii, hisAscii, gisLatin1, aisLatin1,
        hisLatin1, gisControl, aisControl, hisControl,
        gisPrint, aisPrint, hisPrint, gisSpace, aisSpace,
        hisSpace, gisUpper, aisUpper, hisUpper, gisLower,
        aisLower, hisLower, gisAlpha, aisAlpha, hisAlpha,
        gisDigit, aisDigit, hisDigit, gisOctDigit,
        aisOctDigit, hisOctDigit, gisHexDigit, aisHexDigit,
        hisHexDigit, gisAlphaNum, aisAlphaNum, hisAlphaNum,
        gtoUpper, atoUpper, htoUpper, gtoLower, atoLower,
        htoLower, gprimIntToChar, aprimIntToChar,
        hprimIntToChar, gprimCharToInt, aprimCharToInt,
        hprimCharToInt, gprimUnicodeMaxBound,
        gprimIntMinBound, gprimIntMaxBound, gprimIntEq,
        aprimIntEq, hprimIntEq, gprimIntNe, aprimIntNe,
        hprimIntNe, gprimIntLt, aprimIntLt, hprimIntLt,
        gprimIntLe, aprimIntLe, hprimIntLe, gprimIntGt,
        aprimIntGt, hprimIntGt, gprimIntGe, aprimIntGe,
        hprimIntGe, gprimIntQuot, aprimIntQuot, hprimIntQuot,
        gprimIntRem, aprimIntRem, hprimIntRem, gprimIntPlus,
        aprimIntPlus, hprimIntPlus, gprimIntMinus,
        aprimIntMinus, hprimIntMinus, gprimIntTimes,
        aprimIntTimes, hprimIntTimes, gprimIntNegate,
        aprimIntNegate, hprimIntNegate, gprimIntAbs,
        aprimIntAbs, hprimIntAbs, gprimIntSignum,
        aprimIntSignum, hprimIntSignum, gprimIntegerFromInt,
        aprimIntegerFromInt, hprimIntegerFromInt,
        gprimIntFromInteger, aprimIntFromInteger,
        hprimIntFromInteger, gprimIntegerEq, aprimIntegerEq,
        hprimIntegerEq, gprimIntegerNe, aprimIntegerNe,
        hprimIntegerNe, gprimIntegerLt, aprimIntegerLt,
        hprimIntegerLt, gprimIntegerLe, aprimIntegerLe,
        hprimIntegerLe, gprimIntegerGt, aprimIntegerGt,
        hprimIntegerGt, gprimIntegerGe, aprimIntegerGe,
        hprimIntegerGe, gprimIntegerQuot, aprimIntegerQuot,
        hprimIntegerQuot, gprimIntegerRem, aprimIntegerRem,
        hprimIntegerRem, gprimIntegerQuotRem,
        aprimIntegerQuotRem, hprimIntegerQuotRem,
        gprimIntegerAdd, aprimIntegerAdd, hprimIntegerAdd,
        gprimIntegerSub, aprimIntegerSub, hprimIntegerSub,
        gprimIntegerMul, aprimIntegerMul, hprimIntegerMul,
        gprimIntegerNeg, aprimIntegerNeg, hprimIntegerNeg,
        gprimFloatFromInteger, aprimFloatFromInteger,
        hprimFloatFromInteger, gprimFloatRadix,
        aprimFloatRadix, hprimFloatRadix, gprimFloatDigits,
        aprimFloatDigits, hprimFloatDigits, gprimFloatRange,
        aprimFloatRange, hprimFloatRange, gprimDecodeFloat,
        aprimDecodeFloat, hprimDecodeFloat, gprimEncodeFloat,
        aprimEncodeFloat, hprimEncodeFloat, gprimFloatIsNaN,
        aprimFloatIsNaN, hprimFloatIsNaN,
        gprimFloatIsInfinite, aprimFloatIsInfinite,
        hprimFloatIsInfinite, gprimFloatIsDenormalized,
        aprimFloatIsDenormalized, hprimFloatIsDenormalized,
        gprimFloatIsNegativeZero, aprimFloatIsNegativeZero,
        hprimFloatIsNegativeZero, gprimFloatIsIEEE,
        aprimFloatIsIEEE, hprimFloatIsIEEE, gprimFloatEq,
        aprimFloatEq, hprimFloatEq, gprimFloatNe,
        aprimFloatNe, hprimFloatNe, gprimFloatLt,
        aprimFloatLt, hprimFloatLt, gprimFloatLe,
        aprimFloatLe, hprimFloatLe, gprimFloatGt,
        aprimFloatGt, hprimFloatGt, gprimFloatGe,
        aprimFloatGe, hprimFloatGe, gprimFloatPi,
        gprimFloatExp, aprimFloatExp, hprimFloatExp,
        gprimFloatLog, aprimFloatLog, hprimFloatLog,
        gprimFloatSqrt, aprimFloatSqrt, hprimFloatSqrt,
        gprimFloatSin, aprimFloatSin, hprimFloatSin,
        gprimFloatCos, aprimFloatCos, hprimFloatCos,
        gprimFloatTan, aprimFloatTan, hprimFloatTan,
        gprimFloatAsin, aprimFloatAsin, hprimFloatAsin,
        gprimFloatAcos, aprimFloatAcos, hprimFloatAcos,
        gprimFloatAtan, aprimFloatAtan, hprimFloatAtan,
        gprimFloatDiv, aprimFloatDiv, hprimFloatDiv,
        gprimFloatAdd, aprimFloatAdd, hprimFloatAdd,
        gprimFloatSub, aprimFloatSub, hprimFloatSub,
        gprimFloatMul, aprimFloatMul, hprimFloatMul,
        gprimFloatAbs, aprimFloatAbs, hprimFloatAbs,
        gprimFloatSignum, aprimFloatSignum, hprimFloatSignum,
        gprimDoubleFromInteger, aprimDoubleFromInteger,
        hprimDoubleFromInteger, gprimDoubleRadix,
        aprimDoubleRadix, hprimDoubleRadix,
        gprimDoubleDigits, aprimDoubleDigits,
        hprimDoubleDigits, gprimDoubleRange,
        aprimDoubleRange, hprimDoubleRange,
        gprimDecodeDouble, aprimDecodeDouble,
        hprimDecodeDouble, gprimEncodeDouble,
        aprimEncodeDouble, hprimEncodeDouble,
        gprimDoubleIsNaN, aprimDoubleIsNaN, hprimDoubleIsNaN,
        gprimDoubleIsInfinite, aprimDoubleIsInfinite,
        hprimDoubleIsInfinite, gprimDoubleIsDenormalized,
        aprimDoubleIsDenormalized, hprimDoubleIsDenormalized,
        gprimDoubleIsNegativeZero, aprimDoubleIsNegativeZero,
        hprimDoubleIsNegativeZero, gprimDoubleIsIEEE,
        aprimDoubleIsIEEE, hprimDoubleIsIEEE, gprimDoubleEq,
        aprimDoubleEq, hprimDoubleEq, gprimDoubleNe,
        aprimDoubleNe, hprimDoubleNe, gprimDoubleLt,
        aprimDoubleLt, hprimDoubleLt, gprimDoubleLe,
        aprimDoubleLe, hprimDoubleLe, gprimDoubleGt,
        aprimDoubleGt, hprimDoubleGt, gprimDoubleGe,
        aprimDoubleGe, hprimDoubleGe, gprimDoublePi,
        gprimDoubleExp, aprimDoubleExp, hprimDoubleExp,
        gprimDoubleLog, aprimDoubleLog, hprimDoubleLog,
        gprimDoubleSqrt, aprimDoubleSqrt, hprimDoubleSqrt,
        gprimDoubleSin, aprimDoubleSin, hprimDoubleSin,
        gprimDoubleCos, aprimDoubleCos, hprimDoubleCos,
        gprimDoubleTan, aprimDoubleTan, hprimDoubleTan,
        gprimDoubleAsin, aprimDoubleAsin, hprimDoubleAsin,
        gprimDoubleAcos, aprimDoubleAcos, hprimDoubleAcos,
        gprimDoubleAtan, aprimDoubleAtan, hprimDoubleAtan,
        gprimDoubleDiv, aprimDoubleDiv, hprimDoubleDiv,
        gprimDoubleAdd, aprimDoubleAdd, hprimDoubleAdd,
        gprimDoubleSub, aprimDoubleSub, hprimDoubleSub,
        gprimDoubleMul, aprimDoubleMul, hprimDoubleMul,
        gprimDoubleAbs, aprimDoubleAbs, hprimDoubleAbs,
        gprimDoubleSignum, aprimDoubleSignum,
        hprimDoubleSignum, gprimIOBind, aprimIOBind,
        hprimIOBind, gprimIOReturn, aprimIOReturn,
        hprimIOReturn, gioError, aioError, hioError,
        guserError, auserError, huserError, gcatch, acatch,
        hcatch, gputChar, aputChar, hputChar, ggetChar,
        ggetContents, greadFile, areadFile, hreadFile,
        gwriteFile, awriteFile, hwriteFile, gappendFile,
        aappendFile, happendFile, gprimIOErrorShow,
        aprimIOErrorShow, hprimIOErrorShow)
       where
import qualified Prelude
import qualified Hat.Hat as T
import Hat.PreludeBuiltinTypes as T
import Hat.Hat
import qualified Prelude
import qualified System.IO.Error
import qualified Data.Char as Char
 
gseq ::
     T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (T.Fun b b))
gseq pseq p = T.ufun2 aseq pseq p hseq
hseq z1seq z2seq kseq
  = T.fromId kseq
      (Prelude.seq (T.toId kseq z1seq) (T.toId kseq z2seq))
 
gisAscii ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisAscii pisAscii p
  = T.ufun1 aisAscii pisAscii p hisAscii
hisAscii z1isAscii kisAscii
  = T.fromBool kisAscii
      (Char.isAscii (T.toChar kisAscii z1isAscii))
 
gisLatin1 ::
          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisLatin1 pisLatin1 p
  = T.ufun1 aisLatin1 pisLatin1 p hisLatin1
hisLatin1 z1isLatin1 kisLatin1
  = T.fromBool kisLatin1
      (Char.isLatin1 (T.toChar kisLatin1 z1isLatin1))
 
gisControl ::
           T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisControl pisControl p
  = T.ufun1 aisControl pisControl p hisControl
hisControl z1isControl kisControl
  = T.fromBool kisControl
      (Char.isControl (T.toChar kisControl z1isControl))
 
gisPrint ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisPrint pisPrint p
  = T.ufun1 aisPrint pisPrint p hisPrint
hisPrint z1isPrint kisPrint
  = T.fromBool kisPrint
      (Char.isPrint (T.toChar kisPrint z1isPrint))
 
gisSpace ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisSpace pisSpace p
  = T.ufun1 aisSpace pisSpace p hisSpace
hisSpace z1isSpace kisSpace
  = T.fromBool kisSpace
      (Char.isSpace (T.toChar kisSpace z1isSpace))
 
gisUpper ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisUpper pisUpper p
  = T.ufun1 aisUpper pisUpper p hisUpper
hisUpper z1isUpper kisUpper
  = T.fromBool kisUpper
      (Char.isUpper (T.toChar kisUpper z1isUpper))
 
gisLower ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisLower pisLower p
  = T.ufun1 aisLower pisLower p hisLower
hisLower z1isLower kisLower
  = T.fromBool kisLower
      (Char.isLower (T.toChar kisLower z1isLower))
 
gisAlpha ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisAlpha pisAlpha p
  = T.ufun1 aisAlpha pisAlpha p hisAlpha
hisAlpha z1isAlpha kisAlpha
  = T.fromBool kisAlpha
      (Char.isAlpha (T.toChar kisAlpha z1isAlpha))
 
gisDigit ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisDigit pisDigit p
  = T.ufun1 aisDigit pisDigit p hisDigit
hisDigit z1isDigit kisDigit
  = T.fromBool kisDigit
      (Char.isDigit (T.toChar kisDigit z1isDigit))
 
gisOctDigit ::
            T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisOctDigit pisOctDigit p
  = T.ufun1 aisOctDigit pisOctDigit p hisOctDigit
hisOctDigit z1isOctDigit kisOctDigit
  = T.fromBool kisOctDigit
      (Char.isOctDigit (T.toChar kisOctDigit z1isOctDigit))
 
gisHexDigit ::
            T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisHexDigit pisHexDigit p
  = T.ufun1 aisHexDigit pisHexDigit p hisHexDigit
hisHexDigit z1isHexDigit kisHexDigit
  = T.fromBool kisHexDigit
      (Char.isHexDigit (T.toChar kisHexDigit z1isHexDigit))
 
gisAlphaNum ::
            T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisAlphaNum pisAlphaNum p
  = T.ufun1 aisAlphaNum pisAlphaNum p hisAlphaNum
hisAlphaNum z1isAlphaNum kisAlphaNum
  = T.fromBool kisAlphaNum
      (Char.isAlphaNum (T.toChar kisAlphaNum z1isAlphaNum))
 
gtoUpper ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Char)
gtoUpper ptoUpper p
  = T.ufun1 atoUpper ptoUpper p htoUpper
htoUpper z1toUpper ktoUpper
  = T.fromChar ktoUpper
      (Char.toUpper (T.toChar ktoUpper z1toUpper))
 
gtoLower ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Char)
gtoLower ptoLower p
  = T.ufun1 atoLower ptoLower p htoLower
htoLower z1toLower ktoLower
  = T.fromChar ktoLower
      (Char.toLower (T.toChar ktoLower z1toLower))
 
gprimCharToInt ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Int)
gprimCharToInt pprimCharToInt p
  = T.ufun1 aprimCharToInt pprimCharToInt p
      hprimCharToInt
hprimCharToInt z1primCharToInt kprimCharToInt
  = T.fromInt kprimCharToInt
      (Char.ord (T.toChar kprimCharToInt z1primCharToInt))
 
gprimIntToChar ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Char)
gprimIntToChar pprimIntToChar p
  = T.ufun1 aprimIntToChar pprimIntToChar p
      hprimIntToChar
hprimIntToChar z1primIntToChar kprimIntToChar
  = T.fromChar kprimIntToChar
      (Char.chr (T.toInt kprimIntToChar z1primIntToChar))
 
gprimUnicodeMaxBound ::
                     T.RefSrcPos -> T.RefExp -> T.R Char
gprimUnicodeMaxBound pprimUnicodeMaxBound p
  = T.uconstUse pprimUnicodeMaxBound p
      sprimUnicodeMaxBound
sprimUnicodeMaxBound
  = T.uconstDef p aprimUnicodeMaxBound
      (\ p -> T.fromChar p Prelude.maxBound)
 
gprimIntMinBound ::
                 T.RefSrcPos -> T.RefExp -> T.R Int
gprimIntMinBound pprimIntMinBound p
  = T.uconstUse pprimIntMinBound p sprimIntMinBound
sprimIntMinBound
  = T.uconstDef p aprimIntMinBound
      (\ p -> T.fromInt p Prelude.minBound)
 
gprimIntMaxBound ::
                 T.RefSrcPos -> T.RefExp -> T.R Int
gprimIntMaxBound pprimIntMaxBound p
  = T.uconstUse pprimIntMaxBound p sprimIntMaxBound
sprimIntMaxBound
  = T.uconstDef p aprimIntMaxBound
      (\ p -> T.fromInt p Prelude.maxBound)
 
gprimIntEq ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntEq pprimIntEq p
  = T.ufun2 aprimIntEq pprimIntEq p hprimIntEq
hprimIntEq z1primIntEq z2primIntEq kprimIntEq
  = T.fromBool kprimIntEq
      ((Prelude.==) (T.toInt kprimIntEq z1primIntEq)
         (T.toInt kprimIntEq z2primIntEq))
 
gprimIntNe ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntNe pprimIntNe p
  = T.ufun2 aprimIntNe pprimIntNe p hprimIntNe
hprimIntNe z1primIntNe z2primIntNe kprimIntNe
  = T.fromBool kprimIntNe
      ((Prelude./=) (T.toInt kprimIntNe z1primIntNe)
         (T.toInt kprimIntNe z2primIntNe))
 
gprimIntLt ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntLt pprimIntLt p
  = T.ufun2 aprimIntLt pprimIntLt p hprimIntLt
hprimIntLt z1primIntLt z2primIntLt kprimIntLt
  = T.fromBool kprimIntLt
      ((Prelude.<) (T.toInt kprimIntLt z1primIntLt)
         (T.toInt kprimIntLt z2primIntLt))
 
gprimIntLe ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntLe pprimIntLe p
  = T.ufun2 aprimIntLe pprimIntLe p hprimIntLe
hprimIntLe z1primIntLe z2primIntLe kprimIntLe
  = T.fromBool kprimIntLe
      ((Prelude.<=) (T.toInt kprimIntLe z1primIntLe)
         (T.toInt kprimIntLe z2primIntLe))
 
gprimIntGt ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntGt pprimIntGt p
  = T.ufun2 aprimIntGt pprimIntGt p hprimIntGt
hprimIntGt z1primIntGt z2primIntGt kprimIntGt
  = T.fromBool kprimIntGt
      ((Prelude.>) (T.toInt kprimIntGt z1primIntGt)
         (T.toInt kprimIntGt z2primIntGt))
 
gprimIntGe ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun Int (T.Fun Int Bool))
gprimIntGe pprimIntGe p
  = T.ufun2 aprimIntGe pprimIntGe p hprimIntGe
hprimIntGe z1primIntGe z2primIntGe kprimIntGe
  = T.fromBool kprimIntGe
      ((Prelude.>=) (T.toInt kprimIntGe z1primIntGe)
         (T.toInt kprimIntGe z2primIntGe))
 
gprimIntQuot ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))
gprimIntQuot pprimIntQuot p
  = T.ufun2 aprimIntQuot pprimIntQuot p hprimIntQuot
hprimIntQuot z1primIntQuot z2primIntQuot kprimIntQuot
  = T.fromInt kprimIntQuot
      (Prelude.quot (T.toInt kprimIntQuot z1primIntQuot)
         (T.toInt kprimIntQuot z2primIntQuot))
 
gprimIntRem ::
            T.RefSrcPos ->
              T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))
gprimIntRem pprimIntRem p
  = T.ufun2 aprimIntRem pprimIntRem p hprimIntRem
hprimIntRem z1primIntRem z2primIntRem kprimIntRem
  = T.fromInt kprimIntRem
      (Prelude.rem (T.toInt kprimIntRem z1primIntRem)
         (T.toInt kprimIntRem z2primIntRem))
 
gprimIntPlus ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))
gprimIntPlus pprimIntPlus p
  = T.ufun2 aprimIntPlus pprimIntPlus p hprimIntPlus
hprimIntPlus z1primIntPlus z2primIntPlus kprimIntPlus
  = T.fromInt kprimIntPlus
      ((Prelude.+) (T.toInt kprimIntPlus z1primIntPlus)
         (T.toInt kprimIntPlus z2primIntPlus))
 
gprimIntMinus ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))
gprimIntMinus pprimIntMinus p
  = T.ufun2 aprimIntMinus pprimIntMinus p hprimIntMinus
hprimIntMinus z1primIntMinus z2primIntMinus
  kprimIntMinus
  = T.fromInt kprimIntMinus
      ((Prelude.-) (T.toInt kprimIntMinus z1primIntMinus)
         (T.toInt kprimIntMinus z2primIntMinus))
 
gprimIntTimes ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))
gprimIntTimes pprimIntTimes p
  = T.ufun2 aprimIntTimes pprimIntTimes p hprimIntTimes
hprimIntTimes z1primIntTimes z2primIntTimes
  kprimIntTimes
  = T.fromInt kprimIntTimes
      ((Prelude.*) (T.toInt kprimIntTimes z1primIntTimes)
         (T.toInt kprimIntTimes z2primIntTimes))
 
gprimIntNegate ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)
gprimIntNegate pprimIntNegate p
  = T.ufun1 aprimIntNegate pprimIntNegate p
      hprimIntNegate
hprimIntNegate z1primIntNegate kprimIntNegate
  = T.fromInt kprimIntNegate
      (Prelude.negate
         (T.toInt kprimIntNegate z1primIntNegate))
 
gprimIntAbs ::
            T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)
gprimIntAbs pprimIntAbs p
  = T.ufun1 aprimIntAbs pprimIntAbs p hprimIntAbs
hprimIntAbs z1primIntAbs kprimIntAbs
  = T.fromInt kprimIntAbs
      (Prelude.abs (T.toInt kprimIntAbs z1primIntAbs))
 
gprimIntSignum ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)
gprimIntSignum pprimIntSignum p
  = T.ufun1 aprimIntSignum pprimIntSignum p
      hprimIntSignum
hprimIntSignum z1primIntSignum kprimIntSignum
  = T.fromInt kprimIntSignum
      (Prelude.signum
         (T.toInt kprimIntSignum z1primIntSignum))
 
gprimIntegerFromInt ::
                    T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Integer)
gprimIntegerFromInt pprimIntegerFromInt p
  = T.ufun1 aprimIntegerFromInt pprimIntegerFromInt p
      hprimIntegerFromInt
hprimIntegerFromInt z1primIntegerFromInt
  kprimIntegerFromInt
  = T.fromInteger kprimIntegerFromInt
      (Prelude.toInteger
         (T.toInt kprimIntegerFromInt z1primIntegerFromInt))
 
gprimIntFromInteger ::
                    T.RefSrcPos -> T.RefExp -> T.R (T.Fun Integer Int)
gprimIntFromInteger pprimIntFromInteger p
  = T.ufun1 aprimIntFromInteger pprimIntFromInteger p
      hprimIntFromInteger
hprimIntFromInteger z1primIntFromInteger
  kprimIntFromInteger
  = T.fromInt kprimIntFromInteger
      (Prelude.fromInteger
         (T.toInteger kprimIntFromInteger
            z1primIntFromInteger))
 
gprimIntegerEq ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerEq pprimIntegerEq p
  = T.ufun2 aprimIntegerEq pprimIntegerEq p
      hprimIntegerEq
hprimIntegerEq z1primIntegerEq z2primIntegerEq
  kprimIntegerEq
  = T.fromBool kprimIntegerEq
      ((Prelude.==)
         (T.toInteger kprimIntegerEq z1primIntegerEq)
         (T.toInteger kprimIntegerEq z2primIntegerEq))
 
gprimIntegerNe ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerNe pprimIntegerNe p
  = T.ufun2 aprimIntegerNe pprimIntegerNe p
      hprimIntegerNe
hprimIntegerNe z1primIntegerNe z2primIntegerNe
  kprimIntegerNe
  = T.fromBool kprimIntegerNe
      ((Prelude./=)
         (T.toInteger kprimIntegerNe z1primIntegerNe)
         (T.toInteger kprimIntegerNe z2primIntegerNe))
 
gprimIntegerLt ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerLt pprimIntegerLt p
  = T.ufun2 aprimIntegerLt pprimIntegerLt p
      hprimIntegerLt
hprimIntegerLt z1primIntegerLt z2primIntegerLt
  kprimIntegerLt
  = T.fromBool kprimIntegerLt
      ((Prelude.<)
         (T.toInteger kprimIntegerLt z1primIntegerLt)
         (T.toInteger kprimIntegerLt z2primIntegerLt))
 
gprimIntegerLe ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerLe pprimIntegerLe p
  = T.ufun2 aprimIntegerLe pprimIntegerLe p
      hprimIntegerLe
hprimIntegerLe z1primIntegerLe z2primIntegerLe
  kprimIntegerLe
  = T.fromBool kprimIntegerLe
      ((Prelude.<=)
         (T.toInteger kprimIntegerLe z1primIntegerLe)
         (T.toInteger kprimIntegerLe z2primIntegerLe))
 
gprimIntegerGt ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerGt pprimIntegerGt p
  = T.ufun2 aprimIntegerGt pprimIntegerGt p
      hprimIntegerGt
hprimIntegerGt z1primIntegerGt z2primIntegerGt
  kprimIntegerGt
  = T.fromBool kprimIntegerGt
      ((Prelude.>)
         (T.toInteger kprimIntegerGt z1primIntegerGt)
         (T.toInteger kprimIntegerGt z2primIntegerGt))
 
gprimIntegerGe ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Bool))
gprimIntegerGe pprimIntegerGe p
  = T.ufun2 aprimIntegerGe pprimIntegerGe p
      hprimIntegerGe
hprimIntegerGe z1primIntegerGe z2primIntegerGe
  kprimIntegerGe
  = T.fromBool kprimIntegerGe
      ((Prelude.>=)
         (T.toInteger kprimIntegerGe z1primIntegerGe)
         (T.toInteger kprimIntegerGe z2primIntegerGe))
 
gprimIntegerQuot ::
                 T.RefSrcPos ->
                   T.RefExp ->
                     T.R (T.Fun Integer (T.Fun Integer Integer))
gprimIntegerQuot pprimIntegerQuot p
  = T.ufun2 aprimIntegerQuot pprimIntegerQuot p
      hprimIntegerQuot
hprimIntegerQuot z1primIntegerQuot z2primIntegerQuot
  kprimIntegerQuot
  = T.fromInteger kprimIntegerQuot
      (Prelude.quot
         (T.toInteger kprimIntegerQuot z1primIntegerQuot)
         (T.toInteger kprimIntegerQuot z2primIntegerQuot))
 
gprimIntegerRem ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R (T.Fun Integer (T.Fun Integer Integer))
gprimIntegerRem pprimIntegerRem p
  = T.ufun2 aprimIntegerRem pprimIntegerRem p
      hprimIntegerRem
hprimIntegerRem z1primIntegerRem z2primIntegerRem
  kprimIntegerRem
  = T.fromInteger kprimIntegerRem
      (Prelude.rem
         (T.toInteger kprimIntegerRem z1primIntegerRem)
         (T.toInteger kprimIntegerRem z2primIntegerRem))
 
gprimIntegerQuotRem ::
                    T.RefSrcPos ->
                      T.RefExp ->
                        T.R
                          (T.Fun Integer
                             (T.Fun Integer (T.Tuple2 Integer Integer)))
gprimIntegerQuotRem pprimIntegerQuotRem p
  = T.ufun2 aprimIntegerQuotRem pprimIntegerQuotRem p
      hprimIntegerQuotRem
hprimIntegerQuotRem z1primIntegerQuotRem
  z2primIntegerQuotRem kprimIntegerQuotRem
  = T.fromTuple2 T.fromInteger T.fromInteger
      kprimIntegerQuotRem
      (Prelude.quotRem
         (T.toInteger kprimIntegerQuotRem
            z1primIntegerQuotRem)
         (T.toInteger kprimIntegerQuotRem
            z2primIntegerQuotRem))
 
gprimIntegerAdd ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R (T.Fun Integer (T.Fun Integer Integer))
gprimIntegerAdd pprimIntegerAdd p
  = T.ufun2 aprimIntegerAdd pprimIntegerAdd p
      hprimIntegerAdd
hprimIntegerAdd z1primIntegerAdd z2primIntegerAdd
  kprimIntegerAdd
  = T.fromInteger kprimIntegerAdd
      ((Prelude.+)
         (T.toInteger kprimIntegerAdd z1primIntegerAdd)
         (T.toInteger kprimIntegerAdd z2primIntegerAdd))
 
gprimIntegerSub ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R (T.Fun Integer (T.Fun Integer Integer))
gprimIntegerSub pprimIntegerSub p
  = T.ufun2 aprimIntegerSub pprimIntegerSub p
      hprimIntegerSub
hprimIntegerSub z1primIntegerSub z2primIntegerSub
  kprimIntegerSub
  = T.fromInteger kprimIntegerSub
      ((Prelude.-)
         (T.toInteger kprimIntegerSub z1primIntegerSub)
         (T.toInteger kprimIntegerSub z2primIntegerSub))
 
gprimIntegerMul ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R (T.Fun Integer (T.Fun Integer Integer))
gprimIntegerMul pprimIntegerMul p
  = T.ufun2 aprimIntegerMul pprimIntegerMul p
      hprimIntegerMul
hprimIntegerMul z1primIntegerMul z2primIntegerMul
  kprimIntegerMul
  = T.fromInteger kprimIntegerMul
      ((Prelude.*)
         (T.toInteger kprimIntegerMul z1primIntegerMul)
         (T.toInteger kprimIntegerMul z2primIntegerMul))
 
gprimIntegerNeg ::
                T.RefSrcPos ->
                  T.RefExp -> T.R (T.Fun Integer Integer)
gprimIntegerNeg pprimIntegerNeg p
  = T.ufun1 aprimIntegerNeg pprimIntegerNeg p
      hprimIntegerNeg
hprimIntegerNeg z1primIntegerNeg kprimIntegerNeg
  = T.fromInteger kprimIntegerNeg
      (Prelude.negate
         (T.toInteger kprimIntegerNeg z1primIntegerNeg))
 
gprimFloatFromInteger ::
                      T.RefSrcPos -> T.RefExp -> T.R (T.Fun Integer Float)
gprimFloatFromInteger pprimFloatFromInteger p
  = T.ufun1 aprimFloatFromInteger pprimFloatFromInteger
      p
      hprimFloatFromInteger
hprimFloatFromInteger z1primFloatFromInteger
  kprimFloatFromInteger
  = T.fromFloat kprimFloatFromInteger
      (Prelude.fromInteger
         (T.toInteger kprimFloatFromInteger
            z1primFloatFromInteger))
 
gprimFloatRadix ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Integer)
gprimFloatRadix pprimFloatRadix p
  = T.ufun1 aprimFloatRadix pprimFloatRadix p
      hprimFloatRadix
hprimFloatRadix z1primFloatRadix kprimFloatRadix
  = T.fromInteger kprimFloatRadix
      (Prelude.floatRadix
         (T.toFloat kprimFloatRadix z1primFloatRadix))
 
gprimFloatDigits ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Int)
gprimFloatDigits pprimFloatDigits p
  = T.ufun1 aprimFloatDigits pprimFloatDigits p
      hprimFloatDigits
hprimFloatDigits z1primFloatDigits kprimFloatDigits
  = T.fromInt kprimFloatDigits
      (Prelude.floatDigits
         (T.toFloat kprimFloatDigits z1primFloatDigits))
 
gprimFloatRange ::
                T.RefSrcPos ->
                  T.RefExp -> T.R (T.Fun Float (T.Tuple2 Int Int))
gprimFloatRange pprimFloatRange p
  = T.ufun1 aprimFloatRange pprimFloatRange p
      hprimFloatRange
hprimFloatRange z1primFloatRange kprimFloatRange
  = T.fromTuple2 T.fromInt T.fromInt kprimFloatRange
      (Prelude.floatRange
         (T.toFloat kprimFloatRange z1primFloatRange))
 
gprimDecodeFloat ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun Float (T.Tuple2 Integer Int))
gprimDecodeFloat pprimDecodeFloat p
  = T.ufun1 aprimDecodeFloat pprimDecodeFloat p
      hprimDecodeFloat
hprimDecodeFloat z1primDecodeFloat kprimDecodeFloat
  = T.fromTuple2 T.fromInteger T.fromInt
      kprimDecodeFloat
      (Prelude.decodeFloat
         (T.toFloat kprimDecodeFloat z1primDecodeFloat))
 
gprimEncodeFloat ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun Integer (T.Fun Int Float))
gprimEncodeFloat pprimEncodeFloat p
  = T.ufun2 aprimEncodeFloat pprimEncodeFloat p
      hprimEncodeFloat
hprimEncodeFloat z1primEncodeFloat z2primEncodeFloat
  kprimEncodeFloat
  = T.fromFloat kprimEncodeFloat
      (Prelude.encodeFloat
         (T.toInteger kprimEncodeFloat z1primEncodeFloat)
         (T.toInt kprimEncodeFloat z2primEncodeFloat))
 
gprimFloatIsNaN ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Bool)
gprimFloatIsNaN pprimFloatIsNaN p
  = T.ufun1 aprimFloatIsNaN pprimFloatIsNaN p
      hprimFloatIsNaN
hprimFloatIsNaN z1primFloatIsNaN kprimFloatIsNaN
  = T.fromBool kprimFloatIsNaN
      (Prelude.isNaN
         (T.toFloat kprimFloatIsNaN z1primFloatIsNaN))
 
gprimFloatIsInfinite ::
                     T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Bool)
gprimFloatIsInfinite pprimFloatIsInfinite p
  = T.ufun1 aprimFloatIsInfinite pprimFloatIsInfinite p
      hprimFloatIsInfinite
hprimFloatIsInfinite z1primFloatIsInfinite
  kprimFloatIsInfinite
  = T.fromBool kprimFloatIsInfinite
      (Prelude.isInfinite
         (T.toFloat kprimFloatIsInfinite
            z1primFloatIsInfinite))
 
gprimFloatIsDenormalized ::
                         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Bool)
gprimFloatIsDenormalized pprimFloatIsDenormalized p
  = T.ufun1 aprimFloatIsDenormalized
      pprimFloatIsDenormalized
      p
      hprimFloatIsDenormalized
hprimFloatIsDenormalized z1primFloatIsDenormalized
  kprimFloatIsDenormalized
  = T.fromBool kprimFloatIsDenormalized
      (Prelude.isDenormalized
         (T.toFloat kprimFloatIsDenormalized
            z1primFloatIsDenormalized))
 
gprimFloatIsNegativeZero ::
                         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Bool)
gprimFloatIsNegativeZero pprimFloatIsNegativeZero p
  = T.ufun1 aprimFloatIsNegativeZero
      pprimFloatIsNegativeZero
      p
      hprimFloatIsNegativeZero
hprimFloatIsNegativeZero z1primFloatIsNegativeZero
  kprimFloatIsNegativeZero
  = T.fromBool kprimFloatIsNegativeZero
      (Prelude.isNegativeZero
         (T.toFloat kprimFloatIsNegativeZero
            z1primFloatIsNegativeZero))
 
gprimFloatIsIEEE ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Bool)
gprimFloatIsIEEE pprimFloatIsIEEE p
  = T.ufun1 aprimFloatIsIEEE pprimFloatIsIEEE p
      hprimFloatIsIEEE
hprimFloatIsIEEE z1primFloatIsIEEE kprimFloatIsIEEE
  = T.fromBool kprimFloatIsIEEE
      (Prelude.isIEEE
         (T.toFloat kprimFloatIsIEEE z1primFloatIsIEEE))
 
gprimFloatEq ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatEq pprimFloatEq p
  = T.ufun2 aprimFloatEq pprimFloatEq p hprimFloatEq
hprimFloatEq z1primFloatEq z2primFloatEq kprimFloatEq
  = T.fromBool kprimFloatEq
      ((Prelude.==) (T.toFloat kprimFloatEq z1primFloatEq)
         (T.toFloat kprimFloatEq z2primFloatEq))
 
gprimFloatNe ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatNe pprimFloatNe p
  = T.ufun2 aprimFloatNe pprimFloatNe p hprimFloatNe
hprimFloatNe z1primFloatNe z2primFloatNe kprimFloatNe
  = T.fromBool kprimFloatNe
      ((Prelude./=) (T.toFloat kprimFloatNe z1primFloatNe)
         (T.toFloat kprimFloatNe z2primFloatNe))
 
gprimFloatLt ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatLt pprimFloatLt p
  = T.ufun2 aprimFloatLt pprimFloatLt p hprimFloatLt
hprimFloatLt z1primFloatLt z2primFloatLt kprimFloatLt
  = T.fromBool kprimFloatLt
      ((Prelude.<) (T.toFloat kprimFloatLt z1primFloatLt)
         (T.toFloat kprimFloatLt z2primFloatLt))
 
gprimFloatLe ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatLe pprimFloatLe p
  = T.ufun2 aprimFloatLe pprimFloatLe p hprimFloatLe
hprimFloatLe z1primFloatLe z2primFloatLe kprimFloatLe
  = T.fromBool kprimFloatLe
      ((Prelude.<=) (T.toFloat kprimFloatLe z1primFloatLe)
         (T.toFloat kprimFloatLe z2primFloatLe))
 
gprimFloatGt ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatGt pprimFloatGt p
  = T.ufun2 aprimFloatGt pprimFloatGt p hprimFloatGt
hprimFloatGt z1primFloatGt z2primFloatGt kprimFloatGt
  = T.fromBool kprimFloatGt
      ((Prelude.>) (T.toFloat kprimFloatGt z1primFloatGt)
         (T.toFloat kprimFloatGt z2primFloatGt))
 
gprimFloatGe ::
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun Float (T.Fun Float Bool))
gprimFloatGe pprimFloatGe p
  = T.ufun2 aprimFloatGe pprimFloatGe p hprimFloatGe
hprimFloatGe z1primFloatGe z2primFloatGe kprimFloatGe
  = T.fromBool kprimFloatGe
      ((Prelude.>=) (T.toFloat kprimFloatGe z1primFloatGe)
         (T.toFloat kprimFloatGe z2primFloatGe))
 
gprimFloatPi :: T.RefSrcPos -> T.RefExp -> T.R Float
gprimFloatPi pprimFloatPi p
  = T.uconstUse pprimFloatPi p sprimFloatPi
sprimFloatPi
  = T.uconstDef p aprimFloatPi
      (\ p -> T.fromFloat p Prelude.pi)
 
gprimFloatExp ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatExp pprimFloatExp p
  = T.ufun1 aprimFloatExp pprimFloatExp p hprimFloatExp
hprimFloatExp z1primFloatExp kprimFloatExp
  = T.fromFloat kprimFloatExp
      (Prelude.exp
         (T.toFloat kprimFloatExp z1primFloatExp))
 
gprimFloatLog ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatLog pprimFloatLog p
  = T.ufun1 aprimFloatLog pprimFloatLog p hprimFloatLog
hprimFloatLog z1primFloatLog kprimFloatLog
  = T.fromFloat kprimFloatLog
      (Prelude.log
         (T.toFloat kprimFloatLog z1primFloatLog))
 
gprimFloatSqrt ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatSqrt pprimFloatSqrt p
  = T.ufun1 aprimFloatSqrt pprimFloatSqrt p
      hprimFloatSqrt
hprimFloatSqrt z1primFloatSqrt kprimFloatSqrt
  = T.fromFloat kprimFloatSqrt
      (Prelude.sqrt
         (T.toFloat kprimFloatSqrt z1primFloatSqrt))
 
gprimFloatSin ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatSin pprimFloatSin p
  = T.ufun1 aprimFloatSin pprimFloatSin p hprimFloatSin
hprimFloatSin z1primFloatSin kprimFloatSin
  = T.fromFloat kprimFloatSin
      (Prelude.sin
         (T.toFloat kprimFloatSin z1primFloatSin))
 
gprimFloatCos ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatCos pprimFloatCos p
  = T.ufun1 aprimFloatCos pprimFloatCos p hprimFloatCos
hprimFloatCos z1primFloatCos kprimFloatCos
  = T.fromFloat kprimFloatCos
      (Prelude.cos
         (T.toFloat kprimFloatCos z1primFloatCos))
 
gprimFloatTan ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatTan pprimFloatTan p
  = T.ufun1 aprimFloatTan pprimFloatTan p hprimFloatTan
hprimFloatTan z1primFloatTan kprimFloatTan
  = T.fromFloat kprimFloatTan
      (Prelude.tan
         (T.toFloat kprimFloatTan z1primFloatTan))
 
gprimFloatAsin ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatAsin pprimFloatAsin p
  = T.ufun1 aprimFloatAsin pprimFloatAsin p
      hprimFloatAsin
hprimFloatAsin z1primFloatAsin kprimFloatAsin
  = T.fromFloat kprimFloatAsin
      (Prelude.asin
         (T.toFloat kprimFloatAsin z1primFloatAsin))
 
gprimFloatAcos ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatAcos pprimFloatAcos p
  = T.ufun1 aprimFloatAcos pprimFloatAcos p
      hprimFloatAcos
hprimFloatAcos z1primFloatAcos kprimFloatAcos
  = T.fromFloat kprimFloatAcos
      (Prelude.acos
         (T.toFloat kprimFloatAcos z1primFloatAcos))
 
gprimFloatAtan ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatAtan pprimFloatAtan p
  = T.ufun1 aprimFloatAtan pprimFloatAtan p
      hprimFloatAtan
hprimFloatAtan z1primFloatAtan kprimFloatAtan
  = T.fromFloat kprimFloatAtan
      (Prelude.atan
         (T.toFloat kprimFloatAtan z1primFloatAtan))
 
gprimFloatDiv ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Float (T.Fun Float Float))
gprimFloatDiv pprimFloatDiv p
  = T.ufun2 aprimFloatDiv pprimFloatDiv p hprimFloatDiv
hprimFloatDiv z1primFloatDiv z2primFloatDiv
  kprimFloatDiv
  = T.fromFloat kprimFloatDiv
      ((Prelude./) (T.toFloat kprimFloatDiv z1primFloatDiv)
         (T.toFloat kprimFloatDiv z2primFloatDiv))
 
gprimFloatAdd ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Float (T.Fun Float Float))
gprimFloatAdd pprimFloatAdd p
  = T.ufun2 aprimFloatAdd pprimFloatAdd p hprimFloatAdd
hprimFloatAdd z1primFloatAdd z2primFloatAdd
  kprimFloatAdd
  = T.fromFloat kprimFloatAdd
      ((Prelude.+) (T.toFloat kprimFloatAdd z1primFloatAdd)
         (T.toFloat kprimFloatAdd z2primFloatAdd))
 
gprimFloatSub ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Float (T.Fun Float Float))
gprimFloatSub pprimFloatSub p
  = T.ufun2 aprimFloatSub pprimFloatSub p hprimFloatSub
hprimFloatSub z1primFloatSub z2primFloatSub
  kprimFloatSub
  = T.fromFloat kprimFloatSub
      ((Prelude.-) (T.toFloat kprimFloatSub z1primFloatSub)
         (T.toFloat kprimFloatSub z2primFloatSub))
 
gprimFloatMul ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Float (T.Fun Float Float))
gprimFloatMul pprimFloatMul p
  = T.ufun2 aprimFloatMul pprimFloatMul p hprimFloatMul
hprimFloatMul z1primFloatMul z2primFloatMul
  kprimFloatMul
  = T.fromFloat kprimFloatMul
      ((Prelude.*) (T.toFloat kprimFloatMul z1primFloatMul)
         (T.toFloat kprimFloatMul z2primFloatMul))
 
gprimFloatAbs ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatAbs pprimFloatAbs p
  = T.ufun1 aprimFloatAbs pprimFloatAbs p hprimFloatAbs
hprimFloatAbs z1primFloatAbs kprimFloatAbs
  = T.fromFloat kprimFloatAbs
      (Prelude.abs
         (T.toFloat kprimFloatAbs z1primFloatAbs))
 
gprimFloatSignum ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Float Float)
gprimFloatSignum pprimFloatSignum p
  = T.ufun1 aprimFloatSignum pprimFloatSignum p
      hprimFloatSignum
hprimFloatSignum z1primFloatSignum kprimFloatSignum
  = T.fromFloat kprimFloatSignum
      (Prelude.signum
         (T.toFloat kprimFloatSignum z1primFloatSignum))
 
gprimDoubleFromInteger ::
                       T.RefSrcPos -> T.RefExp -> T.R (T.Fun Integer Double)
gprimDoubleFromInteger pprimDoubleFromInteger p
  = T.ufun1 aprimDoubleFromInteger
      pprimDoubleFromInteger
      p
      hprimDoubleFromInteger
hprimDoubleFromInteger z1primDoubleFromInteger
  kprimDoubleFromInteger
  = T.fromDouble kprimDoubleFromInteger
      (Prelude.fromInteger
         (T.toInteger kprimDoubleFromInteger
            z1primDoubleFromInteger))
 
gprimDoubleRadix ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Integer)
gprimDoubleRadix pprimDoubleRadix p
  = T.ufun1 aprimDoubleRadix pprimDoubleRadix p
      hprimDoubleRadix
hprimDoubleRadix z1primDoubleRadix kprimDoubleRadix
  = T.fromInteger kprimDoubleRadix
      (Prelude.floatRadix
         (T.toDouble kprimDoubleRadix z1primDoubleRadix))
 
gprimDoubleDigits ::
                  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Int)
gprimDoubleDigits pprimDoubleDigits p
  = T.ufun1 aprimDoubleDigits pprimDoubleDigits p
      hprimDoubleDigits
hprimDoubleDigits z1primDoubleDigits
  kprimDoubleDigits
  = T.fromInt kprimDoubleDigits
      (Prelude.floatDigits
         (T.toDouble kprimDoubleDigits z1primDoubleDigits))
 
gprimDoubleRange ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun Double (T.Tuple2 Int Int))
gprimDoubleRange pprimDoubleRange p
  = T.ufun1 aprimDoubleRange pprimDoubleRange p
      hprimDoubleRange
hprimDoubleRange z1primDoubleRange kprimDoubleRange
  = T.fromTuple2 T.fromInt T.fromInt kprimDoubleRange
      (Prelude.floatRange
         (T.toDouble kprimDoubleRange z1primDoubleRange))
 
gprimDecodeDouble ::
                  T.RefSrcPos ->
                    T.RefExp -> T.R (T.Fun Double (T.Tuple2 Integer Int))
gprimDecodeDouble pprimDecodeDouble p
  = T.ufun1 aprimDecodeDouble pprimDecodeDouble p
      hprimDecodeDouble
hprimDecodeDouble z1primDecodeDouble
  kprimDecodeDouble
  = T.fromTuple2 T.fromInteger T.fromInt
      kprimDecodeDouble
      (Prelude.decodeFloat
         (T.toDouble kprimDecodeDouble z1primDecodeDouble))
 
gprimEncodeDouble ::
                  T.RefSrcPos ->
                    T.RefExp -> T.R (T.Fun Integer (T.Fun Int Double))
gprimEncodeDouble pprimEncodeDouble p
  = T.ufun2 aprimEncodeDouble pprimEncodeDouble p
      hprimEncodeDouble
hprimEncodeDouble z1primEncodeDouble
  z2primEncodeDouble kprimEncodeDouble
  = T.fromDouble kprimEncodeDouble
      (Prelude.encodeFloat
         (T.toInteger kprimEncodeDouble z1primEncodeDouble)
         (T.toInt kprimEncodeDouble z2primEncodeDouble))
 
gprimDoubleIsNaN ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Bool)
gprimDoubleIsNaN pprimDoubleIsNaN p
  = T.ufun1 aprimDoubleIsNaN pprimDoubleIsNaN p
      hprimDoubleIsNaN
hprimDoubleIsNaN z1primDoubleIsNaN kprimDoubleIsNaN
  = T.fromBool kprimDoubleIsNaN
      (Prelude.isNaN
         (T.toDouble kprimDoubleIsNaN z1primDoubleIsNaN))
 
gprimDoubleIsInfinite ::
                      T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Bool)
gprimDoubleIsInfinite pprimDoubleIsInfinite p
  = T.ufun1 aprimDoubleIsInfinite pprimDoubleIsInfinite
      p
      hprimDoubleIsInfinite
hprimDoubleIsInfinite z1primDoubleIsInfinite
  kprimDoubleIsInfinite
  = T.fromBool kprimDoubleIsInfinite
      (Prelude.isInfinite
         (T.toDouble kprimDoubleIsInfinite
            z1primDoubleIsInfinite))
 
gprimDoubleIsDenormalized ::
                          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Bool)
gprimDoubleIsDenormalized pprimDoubleIsDenormalized p
  = T.ufun1 aprimDoubleIsDenormalized
      pprimDoubleIsDenormalized
      p
      hprimDoubleIsDenormalized
hprimDoubleIsDenormalized z1primDoubleIsDenormalized
  kprimDoubleIsDenormalized
  = T.fromBool kprimDoubleIsDenormalized
      (Prelude.isDenormalized
         (T.toDouble kprimDoubleIsDenormalized
            z1primDoubleIsDenormalized))
 
gprimDoubleIsNegativeZero ::
                          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Bool)
gprimDoubleIsNegativeZero pprimDoubleIsNegativeZero p
  = T.ufun1 aprimDoubleIsNegativeZero
      pprimDoubleIsNegativeZero
      p
      hprimDoubleIsNegativeZero
hprimDoubleIsNegativeZero z1primDoubleIsNegativeZero
  kprimDoubleIsNegativeZero
  = T.fromBool kprimDoubleIsNegativeZero
      (Prelude.isNegativeZero
         (T.toDouble kprimDoubleIsNegativeZero
            z1primDoubleIsNegativeZero))
 
gprimDoubleIsIEEE ::
                  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Bool)
gprimDoubleIsIEEE pprimDoubleIsIEEE p
  = T.ufun1 aprimDoubleIsIEEE pprimDoubleIsIEEE p
      hprimDoubleIsIEEE
hprimDoubleIsIEEE z1primDoubleIsIEEE
  kprimDoubleIsIEEE
  = T.fromBool kprimDoubleIsIEEE
      (Prelude.isIEEE
         (T.toDouble kprimDoubleIsIEEE z1primDoubleIsIEEE))
 
gprimDoubleEq ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleEq pprimDoubleEq p
  = T.ufun2 aprimDoubleEq pprimDoubleEq p hprimDoubleEq
hprimDoubleEq z1primDoubleEq z2primDoubleEq
  kprimDoubleEq
  = T.fromBool kprimDoubleEq
      ((Prelude.==)
         (T.toDouble kprimDoubleEq z1primDoubleEq)
         (T.toDouble kprimDoubleEq z2primDoubleEq))
 
gprimDoubleNe ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleNe pprimDoubleNe p
  = T.ufun2 aprimDoubleNe pprimDoubleNe p hprimDoubleNe
hprimDoubleNe z1primDoubleNe z2primDoubleNe
  kprimDoubleNe
  = T.fromBool kprimDoubleNe
      ((Prelude./=)
         (T.toDouble kprimDoubleNe z1primDoubleNe)
         (T.toDouble kprimDoubleNe z2primDoubleNe))
 
gprimDoubleLt ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleLt pprimDoubleLt p
  = T.ufun2 aprimDoubleLt pprimDoubleLt p hprimDoubleLt
hprimDoubleLt z1primDoubleLt z2primDoubleLt
  kprimDoubleLt
  = T.fromBool kprimDoubleLt
      ((Prelude.<)
         (T.toDouble kprimDoubleLt z1primDoubleLt)
         (T.toDouble kprimDoubleLt z2primDoubleLt))
 
gprimDoubleLe ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleLe pprimDoubleLe p
  = T.ufun2 aprimDoubleLe pprimDoubleLe p hprimDoubleLe
hprimDoubleLe z1primDoubleLe z2primDoubleLe
  kprimDoubleLe
  = T.fromBool kprimDoubleLe
      ((Prelude.<=)
         (T.toDouble kprimDoubleLe z1primDoubleLe)
         (T.toDouble kprimDoubleLe z2primDoubleLe))
 
gprimDoubleGt ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleGt pprimDoubleGt p
  = T.ufun2 aprimDoubleGt pprimDoubleGt p hprimDoubleGt
hprimDoubleGt z1primDoubleGt z2primDoubleGt
  kprimDoubleGt
  = T.fromBool kprimDoubleGt
      ((Prelude.>)
         (T.toDouble kprimDoubleGt z1primDoubleGt)
         (T.toDouble kprimDoubleGt z2primDoubleGt))
 
gprimDoubleGe ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun Double (T.Fun Double Bool))
gprimDoubleGe pprimDoubleGe p
  = T.ufun2 aprimDoubleGe pprimDoubleGe p hprimDoubleGe
hprimDoubleGe z1primDoubleGe z2primDoubleGe
  kprimDoubleGe
  = T.fromBool kprimDoubleGe
      ((Prelude.>=)
         (T.toDouble kprimDoubleGe z1primDoubleGe)
         (T.toDouble kprimDoubleGe z2primDoubleGe))
 
gprimDoublePi ::
              T.RefSrcPos -> T.RefExp -> T.R Double
gprimDoublePi pprimDoublePi p
  = T.uconstUse pprimDoublePi p sprimDoublePi
sprimDoublePi
  = T.uconstDef p aprimDoublePi
      (\ p -> T.fromDouble p Prelude.pi)
 
gprimDoubleExp ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleExp pprimDoubleExp p
  = T.ufun1 aprimDoubleExp pprimDoubleExp p
      hprimDoubleExp
hprimDoubleExp z1primDoubleExp kprimDoubleExp
  = T.fromDouble kprimDoubleExp
      (Prelude.exp
         (T.toDouble kprimDoubleExp z1primDoubleExp))
 
gprimDoubleLog ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleLog pprimDoubleLog p
  = T.ufun1 aprimDoubleLog pprimDoubleLog p
      hprimDoubleLog
hprimDoubleLog z1primDoubleLog kprimDoubleLog
  = T.fromDouble kprimDoubleLog
      (Prelude.log
         (T.toDouble kprimDoubleLog z1primDoubleLog))
 
gprimDoubleSqrt ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleSqrt pprimDoubleSqrt p
  = T.ufun1 aprimDoubleSqrt pprimDoubleSqrt p
      hprimDoubleSqrt
hprimDoubleSqrt z1primDoubleSqrt kprimDoubleSqrt
  = T.fromDouble kprimDoubleSqrt
      (Prelude.sqrt
         (T.toDouble kprimDoubleSqrt z1primDoubleSqrt))
 
gprimDoubleSin ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleSin pprimDoubleSin p
  = T.ufun1 aprimDoubleSin pprimDoubleSin p
      hprimDoubleSin
hprimDoubleSin z1primDoubleSin kprimDoubleSin
  = T.fromDouble kprimDoubleSin
      (Prelude.sin
         (T.toDouble kprimDoubleSin z1primDoubleSin))
 
gprimDoubleCos ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleCos pprimDoubleCos p
  = T.ufun1 aprimDoubleCos pprimDoubleCos p
      hprimDoubleCos
hprimDoubleCos z1primDoubleCos kprimDoubleCos
  = T.fromDouble kprimDoubleCos
      (Prelude.cos
         (T.toDouble kprimDoubleCos z1primDoubleCos))
 
gprimDoubleTan ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleTan pprimDoubleTan p
  = T.ufun1 aprimDoubleTan pprimDoubleTan p
      hprimDoubleTan
hprimDoubleTan z1primDoubleTan kprimDoubleTan
  = T.fromDouble kprimDoubleTan
      (Prelude.tan
         (T.toDouble kprimDoubleTan z1primDoubleTan))
 
gprimDoubleAsin ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleAsin pprimDoubleAsin p
  = T.ufun1 aprimDoubleAsin pprimDoubleAsin p
      hprimDoubleAsin
hprimDoubleAsin z1primDoubleAsin kprimDoubleAsin
  = T.fromDouble kprimDoubleAsin
      (Prelude.asin
         (T.toDouble kprimDoubleAsin z1primDoubleAsin))
 
gprimDoubleAcos ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleAcos pprimDoubleAcos p
  = T.ufun1 aprimDoubleAcos pprimDoubleAcos p
      hprimDoubleAcos
hprimDoubleAcos z1primDoubleAcos kprimDoubleAcos
  = T.fromDouble kprimDoubleAcos
      (Prelude.acos
         (T.toDouble kprimDoubleAcos z1primDoubleAcos))
 
gprimDoubleAtan ::
                T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleAtan pprimDoubleAtan p
  = T.ufun1 aprimDoubleAtan pprimDoubleAtan p
      hprimDoubleAtan
hprimDoubleAtan z1primDoubleAtan kprimDoubleAtan
  = T.fromDouble kprimDoubleAtan
      (Prelude.atan
         (T.toDouble kprimDoubleAtan z1primDoubleAtan))
 
gprimDoubleDiv ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Double (T.Fun Double Double))
gprimDoubleDiv pprimDoubleDiv p
  = T.ufun2 aprimDoubleDiv pprimDoubleDiv p
      hprimDoubleDiv
hprimDoubleDiv z1primDoubleDiv z2primDoubleDiv
  kprimDoubleDiv
  = T.fromDouble kprimDoubleDiv
      ((Prelude./)
         (T.toDouble kprimDoubleDiv z1primDoubleDiv)
         (T.toDouble kprimDoubleDiv z2primDoubleDiv))
 
gprimDoubleAdd ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Double (T.Fun Double Double))
gprimDoubleAdd pprimDoubleAdd p
  = T.ufun2 aprimDoubleAdd pprimDoubleAdd p
      hprimDoubleAdd
hprimDoubleAdd z1primDoubleAdd z2primDoubleAdd
  kprimDoubleAdd
  = T.fromDouble kprimDoubleAdd
      ((Prelude.+)
         (T.toDouble kprimDoubleAdd z1primDoubleAdd)
         (T.toDouble kprimDoubleAdd z2primDoubleAdd))
 
gprimDoubleSub ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Double (T.Fun Double Double))
gprimDoubleSub pprimDoubleSub p
  = T.ufun2 aprimDoubleSub pprimDoubleSub p
      hprimDoubleSub
hprimDoubleSub z1primDoubleSub z2primDoubleSub
  kprimDoubleSub
  = T.fromDouble kprimDoubleSub
      ((Prelude.-)
         (T.toDouble kprimDoubleSub z1primDoubleSub)
         (T.toDouble kprimDoubleSub z2primDoubleSub))
 
gprimDoubleMul ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun Double (T.Fun Double Double))
gprimDoubleMul pprimDoubleMul p
  = T.ufun2 aprimDoubleMul pprimDoubleMul p
      hprimDoubleMul
hprimDoubleMul z1primDoubleMul z2primDoubleMul
  kprimDoubleMul
  = T.fromDouble kprimDoubleMul
      ((Prelude.*)
         (T.toDouble kprimDoubleMul z1primDoubleMul)
         (T.toDouble kprimDoubleMul z2primDoubleMul))
 
gprimDoubleAbs ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleAbs pprimDoubleAbs p
  = T.ufun1 aprimDoubleAbs pprimDoubleAbs p
      hprimDoubleAbs
hprimDoubleAbs z1primDoubleAbs kprimDoubleAbs
  = T.fromDouble kprimDoubleAbs
      (Prelude.abs
         (T.toDouble kprimDoubleAbs z1primDoubleAbs))
 
gprimDoubleSignum ::
                  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Double Double)
gprimDoubleSignum pprimDoubleSignum p
  = T.ufun1 aprimDoubleSignum pprimDoubleSignum p
      hprimDoubleSignum
hprimDoubleSignum z1primDoubleSignum
  kprimDoubleSignum
  = T.fromDouble kprimDoubleSignum
      (Prelude.signum
         (T.toDouble kprimDoubleSignum z1primDoubleSignum))
 
gprimIOBind ::
            T.RefSrcPos ->
              T.RefExp ->
                T.R (T.Fun (IO a) (T.Fun (T.Fun a (IO b)) (IO b)))
gprimIOBind pprimIOBind p
  = T.ufun2 aprimIOBind pprimIOBind p hprimIOBind
hprimIOBind z1primIOBind z2primIOBind kprimIOBind
  = T.fromIO T.fromId kprimIOBind
      ((Prelude.>>=)
         (T.toIO T.toId kprimIOBind z1primIOBind)
         ((T.toFun T.fromId (T.toIO T.toId)) kprimIOBind
            z2primIOBind))
 
gprimIOReturn ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (IO a))
gprimIOReturn pprimIOReturn p
  = T.ufun1 aprimIOReturn pprimIOReturn p hprimIOReturn
hprimIOReturn z1primIOReturn kprimIOReturn
  = T.fromIO T.fromId kprimIOReturn
      (Prelude.return
         (T.toId kprimIOReturn z1primIOReturn))
 
gprimIOErrorShow ::
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun IOError String)
gprimIOErrorShow pprimIOErrorShow p
  = T.ufun1 aprimIOErrorShow pprimIOErrorShow p
      hprimIOErrorShow
hprimIOErrorShow z1primIOErrorShow kprimIOErrorShow
  = T.fromString kprimIOErrorShow
      (Prelude.show
         (T.toIOError kprimIOErrorShow z1primIOErrorShow))
 
gioError ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun IOError (IO a))
gioError pioError p
  = T.ufun1 aioError pioError p hioError
hioError z1ioError kioError
  = T.fromIO T.fromId kioError
      (Prelude.ioError (T.toIOError kioError z1ioError))
 
guserError ::
           T.RefSrcPos -> T.RefExp -> T.R (T.Fun String IOError)
guserError puserError p
  = T.ufun1 auserError puserError p huserError
huserError z1userError kuserError
  = T.fromIOError kuserError
      (Prelude.userError
         (T.toString kuserError z1userError))
 
gcatch ::
       T.RefSrcPos ->
         T.RefExp ->
           T.R
             (T.Fun (IO a) (T.Fun (T.Fun IOError (IO a)) (IO a)))
gcatch pcatch p = T.ufun2 acatch pcatch p hcatch
hcatch z1catch z2catch kcatch
  = T.fromIO T.fromId kcatch
      (System.IO.Error.catchIOError
         (T.toIO T.toId kcatch z1catch)
         ((T.toFun T.fromIOError (T.toIO T.toId)) kcatch
            z2catch))
 
gputChar ::
         T.RefSrcPos ->
           T.RefExp -> T.R (T.Fun Char (IO T.Tuple0))
gputChar pputChar p
  = T.ufun1 aputChar pputChar p hputChar
hputChar z1putChar kputChar
  = T.fromIO T.fromTuple0 kputChar
      ((\_ c -> T.outputTrace kputChar [c] Prelude.>> Prelude.putChar c) Prelude.True
         (T.toChar kputChar z1putChar))
 
ggetChar :: T.RefSrcPos -> T.RefExp -> T.R (IO Char)
ggetChar pgetChar p = T.uconstUse pgetChar p sgetChar
sgetChar
  = T.uconstDef p agetChar
      (\ p -> T.fromIO T.fromChar p Prelude.getChar)
 
ggetContents ::
             T.RefSrcPos -> T.RefExp -> T.R (IO String)
ggetContents pgetContents p
  = T.uconstUse pgetContents p sgetContents
sgetContents
  = T.uconstDef p agetContents
      (\ p -> T.fromIO T.fromString p Prelude.getContents)
 
greadFile ::
          T.RefSrcPos ->
            T.RefExp -> T.R (T.Fun String (IO String))
greadFile preadFile p
  = T.ufun1 areadFile preadFile p hreadFile
hreadFile z1readFile kreadFile
  = T.fromIO T.fromString kreadFile
      (Prelude.readFile (T.toString kreadFile z1readFile))
 
gwriteFile ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R (T.Fun String (T.Fun String (IO T.Tuple0)))
gwriteFile pwriteFile p
  = T.ufun2 awriteFile pwriteFile p hwriteFile
hwriteFile z1writeFile z2writeFile kwriteFile
  = T.fromIO T.fromTuple0 kwriteFile
      ((\_ n s -> T.outputTrace kwriteFile s Prelude.>> Prelude.writeFile n s) Prelude.True
         (T.toString kwriteFile z1writeFile)
         (T.toString kwriteFile z2writeFile))
 
gappendFile ::
            T.RefSrcPos ->
              T.RefExp ->
                T.R (T.Fun String (T.Fun String (IO T.Tuple0)))
gappendFile pappendFile p
  = T.ufun2 aappendFile pappendFile p happendFile
happendFile z1appendFile z2appendFile kappendFile
  = T.fromIO T.fromTuple0 kappendFile
      ((\_ n s -> T.outputTrace kappendFile s Prelude.>> Prelude.appendFile n s) Prelude.True
         (T.toString kappendFile z1appendFile)
         (T.toString kappendFile z2appendFile))
aappendFile
  = T.mkVariable tPreludeBuiltin 3630001 3640021 3 (2)
      "appendFile"
      Prelude.False
acatch
  = T.mkVariable tPreludeBuiltin 3410001 3420010 3 (2)
      "catch"
      Prelude.False
agetChar
  = T.mkVariable tPreludeBuiltin 3510001 3520021 3 (0)
      "getChar"
      Prelude.False
agetContents
  = T.mkVariable tPreludeBuiltin 3540001 3550021 3 (0)
      "getContents"
      Prelude.False
aioError
  = T.mkVariable tPreludeBuiltin 3350001 3360012 3 (1)
      "ioError"
      Prelude.False
aisAlpha
  = T.mkVariable tPreludeBuiltin 860001 870012 3 (1)
      "isAlpha"
      Prelude.False
aisAlphaNum
  = T.mkVariable tPreludeBuiltin 940001 950015 3 (1)
      "isAlphaNum"
      Prelude.False
aisAscii
  = T.mkVariable tPreludeBuiltin 720001 730012 3 (1)
      "isAscii"
      Prelude.False
aisControl
  = T.mkVariable tPreludeBuiltin 760001 770014 3 (1)
      "isControl"
      Prelude.False
aisDigit
  = T.mkVariable tPreludeBuiltin 880001 890012 3 (1)
      "isDigit"
      Prelude.False
aisHexDigit
  = T.mkVariable tPreludeBuiltin 920001 930015 3 (1)
      "isHexDigit"
      Prelude.False
aisLatin1
  = T.mkVariable tPreludeBuiltin 740001 750013 3 (1)
      "isLatin1"
      Prelude.False
aisLower
  = T.mkVariable tPreludeBuiltin 840001 850012 3 (1)
      "isLower"
      Prelude.False
aisOctDigit
  = T.mkVariable tPreludeBuiltin 900001 910015 3 (1)
      "isOctDigit"
      Prelude.False
aisPrint
  = T.mkVariable tPreludeBuiltin 780001 790012 3 (1)
      "isPrint"
      Prelude.False
aisSpace
  = T.mkVariable tPreludeBuiltin 800001 810012 3 (1)
      "isSpace"
      Prelude.False
aisUpper
  = T.mkVariable tPreludeBuiltin 820001 830012 3 (1)
      "isUpper"
      Prelude.False
aprimCharToInt
  = T.mkVariable tPreludeBuiltin 1030001 1040018 3 (1)
      "primCharToInt"
      Prelude.False
aprimDecodeDouble
  = T.mkVariable tPreludeBuiltin 2570001 2580021 3 (1)
      "primDecodeDouble"
      Prelude.False
aprimDecodeFloat
  = T.mkVariable tPreludeBuiltin 1890001 1900026 3 (1)
      "primDecodeFloat"
      Prelude.False
aprimDoubleAbs
  = T.mkVariable tPreludeBuiltin 3120001 3130021 3 (1)
      "primDoubleAbs"
      Prelude.False
aprimDoubleAcos
  = T.mkVariable tPreludeBuiltin 3000001 3010019 3 (1)
      "primDoubleAcos"
      Prelude.False
aprimDoubleAdd
  = T.mkVariable tPreludeBuiltin 3060001 3070019 3 (2)
      "primDoubleAdd"
      Prelude.False
aprimDoubleAsin
  = T.mkVariable tPreludeBuiltin 2980001 2990019 3 (1)
      "primDoubleAsin"
      Prelude.False
aprimDoubleAtan
  = T.mkVariable tPreludeBuiltin 3020001 3030019 3 (1)
      "primDoubleAtan"
      Prelude.False
aprimDoubleCos
  = T.mkVariable tPreludeBuiltin 2940001 2950019 3 (1)
      "primDoubleCos"
      Prelude.False
aprimDoubleDigits
  = T.mkVariable tPreludeBuiltin 2530001 2540022 3 (1)
      "primDoubleDigits"
      Prelude.False
aprimDoubleDiv
  = T.mkVariable tPreludeBuiltin 3040001 3050019 3 (2)
      "primDoubleDiv"
      Prelude.False
aprimDoubleEq
  = T.mkVariable tPreludeBuiltin 2720001 2730017 3 (2)
      "primDoubleEq"
      Prelude.False
aprimDoubleExp
  = T.mkVariable tPreludeBuiltin 2860001 2870019 3 (1)
      "primDoubleExp"
      Prelude.False
aprimDoubleFromInteger
  = T.mkVariable tPreludeBuiltin 2490001 2500026 3 (1)
      "primDoubleFromInteger"
      Prelude.False
aprimDoubleGe
  = T.mkVariable tPreludeBuiltin 2820001 2830017 3 (2)
      "primDoubleGe"
      Prelude.False
aprimDoubleGt
  = T.mkVariable tPreludeBuiltin 2800001 2810017 3 (2)
      "primDoubleGt"
      Prelude.False
aprimDoubleIsDenormalized
  = T.mkVariable tPreludeBuiltin 2650001 2660031 3 (1)
      "primDoubleIsDenormalized"
      Prelude.False
aprimDoubleIsIEEE
  = T.mkVariable tPreludeBuiltin 2690001 2700023 3 (1)
      "primDoubleIsIEEE"
      Prelude.False
aprimDoubleIsInfinite
  = T.mkVariable tPreludeBuiltin 2630001 2640027 3 (1)
      "primDoubleIsInfinite"
      Prelude.False
aprimDoubleIsNaN
  = T.mkVariable tPreludeBuiltin 2610001 2620022 3 (1)
      "primDoubleIsNaN"
      Prelude.False
aprimDoubleIsNegativeZero
  = T.mkVariable tPreludeBuiltin 2670001 2680031 3 (1)
      "primDoubleIsNegativeZero"
      Prelude.False
aprimDoubleLe
  = T.mkVariable tPreludeBuiltin 2780001 2790017 3 (2)
      "primDoubleLe"
      Prelude.False
aprimDoubleLog
  = T.mkVariable tPreludeBuiltin 2880001 2890019 3 (1)
      "primDoubleLog"
      Prelude.False
aprimDoubleLt
  = T.mkVariable tPreludeBuiltin 2760001 2770017 3 (2)
      "primDoubleLt"
      Prelude.False
aprimDoubleMul
  = T.mkVariable tPreludeBuiltin 3100001 3110019 3 (2)
      "primDoubleMul"
      Prelude.False
aprimDoubleNe
  = T.mkVariable tPreludeBuiltin 2740001 2750017 3 (2)
      "primDoubleNe"
      Prelude.False
aprimDoublePi
  = T.mkVariable tPreludeBuiltin 2840001 2850017 3 (0)
      "primDoublePi"
      Prelude.False
aprimDoubleRadix
  = T.mkVariable tPreludeBuiltin 2510001 2520022 3 (1)
      "primDoubleRadix"
      Prelude.False
aprimDoubleRange
  = T.mkVariable tPreludeBuiltin 2550001 2560022 3 (1)
      "primDoubleRange"
      Prelude.False
aprimDoubleSignum
  = T.mkVariable tPreludeBuiltin 3140001 3150021 3 (1)
      "primDoubleSignum"
      Prelude.False
aprimDoubleSin
  = T.mkVariable tPreludeBuiltin 2920001 2930019 3 (1)
      "primDoubleSin"
      Prelude.False
aprimDoubleSqrt
  = T.mkVariable tPreludeBuiltin 2900001 2910019 3 (1)
      "primDoubleSqrt"
      Prelude.False
aprimDoubleSub
  = T.mkVariable tPreludeBuiltin 3080001 3090019 3 (2)
      "primDoubleSub"
      Prelude.False
aprimDoubleTan
  = T.mkVariable tPreludeBuiltin 2960001 2970019 3 (1)
      "primDoubleTan"
      Prelude.False
aprimEncodeDouble
  = T.mkVariable tPreludeBuiltin 2590001 2600021 3 (2)
      "primEncodeDouble"
      Prelude.False
aprimEncodeFloat
  = T.mkVariable tPreludeBuiltin 1910001 1920026 3 (2)
      "primEncodeFloat"
      Prelude.False
aprimFloatAbs
  = T.mkVariable tPreludeBuiltin 2440001 2450020 3 (1)
      "primFloatAbs"
      Prelude.False
aprimFloatAcos
  = T.mkVariable tPreludeBuiltin 2320001 2330018 3 (1)
      "primFloatAcos"
      Prelude.False
aprimFloatAdd
  = T.mkVariable tPreludeBuiltin 2380001 2390018 3 (2)
      "primFloatAdd"
      Prelude.False
aprimFloatAsin
  = T.mkVariable tPreludeBuiltin 2300001 2310018 3 (1)
      "primFloatAsin"
      Prelude.False
aprimFloatAtan
  = T.mkVariable tPreludeBuiltin 2340001 2350018 3 (1)
      "primFloatAtan"
      Prelude.False
aprimFloatCos
  = T.mkVariable tPreludeBuiltin 2260001 2270018 3 (1)
      "primFloatCos"
      Prelude.False
aprimFloatDigits
  = T.mkVariable tPreludeBuiltin 1850001 1860026 3 (1)
      "primFloatDigits"
      Prelude.False
aprimFloatDiv
  = T.mkVariable tPreludeBuiltin 2360001 2370018 3 (2)
      "primFloatDiv"
      Prelude.False
aprimFloatEq
  = T.mkVariable tPreludeBuiltin 2040001 2050016 3 (2)
      "primFloatEq"
      Prelude.False
aprimFloatExp
  = T.mkVariable tPreludeBuiltin 2180001 2190018 3 (1)
      "primFloatExp"
      Prelude.False
aprimFloatFromInteger
  = T.mkVariable tPreludeBuiltin 1810001 1820026 3 (1)
      "primFloatFromInteger"
      Prelude.False
aprimFloatGe
  = T.mkVariable tPreludeBuiltin 2140001 2150016 3 (2)
      "primFloatGe"
      Prelude.False
aprimFloatGt
  = T.mkVariable tPreludeBuiltin 2120001 2130016 3 (2)
      "primFloatGt"
      Prelude.False
aprimFloatIsDenormalized
  = T.mkVariable tPreludeBuiltin 1970001 1980030 3 (1)
      "primFloatIsDenormalized"
      Prelude.False
aprimFloatIsIEEE
  = T.mkVariable tPreludeBuiltin 2010001 2020022 3 (1)
      "primFloatIsIEEE"
      Prelude.False
aprimFloatIsInfinite
  = T.mkVariable tPreludeBuiltin 1950001 1960026 3 (1)
      "primFloatIsInfinite"
      Prelude.False
aprimFloatIsNaN
  = T.mkVariable tPreludeBuiltin 1930001 1940021 3 (1)
      "primFloatIsNaN"
      Prelude.False
aprimFloatIsNegativeZero
  = T.mkVariable tPreludeBuiltin 1990001 2000030 3 (1)
      "primFloatIsNegativeZero"
      Prelude.False
aprimFloatLe
  = T.mkVariable tPreludeBuiltin 2100001 2110016 3 (2)
      "primFloatLe"
      Prelude.False
aprimFloatLog
  = T.mkVariable tPreludeBuiltin 2200001 2210018 3 (1)
      "primFloatLog"
      Prelude.False
aprimFloatLt
  = T.mkVariable tPreludeBuiltin 2080001 2090016 3 (2)
      "primFloatLt"
      Prelude.False
aprimFloatMul
  = T.mkVariable tPreludeBuiltin 2420001 2430018 3 (2)
      "primFloatMul"
      Prelude.False
aprimFloatNe
  = T.mkVariable tPreludeBuiltin 2060001 2070016 3 (2)
      "primFloatNe"
      Prelude.False
aprimFloatPi
  = T.mkVariable tPreludeBuiltin 2160001 2170016 3 (0)
      "primFloatPi"
      Prelude.False
aprimFloatRadix
  = T.mkVariable tPreludeBuiltin 1830001 1840026 3 (1)
      "primFloatRadix"
      Prelude.False
aprimFloatRange
  = T.mkVariable tPreludeBuiltin 1870001 1880026 3 (1)
      "primFloatRange"
      Prelude.False
aprimFloatSignum
  = T.mkVariable tPreludeBuiltin 2460001 2470020 3 (1)
      "primFloatSignum"
      Prelude.False
aprimFloatSin
  = T.mkVariable tPreludeBuiltin 2240001 2250018 3 (1)
      "primFloatSin"
      Prelude.False
aprimFloatSqrt
  = T.mkVariable tPreludeBuiltin 2220001 2230018 3 (1)
      "primFloatSqrt"
      Prelude.False
aprimFloatSub
  = T.mkVariable tPreludeBuiltin 2400001 2410018 3 (2)
      "primFloatSub"
      Prelude.False
aprimFloatTan
  = T.mkVariable tPreludeBuiltin 2280001 2290018 3 (1)
      "primFloatTan"
      Prelude.False
aprimIOBind
  = T.mkVariable tPreludeBuiltin 3200001 3210015 3 (2)
      "primIOBind"
      Prelude.False
aprimIOErrorShow
  = T.mkVariable tPreludeBuiltin 3260001 3270020 3 (1)
      "primIOErrorShow"
      Prelude.False
aprimIOReturn
  = T.mkVariable tPreludeBuiltin 3220001 3230017 3 (1)
      "primIOReturn"
      Prelude.False
aprimIntAbs
  = T.mkVariable tPreludeBuiltin 1440001 1450018 3 (1)
      "primIntAbs"
      Prelude.False
aprimIntEq
  = T.mkVariable tPreludeBuiltin 1200001 1210014 3 (2)
      "primIntEq"
      Prelude.False
aprimIntFromInteger
  = T.mkVariable tPreludeBuiltin 1510001 1520023 3 (1)
      "primIntFromInteger"
      Prelude.False
aprimIntGe
  = T.mkVariable tPreludeBuiltin 1300001 1310014 3 (2)
      "primIntGe"
      Prelude.False
aprimIntGt
  = T.mkVariable tPreludeBuiltin 1280001 1290014 3 (2)
      "primIntGt"
      Prelude.False
aprimIntLe
  = T.mkVariable tPreludeBuiltin 1260001 1270014 3 (2)
      "primIntLe"
      Prelude.False
aprimIntLt
  = T.mkVariable tPreludeBuiltin 1240001 1250014 3 (2)
      "primIntLt"
      Prelude.False
aprimIntMaxBound
  = T.mkVariable tPreludeBuiltin 1170001 1180020 3 (0)
      "primIntMaxBound"
      Prelude.False
aprimIntMinBound
  = T.mkVariable tPreludeBuiltin 1150001 1160020 3 (0)
      "primIntMinBound"
      Prelude.False
aprimIntMinus
  = T.mkVariable tPreludeBuiltin 1380001 1390018 3 (2)
      "primIntMinus"
      Prelude.False
aprimIntNe
  = T.mkVariable tPreludeBuiltin 1220001 1230014 3 (2)
      "primIntNe"
      Prelude.False
aprimIntNegate
  = T.mkVariable tPreludeBuiltin 1420001 1430018 3 (1)
      "primIntNegate"
      Prelude.False
aprimIntPlus
  = T.mkVariable tPreludeBuiltin 1360001 1370018 3 (2)
      "primIntPlus"
      Prelude.False
aprimIntQuot
  = T.mkVariable tPreludeBuiltin 1320001 1330018 3 (2)
      "primIntQuot"
      Prelude.False
aprimIntRem
  = T.mkVariable tPreludeBuiltin 1340001 1350018 3 (2)
      "primIntRem"
      Prelude.False
aprimIntSignum
  = T.mkVariable tPreludeBuiltin 1460001 1470018 3 (1)
      "primIntSignum"
      Prelude.False
aprimIntTimes
  = T.mkVariable tPreludeBuiltin 1400001 1410018 3 (2)
      "primIntTimes"
      Prelude.False
aprimIntToChar
  = T.mkVariable tPreludeBuiltin 1050001 1060018 3 (1)
      "primIntToChar"
      Prelude.False
aprimIntegerAdd
  = T.mkVariable tPreludeBuiltin 1720001 1730023 3 (2)
      "primIntegerAdd"
      Prelude.False
aprimIntegerEq
  = T.mkVariable tPreludeBuiltin 1540001 1550018 3 (2)
      "primIntegerEq"
      Prelude.False
aprimIntegerFromInt
  = T.mkVariable tPreludeBuiltin 1490001 1500023 3 (1)
      "primIntegerFromInt"
      Prelude.False
aprimIntegerGe
  = T.mkVariable tPreludeBuiltin 1640001 1650018 3 (2)
      "primIntegerGe"
      Prelude.False
aprimIntegerGt
  = T.mkVariable tPreludeBuiltin 1620001 1630018 3 (2)
      "primIntegerGt"
      Prelude.False
aprimIntegerLe
  = T.mkVariable tPreludeBuiltin 1600001 1610018 3 (2)
      "primIntegerLe"
      Prelude.False
aprimIntegerLt
  = T.mkVariable tPreludeBuiltin 1580001 1590018 3 (2)
      "primIntegerLt"
      Prelude.False
aprimIntegerMul
  = T.mkVariable tPreludeBuiltin 1760001 1770023 3 (2)
      "primIntegerMul"
      Prelude.False
aprimIntegerNe
  = T.mkVariable tPreludeBuiltin 1560001 1570018 3 (2)
      "primIntegerNe"
      Prelude.False
aprimIntegerNeg
  = T.mkVariable tPreludeBuiltin 1780001 1790023 3 (1)
      "primIntegerNeg"
      Prelude.False
aprimIntegerQuot
  = T.mkVariable tPreludeBuiltin 1660001 1670023 3 (2)
      "primIntegerQuot"
      Prelude.False
aprimIntegerQuotRem
  = T.mkVariable tPreludeBuiltin 1700001 1710023 3 (2)
      "primIntegerQuotRem"
      Prelude.False
aprimIntegerRem
  = T.mkVariable tPreludeBuiltin 1680001 1690023 3 (2)
      "primIntegerRem"
      Prelude.False
aprimIntegerSub
  = T.mkVariable tPreludeBuiltin 1740001 1750023 3 (2)
      "primIntegerSub"
      Prelude.False
aprimUnicodeMaxBound
  = T.mkVariable tPreludeBuiltin 1080001 1090024 3 (0)
      "primUnicodeMaxBound"
      Prelude.False
aputChar
  = T.mkVariable tPreludeBuiltin 3470001 3490012 3 (1)
      "putChar"
      Prelude.False
areadFile
  = T.mkVariable tPreludeBuiltin 3570001 3580021 3 (1)
      "readFile"
      Prelude.False
aseq
  = T.mkVariable tPreludeBuiltin 690001 700008 1 (2)
      "seq"
      Prelude.False
atoLower
  = T.mkVariable tPreludeBuiltin 990001 1000012 3 (1)
      "toLower"
      Prelude.False
atoUpper
  = T.mkVariable tPreludeBuiltin 970001 980012 3 (1)
      "toUpper"
      Prelude.False
auserError
  = T.mkVariable tPreludeBuiltin 3380001 3390014 3 (1)
      "userError"
      Prelude.False
awriteFile
  = T.mkVariable tPreludeBuiltin 3600001 3610021 3 (2)
      "writeFile"
      Prelude.False
p = T.mkRoot
tPreludeBuiltin
  = T.mkModule "PreludeBuiltin" "PreludeBuiltin.hs"
      Prelude.False