module Hat.Char
       (gisAscii, aisAscii, hisAscii, gisLatin1, aisLatin1,
        hisLatin1, gisControl, aisControl, hisControl,
        gisPrint, aisPrint, hisPrint, gisSpace, aisSpace,
        hisSpace, gisUpper, aisUpper, hisUpper, gisLower,
        aisLower, hisLower, gisAlpha, aisAlpha, hisAlpha,
        gisDigit, aisDigit, hisDigit, gisOctDigit,
        aisOctDigit, hisOctDigit, gisHexDigit, aisHexDigit,
        hisHexDigit, gisAlphaNum, aisAlphaNum, hisAlphaNum,
        gdigitToInt, adigitToInt, hdigitToInt, gintToDigit,
        aintToDigit, hintToDigit, gtoUpper, atoUpper,
        htoUpper, gtoLower, atoLower, htoLower, gord, gchr,
        greadLitChar, areadLitChar, hreadLitChar,
        gshowLitChar, ashowLitChar, hshowLitChar,
        glexLitChar, alexLitChar, hlexLitChar, Char, String)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBasic
p = T.mkRoot
tChar = T.mkModule "Char" "Char.hs" Prelude.False