module Hat.Numeric
  (gfromRat,afromRat,hfromRat,gshowSigned,ashowSigned,hshowSigned,gshowIntAtBase
    ,ashowIntAtBase,hshowIntAtBase,gshowInt,gshowOct,gshowHex,greadSigned
    ,areadSigned,hreadSigned,greadInt,areadInt,hreadInt,greadDec,greadOct
    ,greadHex,gfloatToDigits,afloatToDigits,hfloatToDigits,gshowEFloat
    ,ashowEFloat,hshowEFloat,gshowFFloat,ashowFFloat,hshowFFloat,gshowGFloat
    ,ashowGFloat,hshowGFloat,gshowFloat,greadFloat,areadFloat,hreadFloat
    ,glexDigits) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBasic 

tNumeric = T.mkModule "Numeric" "Numeric.hs" Prelude.False
