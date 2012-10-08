module Hat.Ratio
  (Ratio(),Rational(),(!%),(+%),(*%),gnumerator,anumerator,hnumerator
    ,gdenominator,adenominator,hdenominator,gapproxRational,aapproxRational
    ,happroxRational) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBasic 

tRatio = T.mkModule "Ratio" "Ratio.hs" Prelude.False
