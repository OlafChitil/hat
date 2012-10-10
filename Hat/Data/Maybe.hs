module Hat.Data.Maybe
  (Maybe(Nothing,Just),aNothing,aJust,gmaybe,amaybe,hmaybe,gisJust,aisJust
    ,hisJust,gisNothing,gfromJust,afromJust,hfromJust,gfromMaybe,afromMaybe
    ,hfromMaybe,glistToMaybe,alistToMaybe,hlistToMaybe,gmaybeToList,amaybeToList
    ,hmaybeToList,gcatMaybes,acatMaybes,hcatMaybes,gmapMaybe,amapMaybe
    ,hmapMaybe) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Maybe 

tData_Maybe = T.mkModule "Data.Maybe" "Data/Maybe.hs" Prelude.False
