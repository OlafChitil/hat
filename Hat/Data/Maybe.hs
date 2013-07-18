module Hat.Data.Maybe
       (Maybe(Nothing, Just), aNothing, aJust, gmaybe,
        amaybe, hmaybe, gisJust, aisJust, hisJust,
        gisNothing, gfromJust, afromJust, hfromJust,
        gfromMaybe, afromMaybe, hfromMaybe, glistToMaybe,
        alistToMaybe, hlistToMaybe, gmaybeToList,
        amaybeToList, hmaybeToList, gcatMaybes, acatMaybes,
        hcatMaybes, gmapMaybe, amapMaybe, hmapMaybe)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Maybe
p = T.mkRoot
tMaybe
  = T.mkModule "Data.Maybe" "Data/Maybe.hs"
      Prelude.False