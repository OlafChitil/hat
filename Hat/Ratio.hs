module Hat.Ratio
       (Ratio, Rational, (!%), (+%), (*%), gnumerator,
        anumerator, hnumerator, gdenominator, adenominator,
        hdenominator, gapproxRational, aapproxRational,
        happroxRational)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBasic
p = T.mkRoot
tRatio = T.mkModule "Ratio" "Ratio.hs" Prelude.False