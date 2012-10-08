module Hat.RandomBuiltin(StdGen,module Hat.RandomBuiltin) where

import Hat.Hat as T
import System.Random

toStdGen :: RefExp -> R StdGen -> StdGen
toStdGen h (R v _) = v

fromStdGen :: RefExp -> StdGen -> R StdGen
fromStdGen h v = R v (T.mkValueUse h mkNoSrcPos aStdGen)

aStdGen :: RefAtom
aStdGen = mkAbstract "StdGen"

