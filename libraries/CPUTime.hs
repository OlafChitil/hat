module CPUTime ( getCPUTime, cpuTimePrecision ) where

import PreludeBuiltinTypes
import qualified NotHat.System.CPUTime as NotHat.CPUTime

foreign import haskell "CPUTime.getCPUTime"
 getCPUTime        :: IO Integer

foreign import haskell "CPUTime.cpuTimePrecision"
 cpuTimePrecision  :: Integer