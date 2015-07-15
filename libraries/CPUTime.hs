module CPUTime ( getCPUTime, cpuTimePrecision ) where

import PreludeBuiltinTypes
import qualified NotHat.System.CPUTime as NotHat.CPUTime

foreign import ccall "NotHat.CPUTime.getCPUTime"
 getCPUTime        :: IO Integer

foreign import ccall "NotHat.CPUTime.cpuTimePrecision"
 cpuTimePrecision  :: Integer