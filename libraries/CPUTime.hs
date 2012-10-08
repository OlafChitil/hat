module CPUTime ( getCPUTime, cpuTimePrecision ) where

import PreludeBuiltinTypes
import qualified TraceOrigSystem.CPUTime as TraceOrigCPUTime

foreign import haskell "CPUTime.getCPUTime"
 getCPUTime        :: IO Integer

foreign import haskell "CPUTime.cpuTimePrecision"
 cpuTimePrecision  :: Integer