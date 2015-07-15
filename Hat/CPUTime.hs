module Hat.CPUTime (ggetCPUTime, gcpuTimePrecision)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes
import qualified System.CPUTime as CPUTime
 
ggetCPUTime ::
            T.RefSrcPos -> T.RefExp -> T.R (IO Integer)
ggetCPUTime pgetCPUTime p
  = T.uconstUse pgetCPUTime p sgetCPUTime
sgetCPUTime
  = T.uconstDef p agetCPUTime
      (\ p -> T.fromIO T.fromInteger p CPUTime.getCPUTime)
 
gcpuTimePrecision ::
                  T.RefSrcPos -> T.RefExp -> T.R Integer
gcpuTimePrecision pcpuTimePrecision p
  = T.uconstUse pcpuTimePrecision p scpuTimePrecision
scpuTimePrecision
  = T.uconstDef p acpuTimePrecision
      (\ p -> T.fromInteger p CPUTime.cpuTimePrecision)
acpuTimePrecision
  = T.mkVariable tCPUTime 90001 100021 3 (0)
      "cpuTimePrecision"
      Prelude.False
agetCPUTime
  = T.mkVariable tCPUTime 60001 70021 3 (0)
      "getCPUTime"
      Prelude.False
p = T.mkRoot
tCPUTime
  = T.mkModule "CPUTime" "CPUTime.hs" Prelude.False