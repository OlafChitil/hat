module Hat.CPUTime (ggetCPUTime,gcpuTimePrecision) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBuiltinTypes 
import qualified System.CPUTime as CPUTime 

ggetCPUTime :: T.RefSrcPos -> T.RefExp -> T.R (IO Integer)

ggetCPUTime pgetCPUTime p = T.uconstUse pgetCPUTime p sgetCPUTime

sgetCPUTime =
  T.uconstDef T.mkRoot agetCPUTime
    (\ p -> (T.fromIO T.fromInteger) p CPUTime.getCPUTime)

gcpuTimePrecision :: T.RefSrcPos -> T.RefExp -> T.R Integer

gcpuTimePrecision pcpuTimePrecision p =
  T.uconstUse pcpuTimePrecision p scpuTimePrecision

scpuTimePrecision =
  T.uconstDef T.mkRoot acpuTimePrecision
    (\ p -> T.fromInteger p CPUTime.cpuTimePrecision)

tCPUTime = T.mkModule "CPUTime" "CPUTime.hs" Prelude.False

agetCPUTime = T.mkVariable tCPUTime 60001 70024 3 0 "getCPUTime" Prelude.False

acpuTimePrecision =
  T.mkVariable tCPUTime 90001 100029 3 0 "cpuTimePrecision" Prelude.False
