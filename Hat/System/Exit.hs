module Hat.System.Exit
  (ExitCode(ExitSuccess,ExitFailure),aExitSuccess,aExitFailure,gexitWith
    ,aexitWith,hexitWith,gexitFailure,gexitSuccess) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBuiltinTypes 
import Hat.SystemBuiltinTypes 
import Hat.SystemBuiltin 
import qualified System.Exit 

gexitWith :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun ExitCode (IO a))

gexitWith pexitWith p = T.ufun1 aexitWith pexitWith p hexitWith

hexitWith z1exitWith kexitWith =
  (T.fromIO T.fromId) kexitWith
    (System.Exit.exitWith (toExitCode kexitWith z1exitWith))

gexitFailure :: T.RefSrcPos -> T.RefExp -> T.R (IO a)

gexitFailure pexitFailure p = T.uconstUse pexitFailure p sexitFailure

sexitFailure =
  T.uconstDef T.mkRoot aexitFailure
    (\ p -> (T.fromIO T.fromId) p System.Exit.exitFailure)

gexitSuccess :: T.RefSrcPos -> T.RefExp -> T.R (IO a)

gexitSuccess pexitSuccess p = T.uconstUse pexitSuccess p sexitSuccess

sexitSuccess =
  T.uconstDef T.mkRoot aexitSuccess
    (\ p -> (T.fromIO T.fromId) p System.Exit.exitSuccess)

tSystem_Exit = T.mkModule "System.Exit" "System/Exit.hs" Prelude.False

aexitWith = T.mkVariable tSystem_Exit 100001 110027 3 1 "exitWith" Prelude.False

aexitFailure =
  T.mkVariable tSystem_Exit 120001 130018 3 0 "exitFailure" Prelude.False

aexitSuccess =
  T.mkVariable tSystem_Exit 140001 150018 3 0 "exitSuccess" Prelude.False
