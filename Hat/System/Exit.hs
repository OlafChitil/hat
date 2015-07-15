module Hat.System.Exit
       (ExitCode(ExitSuccess, ExitFailure), aExitSuccess,
        aExitFailure, gexitWith, aexitWith, hexitWith,
        gexitFailure, gexitSuccess)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes
import Hat.SystemBuiltinTypes
import Hat.SystemBuiltin as T
import qualified System.Exit
 
gexitWith ::
          T.RefSrcPos ->
            T.RefExp -> T.R (T.Fun ExitCode (IO a))
gexitWith pexitWith p
  = T.ufun1 aexitWith pexitWith p hexitWith
hexitWith z1exitWith kexitWith
  = T.fromIO T.fromId kexitWith
      (System.Exit.exitWith
         (T.toExitCode kexitWith z1exitWith))
 
gexitFailure :: T.RefSrcPos -> T.RefExp -> T.R (IO a)
gexitFailure pexitFailure p
  = T.uconstUse pexitFailure p sexitFailure
sexitFailure
  = T.uconstDef p aexitFailure
      (\ p -> T.fromIO T.fromId p System.Exit.exitFailure)
 
gexitSuccess :: T.RefSrcPos -> T.RefExp -> T.R (IO a)
gexitSuccess pexitSuccess p
  = T.uconstUse pexitSuccess p sexitSuccess
sexitSuccess
  = T.uconstDef p aexitSuccess
      (\ p -> T.fromIO T.fromId p System.Exit.exitSuccess)
aexitFailure
  = T.mkVariable tExit 120001 130015 3 (0)
      "exitFailure"
      Prelude.False
aexitSuccess
  = T.mkVariable tExit 140001 150015 3 (0)
      "exitSuccess"
      Prelude.False
aexitWith
  = T.mkVariable tExit 100001 110012 3 (1) "exitWith"
      Prelude.False
p = T.mkRoot
tExit
  = T.mkModule "System.Exit" "System/Exit.hs"
      Prelude.False