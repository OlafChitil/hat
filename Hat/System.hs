module Hat.System
       (ExitCode(ExitSuccess, ExitFailure), aExitSuccess,
        aExitFailure, ggetArgs, ggetProgName, ggetEnv,
        agetEnv, hgetEnv, gsystem, asystem, hsystem,
        gexitWith, aexitWith, hexitWith, gexitFailure)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes as T
import Hat.SystemBuiltinTypes as T
import Hat.SystemBuiltin as T
import qualified System.Cmd as Cmd
import qualified System.Environment as Environment
import qualified System.Exit as Exit
 
ggetArgs ::
         T.RefSrcPos -> T.RefExp -> T.R (IO (T.List String))
ggetArgs pgetArgs p = T.uconstUse pgetArgs p sgetArgs
sgetArgs
  = T.uconstDef p agetArgs
      (\ p ->
         T.fromIO (T.fromList T.fromString) p
           Environment.getArgs)
 
ggetProgName ::
             T.RefSrcPos -> T.RefExp -> T.R (IO String)
ggetProgName pgetProgName p
  = T.uconstUse pgetProgName p sgetProgName
sgetProgName
  = T.uconstDef p agetProgName
      (\ p ->
         T.fromIO T.fromString p Environment.getProgName)
 
ggetEnv ::
        T.RefSrcPos ->
          T.RefExp -> T.R (T.Fun String (IO String))
ggetEnv pgetEnv p = T.ufun1 agetEnv pgetEnv p hgetEnv
hgetEnv z1getEnv kgetEnv
  = T.fromIO T.fromString kgetEnv
      (Environment.getEnv (T.toString kgetEnv z1getEnv))
 
gsystem ::
        T.RefSrcPos ->
          T.RefExp -> T.R (T.Fun String (IO ExitCode))
gsystem psystem p = T.ufun1 asystem psystem p hsystem
hsystem z1system ksystem
  = T.fromIO T.fromExitCode ksystem
      (Cmd.system (T.toString ksystem z1system))
 
gexitWith ::
          T.RefSrcPos ->
            T.RefExp -> T.R (T.Fun ExitCode (IO a))
gexitWith pexitWith p
  = T.ufun1 aexitWith pexitWith p hexitWith
hexitWith z1exitWith kexitWith
  = T.fromIO T.fromId kexitWith
      (Exit.exitWith (T.toExitCode kexitWith z1exitWith))
 
gexitFailure :: T.RefSrcPos -> T.RefExp -> T.R (IO a)
gexitFailure pexitFailure p
  = T.uconstUse pexitFailure p sexitFailure
sexitFailure
  = T.uconstDef p aexitFailure
      (\ p -> T.fromIO T.fromId p Exit.exitFailure)
aexitFailure
  = T.mkVariable tSystem 230001 240015 3 (0)
      "exitFailure"
      Prelude.False
aexitWith
  = T.mkVariable tSystem 210001 220012 3 (1) "exitWith"
      Prelude.False
agetArgs
  = T.mkVariable tSystem 130001 140011 3 (0) "getArgs"
      Prelude.False
agetEnv
  = T.mkVariable tSystem 170001 180010 3 (1) "getEnv"
      Prelude.False
agetProgName
  = T.mkVariable tSystem 150001 160015 3 (0)
      "getProgName"
      Prelude.False
asystem
  = T.mkVariable tSystem 190001 200010 3 (1) "system"
      Prelude.False
p = T.mkRoot
tSystem
  = T.mkModule "System" "System.hs" Prelude.False