module Hat.System
  (ExitCode(ExitSuccess,ExitFailure),aExitSuccess,aExitFailure,ggetArgs
    ,ggetProgName,ggetEnv,agetEnv,hgetEnv,gsystem,asystem,hsystem,gexitWith
    ,aexitWith,hexitWith,gexitFailure) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBuiltinTypes 
import Hat.SystemBuiltinTypes 
import Hat.SystemBuiltin 
import qualified System.Cmd as Cmd 
import qualified System.Environment as Environment 
import qualified System.Exit as Exit 

ggetArgs :: T.RefSrcPos -> T.RefExp -> T.R (IO (T.List String))

ggetArgs pgetArgs p = T.uconstUse pgetArgs p sgetArgs

sgetArgs =
  T.uconstDef T.mkRoot agetArgs
    (\ p -> (T.fromIO (fromList fromString)) p Environment.getArgs)

ggetProgName :: T.RefSrcPos -> T.RefExp -> T.R (IO String)

ggetProgName pgetProgName p = T.uconstUse pgetProgName p sgetProgName

sgetProgName =
  T.uconstDef T.mkRoot agetProgName
    (\ p -> (T.fromIO fromString) p Environment.getProgName)

ggetEnv :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO String))

ggetEnv pgetEnv p = T.ufun1 agetEnv pgetEnv p hgetEnv

hgetEnv z1getEnv kgetEnv =
  (T.fromIO fromString) kgetEnv (Environment.getEnv (toString kgetEnv z1getEnv))

gsystem :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO ExitCode))

gsystem psystem p = T.ufun1 asystem psystem p hsystem

hsystem z1system ksystem =
  (T.fromIO fromExitCode) ksystem (Cmd.system (toString ksystem z1system))

gexitWith :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun ExitCode (IO a))

gexitWith pexitWith p = T.ufun1 aexitWith pexitWith p hexitWith

hexitWith z1exitWith kexitWith =
  (T.fromIO T.fromId) kexitWith
    (Exit.exitWith (toExitCode kexitWith z1exitWith))

gexitFailure :: T.RefSrcPos -> T.RefExp -> T.R (IO a)

gexitFailure pexitFailure p = T.uconstUse pexitFailure p sexitFailure

sexitFailure =
  T.uconstDef T.mkRoot aexitFailure
    (\ p -> (T.fromIO T.fromId) p Exit.exitFailure)

tSystem = T.mkModule "System" "System.hs" Prelude.False

agetArgs = T.mkVariable tSystem 130001 140014 3 0 "getArgs" Prelude.False

agetProgName =
  T.mkVariable tSystem 150001 160018 3 0 "getProgName" Prelude.False

agetEnv = T.mkVariable tSystem 170001 180023 3 1 "getEnv" Prelude.False

asystem = T.mkVariable tSystem 190001 200023 3 1 "system" Prelude.False

aexitWith = T.mkVariable tSystem 210001 220027 3 1 "exitWith" Prelude.False

aexitFailure =
  T.mkVariable tSystem 230001 240018 3 0 "exitFailure" Prelude.False
