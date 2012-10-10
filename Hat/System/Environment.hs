module Hat.System.Environment
  (ggetArgs,ggetProgName,ggetEnv,agetEnv,hgetEnv) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBuiltinTypes 
import Hat.IOBuiltin 
import qualified System.Environment 

ggetArgs :: T.RefSrcPos -> T.RefExp -> T.R (IO (T.List String))

ggetArgs pgetArgs p = T.uconstUse pgetArgs p sgetArgs

sgetArgs =
  T.uconstDef T.mkRoot agetArgs
    (\ p -> (T.fromIO (fromList fromString)) p System.Environment.getArgs)

ggetProgName :: T.RefSrcPos -> T.RefExp -> T.R (IO String)

ggetProgName pgetProgName p = T.uconstUse pgetProgName p sgetProgName

sgetProgName =
  T.uconstDef T.mkRoot agetProgName
    (\ p -> (T.fromIO fromString) p System.Environment.getProgName)

ggetEnv :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO String))

ggetEnv pgetEnv p = T.ufun1 agetEnv pgetEnv p hgetEnv

hgetEnv z1getEnv kgetEnv =
  (T.fromIO fromString) kgetEnv
    (System.Environment.getEnv (toString kgetEnv z1getEnv))

tSystem_Environment =
  T.mkModule "System.Environment" "System/Environment.hs" Prelude.False

agetArgs =
  T.mkVariable tSystem_Environment 90001 100015 3 0 "getArgs" Prelude.False

agetProgName =
  T.mkVariable tSystem_Environment 120001 130019 3 0 "getProgName" Prelude.False

agetEnv =
  T.mkVariable tSystem_Environment 150001 160024 3 1 "getEnv" Prelude.False
