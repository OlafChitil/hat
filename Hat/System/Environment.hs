module Hat.System.Environment
       (ggetArgs, ggetProgName, ggetEnv, agetEnv, hgetEnv)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes as T
import Hat.IOBuiltin as T
import qualified System.Environment
 
ggetArgs ::
         T.RefSrcPos -> T.RefExp -> T.R (IO (T.List String))
ggetArgs pgetArgs p = T.uconstUse pgetArgs p sgetArgs
sgetArgs
  = T.uconstDef p agetArgs
      (\ p ->
         T.fromIO (T.fromList T.fromString) p
           System.Environment.getArgs)
 
ggetProgName ::
             T.RefSrcPos -> T.RefExp -> T.R (IO String)
ggetProgName pgetProgName p
  = T.uconstUse pgetProgName p sgetProgName
sgetProgName
  = T.uconstDef p agetProgName
      (\ p ->
         T.fromIO T.fromString p
           System.Environment.getProgName)
 
ggetEnv ::
        T.RefSrcPos ->
          T.RefExp -> T.R (T.Fun String (IO String))
ggetEnv pgetEnv p = T.ufun1 agetEnv pgetEnv p hgetEnv
hgetEnv z1getEnv kgetEnv
  = T.fromIO T.fromString kgetEnv
      (System.Environment.getEnv
         (T.toString kgetEnv z1getEnv))
agetArgs
  = T.mkVariable tEnvironment 90001 100012 3 (0)
      "getArgs"
      Prelude.False
agetEnv
  = T.mkVariable tEnvironment 150001 160011 3 (1)
      "getEnv"
      Prelude.False
agetProgName
  = T.mkVariable tEnvironment 120001 130016 3 (0)
      "getProgName"
      Prelude.False
p = T.mkRoot
tEnvironment
  = T.mkModule "System.Environment"
      "System/Environment.hs"
      Prelude.False