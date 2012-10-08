module Hat.SystemBuiltin where

import Hat.Hat as T
import Hat.SystemBuiltinTypes
import Hat.PreludeBuiltinTypes
import qualified System.Exit

toExitCode :: RefExp -> R ExitCode -> System.Exit.ExitCode
toExitCode h (R ExitSuccess _) = System.Exit.ExitSuccess
toExitCode h (R (ExitFailure rnum) _) = System.Exit.ExitFailure (toInt h rnum)

fromExitCode :: RefExp -> System.Exit.ExitCode -> R ExitCode
fromExitCode h System.Exit.ExitSuccess = 
  T.con0 mkNoSrcPos h ExitSuccess aExitSuccess
fromExitCode h (System.Exit.ExitFailure num) =
  T.con1 mkNoSrcPos h ExitFailure aExitFailure 
    (T.wrapForward h (fromInt h num))
