module Hat.SystemBuiltinTypes
       (ExitCode(..), aExitFailure, aExitSuccess,
        (+&=$^=&=$@==))
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
 
data ExitCode = ExitSuccess
              | ExitFailure (T.R Int)
 
instance T.WrapVal ExitCode where
        wrapVal pwrapVal kwrapVal@ExitSuccess p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aExitSuccess)
        wrapVal pwrapVal
          kwrapVal@(ExitFailure (T.R _ z1wrapVal)) p
          = T.R kwrapVal
              (T.mkValueApp1 p pwrapVal aExitFailure z1wrapVal)
 
instance Eq ExitCode where
        (%==) !== p = T.ufun2 (+&=$^=&=$@==) (%==) p (*==)
          where (T.R ExitSuccess _ *== T.R ExitSuccess _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R (ExitFailure fy1) _ *== T.R (ExitFailure fy2) _)
                  p
                  = T.uwrapForward p
                      ((Hat.PreludeBasic.*&&)
                         (T.uap2 T.mkNoSrcPos p
                            ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                            fy1
                            fy2)
                         (T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                            Hat.PreludeBuiltinTypes.aTrue)
                         p)
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord ExitCode where
        gcompare pcompare p
          = T.ufun2 c4v31v4v33compare pcompare p hcompare
          where hcompare (T.R (ExitFailure fy3) _)
                  (T.R (ExitFailure fy4) _) p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p) fy3
                      fy4
                hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c4v31v4v33localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c4v31v4v33localFromEnum
                        hlocalFromEnum (T.R ExitSuccess _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R (ExitFailure _) _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Read ExitCode where
        greadsPrec preadsPrec p
          = T.ufun1 c4v36v4v39readsPrec preadsPrec p hreadsPrec
          where hreadsPrec fy1 p
                  = T.uap2 T.mkNoSrcPos p
                      (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                      (T.uwrapForward p
                         (hreadParen
                            (T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                               Hat.PreludeBuiltinTypes.aFalse)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hthenLex
                                  (T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                     (T.con0 T.mkNoSrcPos p ExitSuccess
                                        aExitSuccess))
                                  (T.fromLitString T.mkNoSrcPos p "ExitSuccess")
                                  p))
                            p))
                      (T.uwrapForward p
                         (hreadParen
                            (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (9))))
                            (T.uap2 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.pa0 ExitFailure T.cn1 T.mkNoSrcPos p
                                           aExitFailure))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "ExitFailure")
                                     p))
                               (T.uap1 T.mkNoSrcPos p
                                  (greadsPrec T.mkNoSrcPos p)
                                  (T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                        p)
                                     (T.conInteger T.mkNoSrcPos p (10)))))
                            p))
 
instance Show ExitCode where
        gshowsPrec pshowsPrec p
          = T.ufun2 c4v42v4v45showsPrec pshowsPrec p hshowsPrec
          where hshowsPrec fy1 (T.R ExitSuccess _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ExitSuccess")
                hshowsPrec fy1 (T.R (ExitFailure fy2) _) p
                  = T.uwrapForward p
                      (hshowParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (9))))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                               (T.fromLitString T.mkNoSrcPos p "ExitFailure "))
                            (T.uap2 T.mkNoSrcPos p (gshowsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (10)))
                               fy2))
                         p)
                hshowsPrec _ _ p = T.fatal p
aExitFailure
  = T.mkConstructor tSystemBuiltinTypes 30031 30041 3
      (1)
      "ExitFailure"
aExitSuccess
  = T.mkConstructor tSystemBuiltinTypes 30017 30027 3
      (0)
      "ExitSuccess"
(+&=$^=&=$@==)
  = T.mkVariable tSystemBuiltinTypes 40027 40028 3 (-1)
      "=="
      Prelude.False
c4v31v4v33localFromEnum
  = T.mkVariable tSystemBuiltinTypes 40031 40033 3 (1)
      "localFromEnum"
      Prelude.True
c4v31v4v33compare
  = T.mkVariable tSystemBuiltinTypes 40031 40033 3 (-1)
      "compare"
      Prelude.False
c4v36v4v39readsPrec
  = T.mkVariable tSystemBuiltinTypes 40036 40039 3 (-1)
      "readsPrec"
      Prelude.False
c4v42v4v45showsPrec
  = T.mkVariable tSystemBuiltinTypes 40042 40045 3 (-1)
      "showsPrec"
      Prelude.False
p = T.mkRoot
tSystemBuiltinTypes
  = T.mkModule "SystemBuiltinTypes"
      "SystemBuiltinTypes.hs"
      Prelude.False