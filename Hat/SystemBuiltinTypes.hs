module Hat.SystemBuiltinTypes (ExitCode(..),aExitSuccess,aExitFailure) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

data ExitCode = ExitSuccess  | ExitFailure (T.R Int)

instance T.WrapVal (ExitCode)
  where
  
  wrapVal pwrapVal (kwrapVal@ExitSuccess) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aExitSuccess)
  wrapVal pwrapVal (kwrapVal@(ExitFailure (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aExitFailure z1wrapVal)
  

instance Eq (ExitCode)
  where
  
  (!==) (%==) p =
    T.ufun2 (+&=$^=&=$@==) (%==) p (*==)
    where
    
    (*==) (T.R ExitSuccess _) (T.R ExitSuccess _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R (ExitFailure fy1) _) (T.R (ExitFailure fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (ExitCode)
  where
  
  gcompare pcompare p =
    T.ufun2 a4v31v4v33compare pcompare p hcompare
    where
    
    hcompare (T.R (ExitFailure fy3) _) (T.R (ExitFailure fy4) _) p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a4v31v4v33localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a4v31v4v33localFromEnum
      
      hlocalFromEnum (T.R (ExitSuccess) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R (ExitFailure _) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Read (ExitCode)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a4v36v4v39readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                  (T.con0 T.mkNoSrcPos p ExitSuccess aExitSuccess))
                (T.fromLitString T.mkNoSrcPos p "ExitSuccess") p)) p))
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 9)))
            (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.pa0 ExitFailure T.cn1 T.mkNoSrcPos p aExitFailure))
                  (T.fromLitString T.mkNoSrcPos p "ExitFailure") p))
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)))) p))
    
  

instance Show (ExitCode)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a4v42v4v45showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (ExitSuccess) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ExitSuccess")
    hshowsPrec fy1 (T.R (ExitFailure fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "ExitFailure "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec _ _ p = T.fatal p
    
  

tSystemBuiltinTypes =
  T.mkModule "SystemBuiltinTypes" "SystemBuiltinTypes.hs" Prelude.True

aExitSuccess = T.mkConstructor tSystemBuiltinTypes 30017 30027 3 0 "ExitSuccess"

aExitFailure = T.mkConstructor tSystemBuiltinTypes 30031 30041 3 1 "ExitFailure"

(+&=$^=&=$@==) =
  T.mkVariable tSystemBuiltinTypes 40027 40028 3 2 "==" Prelude.False

a4v31v4v33compare =
  T.mkVariable tSystemBuiltinTypes 40031 40033 3 2 "compare" Prelude.False

a4v36v4v39readsPrec =
  T.mkVariable tSystemBuiltinTypes 40036 40039 3 1 "readsPrec" Prelude.False

a4v42v4v45showsPrec =
  T.mkVariable tSystemBuiltinTypes 40042 40045 3 2 "showsPrec" Prelude.False

a4v31v4v33localFromEnum =
  T.mkVariable tSystemBuiltinTypes 40031 40033 3 1 "localFromEnum" Prelude.True

p3v17v3v27 = T.mkSrcPos tSystemBuiltinTypes 30017 30027

p3v31v3v41 = T.mkSrcPos tSystemBuiltinTypes 30031 30041

p4v27v4v28 = T.mkSrcPos tSystemBuiltinTypes 40027 40028

p4v31v4v33 = T.mkSrcPos tSystemBuiltinTypes 40031 40033

p4v36v4v39 = T.mkSrcPos tSystemBuiltinTypes 40036 40039

p4v42v4v45 = T.mkSrcPos tSystemBuiltinTypes 40042 40045
