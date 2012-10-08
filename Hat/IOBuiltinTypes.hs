module Hat.IOBuiltinTypes
  (IOMode(..),BufferMode(..),SeekMode(..),aReadMode,aWriteMode,aAppendMode
    ,aReadWriteMode,aNoBuffering,aLineBuffering,aBlockBuffering,aAbsoluteSeek
    ,aRelativeSeek,aSeekFromEnd) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Ix  (Ix(..))

data IOMode = ReadMode  | WriteMode  | AppendMode  | ReadWriteMode 

instance T.WrapVal (IOMode)
  where
  
  wrapVal pwrapVal (kwrapVal@ReadMode) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aReadMode)
  wrapVal pwrapVal (kwrapVal@WriteMode) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aWriteMode)
  wrapVal pwrapVal (kwrapVal@AppendMode) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aAppendMode)
  wrapVal pwrapVal (kwrapVal@ReadWriteMode) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aReadWriteMode)
  

instance Eq (IOMode)
  where
  
  (!==) (%==) p =
    T.ufun2 (++=%$=+=%%==) (%==) p (*==)
    where
    
    (*==) (T.R ReadMode _) (T.R ReadMode _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R WriteMode _) (T.R WriteMode _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R AppendMode _) (T.R AppendMode _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ReadWriteMode _) (T.R ReadWriteMode _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (IOMode)
  where
  
  gcompare pcompare p =
    T.ufun2 a6v36v6v38compare pcompare p hcompare
    where
    
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a6v36v6v38localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a6v36v6v38localFromEnum
      
      hlocalFromEnum (T.R (ReadMode) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R (WriteMode) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R (AppendMode) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R (ReadWriteMode) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Ix (IOMode)
  where
  
  grange prange p =
    T.ufun1 a6v41v6v42range prange p hrange
    where
    
    hrange (T.R (T.Tuple2 fy1 fy2) _) p =
      T.uwrapForward p
        (Hat.PreludeBasic.hmap (glocalToEnum T.mkNoSrcPos p)
          (T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p)
            (T.uwrapForward p (hlocalFromEnum fy1 p))
            (T.uwrapForward p (hlocalFromEnum fy2 p))) p)
      where
      
      glocalToEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun Hat.Prelude.Int IOMode)
      
      hlocalToEnum :: (T.R Hat.Prelude.Int) -> T.RefExp -> T.R IOMode
      
      glocalToEnum plocalToEnum p =
        T.ufun1 a6v41v6v42localToEnum plocalToEnum p hlocalToEnum
      
      alocalToEnum = a6v41v6v42localToEnum
      
      hlocalToEnum fv6v41v6v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v41v6v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0))) (h p)
          (y1localToEnum fv6v41v6v42n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ReadMode aReadMode
        h p = y1localToEnum fv6v41v6v42n p
        
      hlocalToEnum fv6v41v6v42n p = y1localToEnum fv6v41v6v42n p
      
      y1localToEnum fv6v41v6v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v41v6v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1))) (h p)
          (y2localToEnum fv6v41v6v42n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p WriteMode aWriteMode
        h p = y2localToEnum fv6v41v6v42n p
        
      y1localToEnum fv6v41v6v42n p = y2localToEnum fv6v41v6v42n p
      
      y2localToEnum fv6v41v6v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v41v6v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 2))) (h p)
          (y3localToEnum fv6v41v6v42n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p AppendMode aAppendMode
        h p = y3localToEnum fv6v41v6v42n p
        
      y2localToEnum fv6v41v6v42n p = y3localToEnum fv6v41v6v42n p
      
      y3localToEnum fv6v41v6v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v41v6v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 3))) (h p) (T.fatal p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode
        h p = T.fatal p
        
      y3localToEnum _ p = T.fatal p
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun IOMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R IOMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a6v41v6v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a6v41v6v42localFromEnum
      
      hlocalFromEnum (T.R ReadMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R WriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R AppendMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ReadWriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum _ p = T.fatal p
      
    hrange _ p = T.fatal p
    
  
  gindex pindex p =
    T.ufun2 a6v41v6v42index pindex p hindex
    where
    
    hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!- p)
        (T.uwrapForward p (hlocalFromEnum fy3 p))
        (T.uwrapForward p (hlocalFromEnum fy1 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun IOMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R IOMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a6v41v6v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a6v41v6v42localFromEnum
      
      hlocalFromEnum (T.R ReadMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R WriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R AppendMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ReadWriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum _ p = T.fatal p
      
    hindex _ _ p = T.fatal p
    
  
  ginRange pinRange p =
    T.ufun2 a6v41v6v42inRange pinRange p hinRange
    where
    
    hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (Hat.Ix.ginRange T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
          (T.uwrapForward p (hlocalFromEnum fy1 p))
          (T.uwrapForward p (hlocalFromEnum fy2 p)))
        (T.uwrapForward p (hlocalFromEnum fy3 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun IOMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R IOMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a6v41v6v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a6v41v6v42localFromEnum
      
      hlocalFromEnum (T.R ReadMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R WriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R AppendMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ReadWriteMode _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum _ p = T.fatal p
      
    hinRange _ _ p = T.fatal p
    
  

instance Bounded (IOMode)
  where
  
  gminBound pminBound p = T.uconstUse pminBound p sminBound
  
  sminBound =
    T.uconstDef T.mkRoot a6v45v6v51minBound
      (\ p -> T.con0 T.mkNoSrcPos p ReadMode aReadMode)
  
  gmaxBound pmaxBound p = T.uconstUse pmaxBound p smaxBound
  
  smaxBound =
    T.uconstDef T.mkRoot a6v45v6v51maxBound
      (\ p -> T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
  

instance Enum (IOMode)
  where
  
  gfromEnum pfromEnum p =
    T.ufun1 a6v54v6v57fromEnum pfromEnum p hfromEnum
    where
    
    hfromEnum (T.R ReadMode _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 0)
    hfromEnum (T.R WriteMode _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 1)
    hfromEnum (T.R AppendMode _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 2)
    hfromEnum (T.R ReadWriteMode _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 3)
    hfromEnum _ p = T.fatal p
    
  
  gtoEnum ptoEnum p =
    T.ufun1 a6v54v6v57toEnum ptoEnum p htoEnum
    where
    
    htoEnum fv6v54v6v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v54v6v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1toEnum fv6v54v6v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ReadMode aReadMode
      h p = y1toEnum fv6v54v6v57n p
      
    htoEnum fv6v54v6v57n p = y1toEnum fv6v54v6v57n p
    
    y1toEnum fv6v54v6v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v54v6v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1))) (h p) (y2toEnum fv6v54v6v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p WriteMode aWriteMode
      h p = y2toEnum fv6v54v6v57n p
      
    y1toEnum fv6v54v6v57n p = y2toEnum fv6v54v6v57n p
    
    y2toEnum fv6v54v6v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v54v6v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2))) (h p) (y3toEnum fv6v54v6v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p AppendMode aAppendMode
      h p = y3toEnum fv6v54v6v57n p
      
    y2toEnum fv6v54v6v57n p = y3toEnum fv6v54v6v57n p
    
    y3toEnum fv6v54v6v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv6v54v6v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 3))) (h p) (y4toEnum fv6v54v6v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode
      h p = y4toEnum fv6v54v6v57n p
      
    y3toEnum fv6v54v6v57n p = y4toEnum fv6v54v6v57n p
    
    y4toEnum _ p =
      T.uwrapForward p
        (Hat.Prelude.herror
          (T.fromLitString T.mkNoSrcPos p "toEnum: argument out of bounds") p)
    
  
  genumFrom penumFrom p =
    T.ufun1 a6v54v6v57enumFrom penumFrom p henumFrom
    where
    
    henumFrom fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p) fy1
        (T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
    
  
  genumFromThen penumFromThen p =
    T.ufun2 a6v54v6v57enumFromThen penumFromThen p henumFromThen
    where
    
    henumFromThen fy1 fy2 p =
      T.uap3 T.mkNoSrcPos p (Hat.Prelude.genumFromThenTo T.mkNoSrcPos p) fy1 fy2
        (T.ucif p
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>= p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy1)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy2))
          (T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
          (T.con0 T.mkNoSrcPos p ReadMode aReadMode))
    
  

instance Read (IOMode)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a6v60v6v63readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                  (T.con0 T.mkNoSrcPos p ReadMode aReadMode))
                (T.fromLitString T.mkNoSrcPos p "ReadMode") p)) p))
        (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.con0 T.mkNoSrcPos p WriteMode aWriteMode))
                  (T.fromLitString T.mkNoSrcPos p "WriteMode") p)) p))
          (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
            (T.uwrapForward p
              (Hat.Prelude.hreadParen
                (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
                (T.uwrapForward p
                  (Hat.PreludeBasic.hthenLex
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                      (T.con0 T.mkNoSrcPos p AppendMode aAppendMode))
                    (T.fromLitString T.mkNoSrcPos p "AppendMode") p)) p))
            (T.uwrapForward p
              (Hat.Prelude.hreadParen
                (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
                (T.uwrapForward p
                  (Hat.PreludeBasic.hthenLex
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                      (T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode))
                    (T.fromLitString T.mkNoSrcPos p "ReadWriteMode") p)) p))))
    
  

instance Show (IOMode)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a6v66v6v69showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (ReadMode) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ReadMode")
    hshowsPrec fy1 (T.R (WriteMode) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "WriteMode")
    hshowsPrec fy1 (T.R (AppendMode) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "AppendMode")
    hshowsPrec fy1 (T.R (ReadWriteMode) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ReadWriteMode")
    hshowsPrec _ _ p = T.fatal p
    
  

data BufferMode =
  NoBuffering  | LineBuffering  | BlockBuffering (T.R (Maybe Int))

instance T.WrapVal (BufferMode)
  where
  
  wrapVal pwrapVal (kwrapVal@NoBuffering) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoBuffering)
  wrapVal pwrapVal (kwrapVal@LineBuffering) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aLineBuffering)
  wrapVal pwrapVal (kwrapVal@(BlockBuffering (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aBlockBuffering z1wrapVal)
  

instance Eq (BufferMode)
  where
  
  (!==) (%==) p =
    T.ufun2 (+>=%$=>=%%==) (%==) p (*==)
    where
    
    (*==) (T.R NoBuffering _) (T.R NoBuffering _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R LineBuffering _) (T.R LineBuffering _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R (BlockBuffering fy1) _) (T.R (BlockBuffering fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (BufferMode)
  where
  
  gcompare pcompare p =
    T.ufun2 a9v36v9v38compare pcompare p hcompare
    where
    
    hcompare (T.R (BlockBuffering fy3) _) (T.R (BlockBuffering fy4) _) p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a9v36v9v38localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a9v36v9v38localFromEnum
      
      hlocalFromEnum (T.R (NoBuffering) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R (LineBuffering) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R (BlockBuffering _) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Read (BufferMode)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a9v41v9v44readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                  (T.con0 T.mkNoSrcPos p NoBuffering aNoBuffering))
                (T.fromLitString T.mkNoSrcPos p "NoBuffering") p)) p))
        (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.con0 T.mkNoSrcPos p LineBuffering aLineBuffering))
                  (T.fromLitString T.mkNoSrcPos p "LineBuffering") p)) p))
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
                      (T.pa0 BlockBuffering T.cn1 T.mkNoSrcPos p
                        aBlockBuffering))
                    (T.fromLitString T.mkNoSrcPos p "BlockBuffering") p))
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)))) p)))
    
  

instance Show (BufferMode)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a9v47v9v50showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (NoBuffering) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "NoBuffering")
    hshowsPrec fy1 (T.R (LineBuffering) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "LineBuffering")
    hshowsPrec fy1 (T.R (BlockBuffering fy2) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
              (T.fromLitString T.mkNoSrcPos p "BlockBuffering "))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy2)) p)
    hshowsPrec _ _ p = T.fatal p
    
  

data SeekMode = AbsoluteSeek  | RelativeSeek  | SeekFromEnd 

instance T.WrapVal (SeekMode)
  where
  
  wrapVal pwrapVal (kwrapVal@AbsoluteSeek) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aAbsoluteSeek)
  wrapVal pwrapVal (kwrapVal@RelativeSeek) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aRelativeSeek)
  wrapVal pwrapVal (kwrapVal@SeekFromEnd) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSeekFromEnd)
  

instance Eq (SeekMode)
  where
  
  (!==) (%==) p =
    T.ufun2 (+##=%$=##=%%==) (%==) p (*==)
    where
    
    (*==) (T.R AbsoluteSeek _) (T.R AbsoluteSeek _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R RelativeSeek _) (T.R RelativeSeek _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R SeekFromEnd _) (T.R SeekFromEnd _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (SeekMode)
  where
  
  gcompare pcompare p =
    T.ufun2 a11v36v11v38compare pcompare p hcompare
    where
    
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a11v36v11v38localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a11v36v11v38localFromEnum
      
      hlocalFromEnum (T.R (AbsoluteSeek) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R (RelativeSeek) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R (SeekFromEnd) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Ix (SeekMode)
  where
  
  grange prange p =
    T.ufun1 a11v41v11v42range prange p hrange
    where
    
    hrange (T.R (T.Tuple2 fy1 fy2) _) p =
      T.uwrapForward p
        (Hat.PreludeBasic.hmap (glocalToEnum T.mkNoSrcPos p)
          (T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p)
            (T.uwrapForward p (hlocalFromEnum fy1 p))
            (T.uwrapForward p (hlocalFromEnum fy2 p))) p)
      where
      
      glocalToEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun Hat.Prelude.Int SeekMode)
      
      hlocalToEnum :: (T.R Hat.Prelude.Int) -> T.RefExp -> T.R SeekMode
      
      glocalToEnum plocalToEnum p =
        T.ufun1 a11v41v11v42localToEnum plocalToEnum p hlocalToEnum
      
      alocalToEnum = a11v41v11v42localToEnum
      
      hlocalToEnum fv11v41v11v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v41v11v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0))) (h p)
          (y1localToEnum fv11v41v11v42n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek
        h p = y1localToEnum fv11v41v11v42n p
        
      hlocalToEnum fv11v41v11v42n p = y1localToEnum fv11v41v11v42n p
      
      y1localToEnum fv11v41v11v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v41v11v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1))) (h p)
          (y2localToEnum fv11v41v11v42n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p RelativeSeek aRelativeSeek
        h p = y2localToEnum fv11v41v11v42n p
        
      y1localToEnum fv11v41v11v42n p = y2localToEnum fv11v41v11v42n p
      
      y2localToEnum fv11v41v11v42n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v41v11v42n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 2))) (h p) (T.fatal p)
        where
        
        h p = T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd
        h p = T.fatal p
        
      y2localToEnum _ p = T.fatal p
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun SeekMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R SeekMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a11v41v11v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a11v41v11v42localFromEnum
      
      hlocalFromEnum (T.R AbsoluteSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R RelativeSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R SeekFromEnd _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum _ p = T.fatal p
      
    hrange _ p = T.fatal p
    
  
  gindex pindex p =
    T.ufun2 a11v41v11v42index pindex p hindex
    where
    
    hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!- p)
        (T.uwrapForward p (hlocalFromEnum fy3 p))
        (T.uwrapForward p (hlocalFromEnum fy1 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun SeekMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R SeekMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a11v41v11v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a11v41v11v42localFromEnum
      
      hlocalFromEnum (T.R AbsoluteSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R RelativeSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R SeekFromEnd _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum _ p = T.fatal p
      
    hindex _ _ p = T.fatal p
    
  
  ginRange pinRange p =
    T.ufun2 a11v41v11v42inRange pinRange p hinRange
    where
    
    hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (Hat.Ix.ginRange T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
          (T.uwrapForward p (hlocalFromEnum fy1 p))
          (T.uwrapForward p (hlocalFromEnum fy2 p)))
        (T.uwrapForward p (hlocalFromEnum fy3 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun SeekMode Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R SeekMode) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a11v41v11v42localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a11v41v11v42localFromEnum
      
      hlocalFromEnum (T.R AbsoluteSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R RelativeSeek _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R SeekFromEnd _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum _ p = T.fatal p
      
    hinRange _ _ p = T.fatal p
    
  

instance Bounded (SeekMode)
  where
  
  gminBound pminBound p = T.uconstUse pminBound p sminBound
  
  sminBound =
    T.uconstDef T.mkRoot a11v45v11v51minBound
      (\ p -> T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek)
  
  gmaxBound pmaxBound p = T.uconstUse pmaxBound p smaxBound
  
  smaxBound =
    T.uconstDef T.mkRoot a11v45v11v51maxBound
      (\ p -> T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
  

instance Enum (SeekMode)
  where
  
  gfromEnum pfromEnum p =
    T.ufun1 a11v54v11v57fromEnum pfromEnum p hfromEnum
    where
    
    hfromEnum (T.R AbsoluteSeek _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 0)
    hfromEnum (T.R RelativeSeek _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 1)
    hfromEnum (T.R SeekFromEnd _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 2)
    hfromEnum _ p = T.fatal p
    
  
  gtoEnum ptoEnum p =
    T.ufun1 a11v54v11v57toEnum ptoEnum p htoEnum
    where
    
    htoEnum fv11v54v11v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v54v11v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1toEnum fv11v54v11v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek
      h p = y1toEnum fv11v54v11v57n p
      
    htoEnum fv11v54v11v57n p = y1toEnum fv11v54v11v57n p
    
    y1toEnum fv11v54v11v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v54v11v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1))) (h p) (y2toEnum fv11v54v11v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p RelativeSeek aRelativeSeek
      h p = y2toEnum fv11v54v11v57n p
      
    y1toEnum fv11v54v11v57n p = y2toEnum fv11v54v11v57n p
    
    y2toEnum fv11v54v11v57n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv11v54v11v57n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2))) (h p) (y3toEnum fv11v54v11v57n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd
      h p = y3toEnum fv11v54v11v57n p
      
    y2toEnum fv11v54v11v57n p = y3toEnum fv11v54v11v57n p
    
    y3toEnum _ p =
      T.uwrapForward p
        (Hat.Prelude.herror
          (T.fromLitString T.mkNoSrcPos p "toEnum: argument out of bounds") p)
    
  
  genumFrom penumFrom p =
    T.ufun1 a11v54v11v57enumFrom penumFrom p henumFrom
    where
    
    henumFrom fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p) fy1
        (T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
    
  
  genumFromThen penumFromThen p =
    T.ufun2 a11v54v11v57enumFromThen penumFromThen p henumFromThen
    where
    
    henumFromThen fy1 fy2 p =
      T.uap3 T.mkNoSrcPos p (Hat.Prelude.genumFromThenTo T.mkNoSrcPos p) fy1 fy2
        (T.ucif p
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>= p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy1)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy2))
          (T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
          (T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek))
    
  

instance Read (SeekMode)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a11v60v11v63readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                  (T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek))
                (T.fromLitString T.mkNoSrcPos p "AbsoluteSeek") p)) p))
        (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.con0 T.mkNoSrcPos p RelativeSeek aRelativeSeek))
                  (T.fromLitString T.mkNoSrcPos p "RelativeSeek") p)) p))
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd))
                  (T.fromLitString T.mkNoSrcPos p "SeekFromEnd") p)) p)))
    
  

instance Show (SeekMode)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a11v66v11v69showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (AbsoluteSeek) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "AbsoluteSeek")
    hshowsPrec fy1 (T.R (RelativeSeek) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "RelativeSeek")
    hshowsPrec fy1 (T.R (SeekFromEnd) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "SeekFromEnd")
    hshowsPrec _ _ p = T.fatal p
    
  

tIOBuiltinTypes = T.mkModule "IOBuiltinTypes" "IOBuiltinTypes.hs" Prelude.False

aReadMode = T.mkConstructor tIOBuiltinTypes 50021 50028 3 0 "ReadMode"

aWriteMode = T.mkConstructor tIOBuiltinTypes 50032 50040 3 0 "WriteMode"

aAppendMode = T.mkConstructor tIOBuiltinTypes 50044 50053 3 0 "AppendMode"

aReadWriteMode = T.mkConstructor tIOBuiltinTypes 50057 50069 3 0 "ReadWriteMode"

aNoBuffering = T.mkConstructor tIOBuiltinTypes 70021 70031 3 0 "NoBuffering"

aLineBuffering = T.mkConstructor tIOBuiltinTypes 70035 70047 3 0 "LineBuffering"

aBlockBuffering =
  T.mkConstructor tIOBuiltinTypes 80022 80035 3 1 "BlockBuffering"

aAbsoluteSeek = T.mkConstructor tIOBuiltinTypes 100021 100032 3 0 "AbsoluteSeek"

aRelativeSeek = T.mkConstructor tIOBuiltinTypes 100036 100047 3 0 "RelativeSeek"

aSeekFromEnd = T.mkConstructor tIOBuiltinTypes 100051 100061 3 0 "SeekFromEnd"

(++=%$=+=%%==) = T.mkVariable tIOBuiltinTypes 60032 60033 3 2 "==" Prelude.False

a6v36v6v38compare =
  T.mkVariable tIOBuiltinTypes 60036 60038 3 2 "compare" Prelude.False

a6v41v6v42range =
  T.mkVariable tIOBuiltinTypes 60041 60042 3 1 "range" Prelude.False

a6v41v6v42index =
  T.mkVariable tIOBuiltinTypes 60041 60042 3 2 "index" Prelude.False

a6v41v6v42inRange =
  T.mkVariable tIOBuiltinTypes 60041 60042 3 2 "inRange" Prelude.False

a6v45v6v51minBound =
  T.mkVariable tIOBuiltinTypes 60045 60051 3 0 "minBound" Prelude.False

a6v45v6v51maxBound =
  T.mkVariable tIOBuiltinTypes 60045 60051 3 0 "maxBound" Prelude.False

a6v54v6v57fromEnum =
  T.mkVariable tIOBuiltinTypes 60054 60057 3 1 "fromEnum" Prelude.False

a6v54v6v57toEnum =
  T.mkVariable tIOBuiltinTypes 60054 60057 3 1 "toEnum" Prelude.False

a6v54v6v57enumFrom =
  T.mkVariable tIOBuiltinTypes 60054 60057 3 1 "enumFrom" Prelude.False

a6v54v6v57enumFromThen =
  T.mkVariable tIOBuiltinTypes 60054 60057 3 2 "enumFromThen" Prelude.False

a6v60v6v63readsPrec =
  T.mkVariable tIOBuiltinTypes 60060 60063 3 1 "readsPrec" Prelude.False

a6v66v6v69showsPrec =
  T.mkVariable tIOBuiltinTypes 60066 60069 3 2 "showsPrec" Prelude.False

(+>=%$=>=%%==) = T.mkVariable tIOBuiltinTypes 90032 90033 3 2 "==" Prelude.False

a9v36v9v38compare =
  T.mkVariable tIOBuiltinTypes 90036 90038 3 2 "compare" Prelude.False

a9v41v9v44readsPrec =
  T.mkVariable tIOBuiltinTypes 90041 90044 3 1 "readsPrec" Prelude.False

a9v47v9v50showsPrec =
  T.mkVariable tIOBuiltinTypes 90047 90050 3 2 "showsPrec" Prelude.False

(+##=%$=##=%%==) =
  T.mkVariable tIOBuiltinTypes 110032 110033 3 2 "==" Prelude.False

a11v36v11v38compare =
  T.mkVariable tIOBuiltinTypes 110036 110038 3 2 "compare" Prelude.False

a11v41v11v42range =
  T.mkVariable tIOBuiltinTypes 110041 110042 3 1 "range" Prelude.False

a11v41v11v42index =
  T.mkVariable tIOBuiltinTypes 110041 110042 3 2 "index" Prelude.False

a11v41v11v42inRange =
  T.mkVariable tIOBuiltinTypes 110041 110042 3 2 "inRange" Prelude.False

a11v45v11v51minBound =
  T.mkVariable tIOBuiltinTypes 110045 110051 3 0 "minBound" Prelude.False

a11v45v11v51maxBound =
  T.mkVariable tIOBuiltinTypes 110045 110051 3 0 "maxBound" Prelude.False

a11v54v11v57fromEnum =
  T.mkVariable tIOBuiltinTypes 110054 110057 3 1 "fromEnum" Prelude.False

a11v54v11v57toEnum =
  T.mkVariable tIOBuiltinTypes 110054 110057 3 1 "toEnum" Prelude.False

a11v54v11v57enumFrom =
  T.mkVariable tIOBuiltinTypes 110054 110057 3 1 "enumFrom" Prelude.False

a11v54v11v57enumFromThen =
  T.mkVariable tIOBuiltinTypes 110054 110057 3 2 "enumFromThen" Prelude.False

a11v60v11v63readsPrec =
  T.mkVariable tIOBuiltinTypes 110060 110063 3 1 "readsPrec" Prelude.False

a11v66v11v69showsPrec =
  T.mkVariable tIOBuiltinTypes 110066 110069 3 2 "showsPrec" Prelude.False

a6v36v6v38localFromEnum =
  T.mkVariable tIOBuiltinTypes 60036 60038 3 1 "localFromEnum" Prelude.True

a6v41v6v42localToEnum =
  T.mkVariable tIOBuiltinTypes 60041 60042 3 1 "localToEnum" Prelude.True

a6v41v6v42localFromEnum =
  T.mkVariable tIOBuiltinTypes 60041 60042 3 1 "localFromEnum" Prelude.True

a9v36v9v38localFromEnum =
  T.mkVariable tIOBuiltinTypes 90036 90038 3 1 "localFromEnum" Prelude.True

a11v36v11v38localFromEnum =
  T.mkVariable tIOBuiltinTypes 110036 110038 3 1 "localFromEnum" Prelude.True

a11v41v11v42localToEnum =
  T.mkVariable tIOBuiltinTypes 110041 110042 3 1 "localToEnum" Prelude.True

a11v41v11v42localFromEnum =
  T.mkVariable tIOBuiltinTypes 110041 110042 3 1 "localFromEnum" Prelude.True
