module Hat.IOBuiltinTypes
       (IOMode(..), BufferMode(..), SeekMode(..),
        aAbsoluteSeek, aAppendMode, aBlockBuffering,
        aLineBuffering, aNoBuffering, aReadMode,
        aReadWriteMode, aRelativeSeek, aSeekFromEnd,
        aWriteMode, (++=%$=+=%%==), (+>=%$=>=%%==),
        (+##=%$=##=%%==))
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Ix (Ix(..))
 
data IOMode = ReadMode
            | WriteMode
            | AppendMode
            | ReadWriteMode
 
instance T.WrapVal IOMode where
        wrapVal pwrapVal kwrapVal@ReadMode p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aReadMode)
        wrapVal pwrapVal kwrapVal@WriteMode p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aWriteMode)
        wrapVal pwrapVal kwrapVal@AppendMode p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aAppendMode)
        wrapVal pwrapVal kwrapVal@ReadWriteMode p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aReadWriteMode)
 
instance Eq IOMode where
        (%==) !== p = T.ufun2 (++=%$=+=%%==) (%==) p (*==)
          where (T.R ReadMode _ *== T.R ReadMode _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R WriteMode _ *== T.R WriteMode _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R AppendMode _ *== T.R AppendMode _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ReadWriteMode _ *== T.R ReadWriteMode _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord IOMode where
        gcompare pcompare p
          = T.ufun2 c6v36v6v38compare pcompare p hcompare
          where hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c6v36v6v38localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c6v36v6v38localFromEnum
                        hlocalFromEnum (T.R ReadMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R WriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R AppendMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ReadWriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Ix IOMode where
        grange prange p
          = T.ufun1 c6v41v6v42range prange p hrange
          where hrange (T.R (T.Tuple2 fy1 fy2) _) p
                  = T.uwrapForward p
                      (hmap (grtoEnum T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                            (T.uwrapForward p (hrfromEnum fy1 p))
                            (T.uwrapForward p (hrfromEnum fy2 p)))
                         p)
                  where  
                        grtoEnum ::
                                 T.RefSrcPos ->
                                   T.RefExp -> T.R (T.Fun Int IOMode)
                         
                        hrtoEnum :: T.R Int -> T.RefExp -> T.R IOMode
                        grtoEnum prtoEnum p
                          = T.ufun1 c6v41v6v42rtoEnum prtoEnum p hrtoEnum
                        artoEnum = c6v41v6v42rtoEnum
                        hrtoEnum fv6v41v6v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv6v41v6v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h6v41v6v42n p)
                              (y1rtoEnum fv6v41v6v42n p)
                          where h6v41v6v42n p
                                  = T.con0 T.mkNoSrcPos p ReadMode aReadMode
                                h6v41v6v42n p = y1rtoEnum fv6v41v6v42n p
                        hrtoEnum fv6v41v6v42n p = y1rtoEnum fv6v41v6v42n p
                        y1rtoEnum fv6v41v6v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv6v41v6v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (1))))
                              (h6v41v6v42n p)
                              (y2rtoEnum fv6v41v6v42n p)
                          where h6v41v6v42n p
                                  = T.con0 T.mkNoSrcPos p WriteMode aWriteMode
                                h6v41v6v42n p = y2rtoEnum fv6v41v6v42n p
                        y1rtoEnum fv6v41v6v42n p = y2rtoEnum fv6v41v6v42n p
                        y2rtoEnum fv6v41v6v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv6v41v6v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (2))))
                              (h6v41v6v42n p)
                              (y3rtoEnum fv6v41v6v42n p)
                          where h6v41v6v42n p
                                  = T.con0 T.mkNoSrcPos p AppendMode aAppendMode
                                h6v41v6v42n p = y3rtoEnum fv6v41v6v42n p
                        y2rtoEnum fv6v41v6v42n p = y3rtoEnum fv6v41v6v42n p
                        y3rtoEnum fv6v41v6v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv6v41v6v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (3))))
                              (h6v41v6v42n p)
                              (T.fatal p)
                          where h6v41v6v42n p
                                  = T.con0 T.mkNoSrcPos p ReadWriteMode
                                      aReadWriteMode
                                h6v41v6v42n p = T.fatal p
                        y3rtoEnum _ p = T.fatal p
                         
                        grfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun IOMode Int)
                         
                        hrfromEnum :: T.R IOMode -> T.RefExp -> T.R Int
                        grfromEnum prfromEnum p
                          = T.ufun1 c6v41v6v42rfromEnum prfromEnum p hrfromEnum
                        arfromEnum = c6v41v6v42rfromEnum
                        hrfromEnum (T.R ReadMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hrfromEnum (T.R WriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hrfromEnum (T.R AppendMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hrfromEnum (T.R ReadWriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hrfromEnum _ p = T.fatal p
        gindex pindex p
          = T.ufun2 c6v41v6v42index pindex p hindex
          where hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                      (T.uwrapForward p (hifromEnum fy3 p))
                      (T.uwrapForward p (hifromEnum fy2 p))
                  where  
                        gifromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun IOMode Int)
                         
                        hifromEnum :: T.R IOMode -> T.RefExp -> T.R Int
                        gifromEnum pifromEnum p
                          = T.ufun1 c6v41v6v42ifromEnum pifromEnum p hifromEnum
                        aifromEnum = c6v41v6v42ifromEnum
                        hifromEnum (T.R ReadMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hifromEnum (T.R WriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hifromEnum (T.R AppendMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hifromEnum (T.R ReadWriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hifromEnum _ p = T.fatal p
        ginRange pinRange p
          = T.ufun2 c6v41v6v42inRange pinRange p hinRange
          where hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uwrapForward p (hnfromEnum fy1 p))
                         (T.uwrapForward p (hnfromEnum fy2 p)))
                      (T.uwrapForward p (hnfromEnum fy3 p))
                  where  
                        gnfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun IOMode Int)
                         
                        hnfromEnum :: T.R IOMode -> T.RefExp -> T.R Int
                        gnfromEnum pnfromEnum p
                          = T.ufun1 c6v41v6v42nfromEnum pnfromEnum p hnfromEnum
                        anfromEnum = c6v41v6v42nfromEnum
                        hnfromEnum (T.R ReadMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hnfromEnum (T.R WriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hnfromEnum (T.R AppendMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hnfromEnum (T.R ReadWriteMode _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hnfromEnum _ p = T.fatal p
 
instance Bounded IOMode where
        gminBound pminBound p
          = T.uconstUse pminBound p sminBound
        sminBound
          = T.uconstDef p c6v45v6v51minBound
              (\ p -> T.con0 T.mkNoSrcPos p ReadMode aReadMode)
        gmaxBound pmaxBound p
          = T.uconstUse pmaxBound p smaxBound
        smaxBound
          = T.uconstDef p c6v45v6v51maxBound
              (\ p ->
                 T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
 
instance Enum IOMode where
        gfromEnum pfromEnum p
          = T.ufun1 c6v54v6v57fromEnum pfromEnum p hfromEnum
          where hfromEnum (T.R ReadMode _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                hfromEnum (T.R WriteMode _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))
                hfromEnum (T.R AppendMode _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (2))
                hfromEnum (T.R ReadWriteMode _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (3))
                hfromEnum _ p = T.fatal p
        gtoEnum ptoEnum p
          = T.ufun1 c6v54v6v57toEnum ptoEnum p htoEnum
          where htoEnum fv6v54v6v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv6v54v6v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h6v54v6v57n p)
                      (y1toEnum fv6v54v6v57n p)
                  where h6v54v6v57n p
                          = T.con0 T.mkNoSrcPos p ReadMode aReadMode
                        h6v54v6v57n p = y1toEnum fv6v54v6v57n p
                htoEnum fv6v54v6v57n p = y1toEnum fv6v54v6v57n p
                y1toEnum fv6v54v6v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv6v54v6v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      (h6v54v6v57n p)
                      (y2toEnum fv6v54v6v57n p)
                  where h6v54v6v57n p
                          = T.con0 T.mkNoSrcPos p WriteMode aWriteMode
                        h6v54v6v57n p = y2toEnum fv6v54v6v57n p
                y1toEnum fv6v54v6v57n p = y2toEnum fv6v54v6v57n p
                y2toEnum fv6v54v6v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv6v54v6v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (2))))
                      (h6v54v6v57n p)
                      (y3toEnum fv6v54v6v57n p)
                  where h6v54v6v57n p
                          = T.con0 T.mkNoSrcPos p AppendMode aAppendMode
                        h6v54v6v57n p = y3toEnum fv6v54v6v57n p
                y2toEnum fv6v54v6v57n p = y3toEnum fv6v54v6v57n p
                y3toEnum fv6v54v6v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv6v54v6v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (3))))
                      (h6v54v6v57n p)
                      (y4toEnum fv6v54v6v57n p)
                  where h6v54v6v57n p
                          = T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode
                        h6v54v6v57n p = y4toEnum fv6v54v6v57n p
                y3toEnum fv6v54v6v57n p = y4toEnum fv6v54v6v57n p
                y4toEnum _ p
                  = T.uwrapForward p
                      (herror
                         (T.fromLitString T.mkNoSrcPos p
                            "toEnum: argument out of bounds")
                         p)
        genumFrom penumFrom p
          = T.ufun1 c6v54v6v57enumFrom penumFrom p henumFrom
          where henumFrom fy1 p
                  = T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                      fy1
                      (T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
        genumFromThen penumFromThen p
          = T.ufun2 c6v54v6v57enumFromThen penumFromThen p
              henumFromThen
          where henumFromThen fy1 fy2 p
                  = T.uap3 T.mkNoSrcPos p
                      (genumFromThenTo T.mkNoSrcPos p)
                      fy1
                      fy2
                      (T.ucif p
                         (T.uap2 T.mkNoSrcPos p ((!>=) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy2)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy1))
                         (T.con0 T.mkNoSrcPos p ReadWriteMode aReadWriteMode)
                         (T.con0 T.mkNoSrcPos p ReadMode aReadMode))
 
instance Read IOMode where
        greadsPrec preadsPrec p
          = T.ufun1 c6v60v6v63readsPrec preadsPrec p hreadsPrec
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
                                     (T.con0 T.mkNoSrcPos p ReadMode aReadMode))
                                  (T.fromLitString T.mkNoSrcPos p "ReadMode")
                                  p))
                            p))
                      (T.uap2 T.mkNoSrcPos p
                         (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                         (T.uwrapForward p
                            (hreadParen
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.False
                                  Hat.PreludeBuiltinTypes.aFalse)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p WriteMode
                                           aWriteMode))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "WriteMode")
                                     p))
                               p))
                         (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (hreadParen
                                  (T.con0 T.mkNoSrcPos p
                                     Hat.PreludeBuiltinTypes.False
                                     Hat.PreludeBuiltinTypes.aFalse)
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gyield T.mkNoSrcPos
                                              p)
                                           (T.con0 T.mkNoSrcPos p AppendMode
                                              aAppendMode))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "AppendMode")
                                        p))
                                  p))
                            (T.uwrapForward p
                               (hreadParen
                                  (T.con0 T.mkNoSrcPos p
                                     Hat.PreludeBuiltinTypes.False
                                     Hat.PreludeBuiltinTypes.aFalse)
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gyield T.mkNoSrcPos
                                              p)
                                           (T.con0 T.mkNoSrcPos p ReadWriteMode
                                              aReadWriteMode))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "ReadWriteMode")
                                        p))
                                  p))))
 
instance Show IOMode where
        gshowsPrec pshowsPrec p
          = T.ufun2 c6v66v6v69showsPrec pshowsPrec p hshowsPrec
          where hshowsPrec fy1 (T.R ReadMode _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ReadMode")
                hshowsPrec fy1 (T.R WriteMode _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "WriteMode")
                hshowsPrec fy1 (T.R AppendMode _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "AppendMode")
                hshowsPrec fy1 (T.R ReadWriteMode _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ReadWriteMode")
                hshowsPrec _ _ p = T.fatal p
 
data BufferMode = NoBuffering
                | LineBuffering
                | BlockBuffering (T.R (Maybe Int))
 
instance T.WrapVal BufferMode where
        wrapVal pwrapVal kwrapVal@NoBuffering p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aNoBuffering)
        wrapVal pwrapVal kwrapVal@LineBuffering p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aLineBuffering)
        wrapVal pwrapVal
          kwrapVal@(BlockBuffering (T.R _ z1wrapVal)) p
          = T.R kwrapVal
              (T.mkValueApp1 p pwrapVal aBlockBuffering z1wrapVal)
 
instance Eq BufferMode where
        (%==) !== p = T.ufun2 (+>=%$=>=%%==) (%==) p (*==)
          where (T.R NoBuffering _ *== T.R NoBuffering _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R LineBuffering _ *== T.R LineBuffering _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R (BlockBuffering fy1) _ *==
                   T.R (BlockBuffering fy2) _)
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
 
instance Ord BufferMode where
        gcompare pcompare p
          = T.ufun2 c9v36v9v38compare pcompare p hcompare
          where hcompare (T.R (BlockBuffering fy3) _)
                  (T.R (BlockBuffering fy4) _) p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p) fy3
                      fy4
                hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c9v36v9v38localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c9v36v9v38localFromEnum
                        hlocalFromEnum (T.R NoBuffering _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R LineBuffering _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R (BlockBuffering _) _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Read BufferMode where
        greadsPrec preadsPrec p
          = T.ufun1 c9v41v9v44readsPrec preadsPrec p hreadsPrec
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
                                     (T.con0 T.mkNoSrcPos p NoBuffering
                                        aNoBuffering))
                                  (T.fromLitString T.mkNoSrcPos p "NoBuffering")
                                  p))
                            p))
                      (T.uap2 T.mkNoSrcPos p
                         (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                         (T.uwrapForward p
                            (hreadParen
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.False
                                  Hat.PreludeBuiltinTypes.aFalse)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p LineBuffering
                                           aLineBuffering))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "LineBuffering")
                                     p))
                               p))
                         (T.uwrapForward p
                            (hreadParen
                               (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                                  (T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                        p)
                                     (T.conInteger T.mkNoSrcPos p (9))))
                               (T.uap2 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gyield T.mkNoSrcPos
                                              p)
                                           (T.pa0 BlockBuffering T.cn1
                                              T.mkNoSrcPos
                                              p
                                              aBlockBuffering))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "BlockBuffering")
                                        p))
                                  (T.uap1 T.mkNoSrcPos p
                                     (greadsPrec T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gfromInteger
                                           T.mkNoSrcPos
                                           p)
                                        (T.conInteger T.mkNoSrcPos p (10)))))
                               p)))
 
instance Show BufferMode where
        gshowsPrec pshowsPrec p
          = T.ufun2 c9v47v9v50showsPrec pshowsPrec p hshowsPrec
          where hshowsPrec fy1 (T.R NoBuffering _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "NoBuffering")
                hshowsPrec fy1 (T.R LineBuffering _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "LineBuffering")
                hshowsPrec fy1 (T.R (BlockBuffering fy2) _) p
                  = T.uwrapForward p
                      (hshowParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (9))))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                               (T.fromLitString T.mkNoSrcPos p
                                  "BlockBuffering "))
                            (T.uap2 T.mkNoSrcPos p (gshowsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (10)))
                               fy2))
                         p)
                hshowsPrec _ _ p = T.fatal p
 
data SeekMode = AbsoluteSeek
              | RelativeSeek
              | SeekFromEnd
 
instance T.WrapVal SeekMode where
        wrapVal pwrapVal kwrapVal@AbsoluteSeek p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aAbsoluteSeek)
        wrapVal pwrapVal kwrapVal@RelativeSeek p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aRelativeSeek)
        wrapVal pwrapVal kwrapVal@SeekFromEnd p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSeekFromEnd)
 
instance Eq SeekMode where
        (%==) !== p = T.ufun2 (+##=%$=##=%%==) (%==) p (*==)
          where (T.R AbsoluteSeek _ *== T.R AbsoluteSeek _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R RelativeSeek _ *== T.R RelativeSeek _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R SeekFromEnd _ *== T.R SeekFromEnd _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord SeekMode where
        gcompare pcompare p
          = T.ufun2 c11v36v11v38compare pcompare p hcompare
          where hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c11v36v11v38localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c11v36v11v38localFromEnum
                        hlocalFromEnum (T.R AbsoluteSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R RelativeSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R SeekFromEnd _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Ix SeekMode where
        grange prange p
          = T.ufun1 c11v41v11v42range prange p hrange
          where hrange (T.R (T.Tuple2 fy1 fy2) _) p
                  = T.uwrapForward p
                      (hmap (grtoEnum T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                            (T.uwrapForward p (hrfromEnum fy1 p))
                            (T.uwrapForward p (hrfromEnum fy2 p)))
                         p)
                  where  
                        grtoEnum ::
                                 T.RefSrcPos ->
                                   T.RefExp -> T.R (T.Fun Int SeekMode)
                         
                        hrtoEnum :: T.R Int -> T.RefExp -> T.R SeekMode
                        grtoEnum prtoEnum p
                          = T.ufun1 c11v41v11v42rtoEnum prtoEnum p hrtoEnum
                        artoEnum = c11v41v11v42rtoEnum
                        hrtoEnum fv11v41v11v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv11v41v11v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h11v41v11v42n p)
                              (y1rtoEnum fv11v41v11v42n p)
                          where h11v41v11v42n p
                                  = T.con0 T.mkNoSrcPos p AbsoluteSeek
                                      aAbsoluteSeek
                                h11v41v11v42n p = y1rtoEnum fv11v41v11v42n p
                        hrtoEnum fv11v41v11v42n p
                          = y1rtoEnum fv11v41v11v42n p
                        y1rtoEnum fv11v41v11v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv11v41v11v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (1))))
                              (h11v41v11v42n p)
                              (y2rtoEnum fv11v41v11v42n p)
                          where h11v41v11v42n p
                                  = T.con0 T.mkNoSrcPos p RelativeSeek
                                      aRelativeSeek
                                h11v41v11v42n p = y2rtoEnum fv11v41v11v42n p
                        y1rtoEnum fv11v41v11v42n p
                          = y2rtoEnum fv11v41v11v42n p
                        y2rtoEnum fv11v41v11v42n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv11v41v11v42n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (2))))
                              (h11v41v11v42n p)
                              (T.fatal p)
                          where h11v41v11v42n p
                                  = T.con0 T.mkNoSrcPos p SeekFromEnd
                                      aSeekFromEnd
                                h11v41v11v42n p = T.fatal p
                        y2rtoEnum _ p = T.fatal p
                         
                        grfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun SeekMode Int)
                         
                        hrfromEnum :: T.R SeekMode -> T.RefExp -> T.R Int
                        grfromEnum prfromEnum p
                          = T.ufun1 c11v41v11v42rfromEnum prfromEnum p
                              hrfromEnum
                        arfromEnum = c11v41v11v42rfromEnum
                        hrfromEnum (T.R AbsoluteSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hrfromEnum (T.R RelativeSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hrfromEnum (T.R SeekFromEnd _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hrfromEnum _ p = T.fatal p
        gindex pindex p
          = T.ufun2 c11v41v11v42index pindex p hindex
          where hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                      (T.uwrapForward p (hifromEnum fy3 p))
                      (T.uwrapForward p (hifromEnum fy2 p))
                  where  
                        gifromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun SeekMode Int)
                         
                        hifromEnum :: T.R SeekMode -> T.RefExp -> T.R Int
                        gifromEnum pifromEnum p
                          = T.ufun1 c11v41v11v42ifromEnum pifromEnum p
                              hifromEnum
                        aifromEnum = c11v41v11v42ifromEnum
                        hifromEnum (T.R AbsoluteSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hifromEnum (T.R RelativeSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hifromEnum (T.R SeekFromEnd _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hifromEnum _ p = T.fatal p
        ginRange pinRange p
          = T.ufun2 c11v41v11v42inRange pinRange p hinRange
          where hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uwrapForward p (hnfromEnum fy1 p))
                         (T.uwrapForward p (hnfromEnum fy2 p)))
                      (T.uwrapForward p (hnfromEnum fy3 p))
                  where  
                        gnfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun SeekMode Int)
                         
                        hnfromEnum :: T.R SeekMode -> T.RefExp -> T.R Int
                        gnfromEnum pnfromEnum p
                          = T.ufun1 c11v41v11v42nfromEnum pnfromEnum p
                              hnfromEnum
                        anfromEnum = c11v41v11v42nfromEnum
                        hnfromEnum (T.R AbsoluteSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hnfromEnum (T.R RelativeSeek _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hnfromEnum (T.R SeekFromEnd _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hnfromEnum _ p = T.fatal p
 
instance Bounded SeekMode where
        gminBound pminBound p
          = T.uconstUse pminBound p sminBound
        sminBound
          = T.uconstDef p c11v45v11v51minBound
              (\ p ->
                 T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek)
        gmaxBound pmaxBound p
          = T.uconstUse pmaxBound p smaxBound
        smaxBound
          = T.uconstDef p c11v45v11v51maxBound
              (\ p ->
                 T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
 
instance Enum SeekMode where
        gfromEnum pfromEnum p
          = T.ufun1 c11v54v11v57fromEnum pfromEnum p hfromEnum
          where hfromEnum (T.R AbsoluteSeek _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                hfromEnum (T.R RelativeSeek _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))
                hfromEnum (T.R SeekFromEnd _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (2))
                hfromEnum _ p = T.fatal p
        gtoEnum ptoEnum p
          = T.ufun1 c11v54v11v57toEnum ptoEnum p htoEnum
          where htoEnum fv11v54v11v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv11v54v11v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h11v54v11v57n p)
                      (y1toEnum fv11v54v11v57n p)
                  where h11v54v11v57n p
                          = T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek
                        h11v54v11v57n p = y1toEnum fv11v54v11v57n p
                htoEnum fv11v54v11v57n p = y1toEnum fv11v54v11v57n p
                y1toEnum fv11v54v11v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv11v54v11v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      (h11v54v11v57n p)
                      (y2toEnum fv11v54v11v57n p)
                  where h11v54v11v57n p
                          = T.con0 T.mkNoSrcPos p RelativeSeek aRelativeSeek
                        h11v54v11v57n p = y2toEnum fv11v54v11v57n p
                y1toEnum fv11v54v11v57n p = y2toEnum fv11v54v11v57n p
                y2toEnum fv11v54v11v57n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv11v54v11v57n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (2))))
                      (h11v54v11v57n p)
                      (y3toEnum fv11v54v11v57n p)
                  where h11v54v11v57n p
                          = T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd
                        h11v54v11v57n p = y3toEnum fv11v54v11v57n p
                y2toEnum fv11v54v11v57n p = y3toEnum fv11v54v11v57n p
                y3toEnum _ p
                  = T.uwrapForward p
                      (herror
                         (T.fromLitString T.mkNoSrcPos p
                            "toEnum: argument out of bounds")
                         p)
        genumFrom penumFrom p
          = T.ufun1 c11v54v11v57enumFrom penumFrom p henumFrom
          where henumFrom fy1 p
                  = T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                      fy1
                      (T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
        genumFromThen penumFromThen p
          = T.ufun2 c11v54v11v57enumFromThen penumFromThen p
              henumFromThen
          where henumFromThen fy1 fy2 p
                  = T.uap3 T.mkNoSrcPos p
                      (genumFromThenTo T.mkNoSrcPos p)
                      fy1
                      fy2
                      (T.ucif p
                         (T.uap2 T.mkNoSrcPos p ((!>=) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy2)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy1))
                         (T.con0 T.mkNoSrcPos p SeekFromEnd aSeekFromEnd)
                         (T.con0 T.mkNoSrcPos p AbsoluteSeek aAbsoluteSeek))
 
instance Read SeekMode where
        greadsPrec preadsPrec p
          = T.ufun1 c11v60v11v63readsPrec preadsPrec p
              hreadsPrec
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
                                     (T.con0 T.mkNoSrcPos p AbsoluteSeek
                                        aAbsoluteSeek))
                                  (T.fromLitString T.mkNoSrcPos p
                                     "AbsoluteSeek")
                                  p))
                            p))
                      (T.uap2 T.mkNoSrcPos p
                         (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                         (T.uwrapForward p
                            (hreadParen
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.False
                                  Hat.PreludeBuiltinTypes.aFalse)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p RelativeSeek
                                           aRelativeSeek))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "RelativeSeek")
                                     p))
                               p))
                         (T.uwrapForward p
                            (hreadParen
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.False
                                  Hat.PreludeBuiltinTypes.aFalse)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p SeekFromEnd
                                           aSeekFromEnd))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "SeekFromEnd")
                                     p))
                               p)))
 
instance Show SeekMode where
        gshowsPrec pshowsPrec p
          = T.ufun2 c11v66v11v69showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fy1 (T.R AbsoluteSeek _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "AbsoluteSeek")
                hshowsPrec fy1 (T.R RelativeSeek _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "RelativeSeek")
                hshowsPrec fy1 (T.R SeekFromEnd _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "SeekFromEnd")
                hshowsPrec _ _ p = T.fatal p
aAbsoluteSeek
  = T.mkConstructor tIOBuiltinTypes 100021 100032 3 (0)
      "AbsoluteSeek"
aAppendMode
  = T.mkConstructor tIOBuiltinTypes 50044 50053 3 (0)
      "AppendMode"
aBlockBuffering
  = T.mkConstructor tIOBuiltinTypes 80022 80035 3 (1)
      "BlockBuffering"
aLineBuffering
  = T.mkConstructor tIOBuiltinTypes 70035 70047 3 (0)
      "LineBuffering"
aNoBuffering
  = T.mkConstructor tIOBuiltinTypes 70021 70031 3 (0)
      "NoBuffering"
aReadMode
  = T.mkConstructor tIOBuiltinTypes 50021 50028 3 (0)
      "ReadMode"
aReadWriteMode
  = T.mkConstructor tIOBuiltinTypes 50057 50069 3 (0)
      "ReadWriteMode"
aRelativeSeek
  = T.mkConstructor tIOBuiltinTypes 100036 100047 3 (0)
      "RelativeSeek"
aSeekFromEnd
  = T.mkConstructor tIOBuiltinTypes 100051 100061 3 (0)
      "SeekFromEnd"
aWriteMode
  = T.mkConstructor tIOBuiltinTypes 50032 50040 3 (0)
      "WriteMode"
(++=%$=+=%%==)
  = T.mkVariable tIOBuiltinTypes 60032 60033 3 (-1)
      "=="
      Prelude.False
c6v36v6v38localFromEnum
  = T.mkVariable tIOBuiltinTypes 60036 60038 3 (1)
      "localFromEnum"
      Prelude.True
c6v36v6v38compare
  = T.mkVariable tIOBuiltinTypes 60036 60038 3 (-1)
      "compare"
      Prelude.False
c6v41v6v42rfromEnum
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (1)
      "rfromEnum"
      Prelude.True
c6v41v6v42rtoEnum
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (1)
      "rtoEnum"
      Prelude.True
c6v41v6v42ifromEnum
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (1)
      "ifromEnum"
      Prelude.True
c6v41v6v42nfromEnum
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (1)
      "nfromEnum"
      Prelude.True
c6v41v6v42inRange
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (-1)
      "inRange"
      Prelude.False
c6v41v6v42index
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (-1)
      "index"
      Prelude.False
c6v41v6v42range
  = T.mkVariable tIOBuiltinTypes 60041 60042 3 (-1)
      "range"
      Prelude.False
c6v45v6v51maxBound
  = T.mkVariable tIOBuiltinTypes 60045 60051 3 (-1)
      "maxBound"
      Prelude.False
c6v45v6v51minBound
  = T.mkVariable tIOBuiltinTypes 60045 60051 3 (-1)
      "minBound"
      Prelude.False
c6v54v6v57enumFrom
  = T.mkVariable tIOBuiltinTypes 60054 60057 3 (-1)
      "enumFrom"
      Prelude.False
c6v54v6v57enumFromThen
  = T.mkVariable tIOBuiltinTypes 60054 60057 3 (-1)
      "enumFromThen"
      Prelude.False
c6v54v6v57fromEnum
  = T.mkVariable tIOBuiltinTypes 60054 60057 3 (-1)
      "fromEnum"
      Prelude.False
c6v54v6v57toEnum
  = T.mkVariable tIOBuiltinTypes 60054 60057 3 (-1)
      "toEnum"
      Prelude.False
c6v60v6v63readsPrec
  = T.mkVariable tIOBuiltinTypes 60060 60063 3 (-1)
      "readsPrec"
      Prelude.False
c6v66v6v69showsPrec
  = T.mkVariable tIOBuiltinTypes 60066 60069 3 (-1)
      "showsPrec"
      Prelude.False
(+>=%$=>=%%==)
  = T.mkVariable tIOBuiltinTypes 90032 90033 3 (-1)
      "=="
      Prelude.False
c9v36v9v38localFromEnum
  = T.mkVariable tIOBuiltinTypes 90036 90038 3 (1)
      "localFromEnum"
      Prelude.True
c9v36v9v38compare
  = T.mkVariable tIOBuiltinTypes 90036 90038 3 (-1)
      "compare"
      Prelude.False
c9v41v9v44readsPrec
  = T.mkVariable tIOBuiltinTypes 90041 90044 3 (-1)
      "readsPrec"
      Prelude.False
c9v47v9v50showsPrec
  = T.mkVariable tIOBuiltinTypes 90047 90050 3 (-1)
      "showsPrec"
      Prelude.False
(+##=%$=##=%%==)
  = T.mkVariable tIOBuiltinTypes 110032 110033 3 (-1)
      "=="
      Prelude.False
c11v36v11v38localFromEnum
  = T.mkVariable tIOBuiltinTypes 110036 110038 3 (1)
      "localFromEnum"
      Prelude.True
c11v36v11v38compare
  = T.mkVariable tIOBuiltinTypes 110036 110038 3 (-1)
      "compare"
      Prelude.False
c11v41v11v42rfromEnum
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (1)
      "rfromEnum"
      Prelude.True
c11v41v11v42rtoEnum
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (1)
      "rtoEnum"
      Prelude.True
c11v41v11v42ifromEnum
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (1)
      "ifromEnum"
      Prelude.True
c11v41v11v42nfromEnum
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (1)
      "nfromEnum"
      Prelude.True
c11v41v11v42inRange
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (-1)
      "inRange"
      Prelude.False
c11v41v11v42index
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (-1)
      "index"
      Prelude.False
c11v41v11v42range
  = T.mkVariable tIOBuiltinTypes 110041 110042 3 (-1)
      "range"
      Prelude.False
c11v45v11v51maxBound
  = T.mkVariable tIOBuiltinTypes 110045 110051 3 (-1)
      "maxBound"
      Prelude.False
c11v45v11v51minBound
  = T.mkVariable tIOBuiltinTypes 110045 110051 3 (-1)
      "minBound"
      Prelude.False
c11v54v11v57enumFrom
  = T.mkVariable tIOBuiltinTypes 110054 110057 3 (-1)
      "enumFrom"
      Prelude.False
c11v54v11v57enumFromThen
  = T.mkVariable tIOBuiltinTypes 110054 110057 3 (-1)
      "enumFromThen"
      Prelude.False
c11v54v11v57fromEnum
  = T.mkVariable tIOBuiltinTypes 110054 110057 3 (-1)
      "fromEnum"
      Prelude.False
c11v54v11v57toEnum
  = T.mkVariable tIOBuiltinTypes 110054 110057 3 (-1)
      "toEnum"
      Prelude.False
c11v60v11v63readsPrec
  = T.mkVariable tIOBuiltinTypes 110060 110063 3 (-1)
      "readsPrec"
      Prelude.False
c11v66v11v69showsPrec
  = T.mkVariable tIOBuiltinTypes 110066 110069 3 (-1)
      "showsPrec"
      Prelude.False
p = T.mkRoot
tIOBuiltinTypes
  = T.mkModule "IOBuiltinTypes" "IOBuiltinTypes.hs"
      Prelude.False