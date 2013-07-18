module Hat.TimeBuiltinTypes
       (Month(..), Day(..), CalendarTime(..), gctYear,
        hctYear, gctMonth, hctMonth, gctDay, hctDay, gctHour,
        hctHour, gctMin, hctMin, gctSec, hctSec, gctPicosec,
        hctPicosec, gctWDay, hctWDay, gctYDay, hctYDay,
        gctTZName, hctTZName, gctTZ, hctTZ, gctIsDST,
        hctIsDST, TimeDiff(..), gtdYear, htdYear, gtdMonth,
        htdMonth, gtdDay, htdDay, gtdHour, htdHour, gtdMin,
        htdMin, gtdSec, htdSec, gtdPicosec, htdPicosec,
        aApril, aAugust, aCalendarTime, aDecember, aFebruary,
        aFriday, aJanuary, aJuly, aJune, aMarch, aMay,
        aMonday, aNovember, aOctober, aSaturday, aSeptember,
        aSunday, aThursday, aTimeDiff, aTuesday, aWednesday,
        actDay, actHour, actIsDST, actMin, actMonth,
        actPicosec, actSec, actTZ, actTZName, actWDay,
        actYDay, actYear, atdDay, atdHour, atdMin, atdMonth,
        atdPicosec, atdSec, atdYear, (+@=$$=@=$%==),
        (+#$=$$=#$=$%==), (+$&=$#=$&=$$==), (+$>=$#=$>=$$==))
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Ix (Ix(..))
 
data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
 
instance T.WrapVal Month where
        wrapVal pwrapVal kwrapVal@January p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aJanuary)
        wrapVal pwrapVal kwrapVal@February p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aFebruary)
        wrapVal pwrapVal kwrapVal@March p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aMarch)
        wrapVal pwrapVal kwrapVal@April p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aApril)
        wrapVal pwrapVal kwrapVal@May p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aMay)
        wrapVal pwrapVal kwrapVal@June p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aJune)
        wrapVal pwrapVal kwrapVal@July p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aJuly)
        wrapVal pwrapVal kwrapVal@August p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aAugust)
        wrapVal pwrapVal kwrapVal@September p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSeptember)
        wrapVal pwrapVal kwrapVal@October p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aOctober)
        wrapVal pwrapVal kwrapVal@November p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aNovember)
        wrapVal pwrapVal kwrapVal@December p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aDecember)
 
instance Eq Month where
        (%==) !== p = T.ufun2 (+@=$$=@=$%==) (%==) p (*==)
          where (T.R January _ *== T.R January _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R February _ *== T.R February _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R March _ *== T.R March _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R April _ *== T.R April _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R May _ *== T.R May _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R June _ *== T.R June _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R July _ *== T.R July _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R August _ *== T.R August _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R September _ *== T.R September _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R October _ *== T.R October _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R November _ *== T.R November _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R December _ *== T.R December _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord Month where
        gcompare pcompare p
          = T.ufun2 c8v26v8v28compare pcompare p hcompare
          where hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c8v26v8v28localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c8v26v8v28localFromEnum
                        hlocalFromEnum (T.R January _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R February _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R March _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R April _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R May _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R June _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R July _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R August _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R September _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R October _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R November _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R December _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Enum Month where
        gfromEnum pfromEnum p
          = T.ufun1 c8v31v8v34fromEnum pfromEnum p hfromEnum
          where hfromEnum (T.R January _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                hfromEnum (T.R February _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))
                hfromEnum (T.R March _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (2))
                hfromEnum (T.R April _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (3))
                hfromEnum (T.R May _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (4))
                hfromEnum (T.R June _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (5))
                hfromEnum (T.R July _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (6))
                hfromEnum (T.R August _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (7))
                hfromEnum (T.R September _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (8))
                hfromEnum (T.R October _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (9))
                hfromEnum (T.R November _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (10))
                hfromEnum (T.R December _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (11))
                hfromEnum _ p = T.fatal p
        gtoEnum ptoEnum p
          = T.ufun1 c8v31v8v34toEnum ptoEnum p htoEnum
          where htoEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h8v31v8v34n p)
                      (y1toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p January aJanuary
                        h8v31v8v34n p = y1toEnum fv8v31v8v34n p
                htoEnum fv8v31v8v34n p = y1toEnum fv8v31v8v34n p
                y1toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      (h8v31v8v34n p)
                      (y2toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p February aFebruary
                        h8v31v8v34n p = y2toEnum fv8v31v8v34n p
                y1toEnum fv8v31v8v34n p = y2toEnum fv8v31v8v34n p
                y2toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (2))))
                      (h8v31v8v34n p)
                      (y3toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p March aMarch
                        h8v31v8v34n p = y3toEnum fv8v31v8v34n p
                y2toEnum fv8v31v8v34n p = y3toEnum fv8v31v8v34n p
                y3toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (3))))
                      (h8v31v8v34n p)
                      (y4toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p April aApril
                        h8v31v8v34n p = y4toEnum fv8v31v8v34n p
                y3toEnum fv8v31v8v34n p = y4toEnum fv8v31v8v34n p
                y4toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (4))))
                      (h8v31v8v34n p)
                      (y5toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p = T.con0 T.mkNoSrcPos p May aMay
                        h8v31v8v34n p = y5toEnum fv8v31v8v34n p
                y4toEnum fv8v31v8v34n p = y5toEnum fv8v31v8v34n p
                y5toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (5))))
                      (h8v31v8v34n p)
                      (y6toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p June aJune
                        h8v31v8v34n p = y6toEnum fv8v31v8v34n p
                y5toEnum fv8v31v8v34n p = y6toEnum fv8v31v8v34n p
                y6toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (6))))
                      (h8v31v8v34n p)
                      (y7toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p July aJuly
                        h8v31v8v34n p = y7toEnum fv8v31v8v34n p
                y6toEnum fv8v31v8v34n p = y7toEnum fv8v31v8v34n p
                y7toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (7))))
                      (h8v31v8v34n p)
                      (y8toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p August aAugust
                        h8v31v8v34n p = y8toEnum fv8v31v8v34n p
                y7toEnum fv8v31v8v34n p = y8toEnum fv8v31v8v34n p
                y8toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (8))))
                      (h8v31v8v34n p)
                      (y9toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p September aSeptember
                        h8v31v8v34n p = y9toEnum fv8v31v8v34n p
                y8toEnum fv8v31v8v34n p = y9toEnum fv8v31v8v34n p
                y9toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (9))))
                      (h8v31v8v34n p)
                      (y10toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p October aOctober
                        h8v31v8v34n p = y10toEnum fv8v31v8v34n p
                y9toEnum fv8v31v8v34n p = y10toEnum fv8v31v8v34n p
                y10toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (10))))
                      (h8v31v8v34n p)
                      (y11toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p November aNovember
                        h8v31v8v34n p = y11toEnum fv8v31v8v34n p
                y10toEnum fv8v31v8v34n p = y11toEnum fv8v31v8v34n p
                y11toEnum fv8v31v8v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv8v31v8v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (11))))
                      (h8v31v8v34n p)
                      (y12toEnum fv8v31v8v34n p)
                  where h8v31v8v34n p
                          = T.con0 T.mkNoSrcPos p December aDecember
                        h8v31v8v34n p = y12toEnum fv8v31v8v34n p
                y11toEnum fv8v31v8v34n p = y12toEnum fv8v31v8v34n p
                y12toEnum _ p
                  = T.uwrapForward p
                      (herror
                         (T.fromLitString T.mkNoSrcPos p
                            "toEnum: argument out of bounds")
                         p)
        genumFrom penumFrom p
          = T.ufun1 c8v31v8v34enumFrom penumFrom p henumFrom
          where henumFrom fy1 p
                  = T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                      fy1
                      (T.con0 T.mkNoSrcPos p December aDecember)
        genumFromThen penumFromThen p
          = T.ufun2 c8v31v8v34enumFromThen penumFromThen p
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
                         (T.con0 T.mkNoSrcPos p December aDecember)
                         (T.con0 T.mkNoSrcPos p January aJanuary))
 
instance Bounded Month where
        gminBound pminBound p
          = T.uconstUse pminBound p sminBound
        sminBound
          = T.uconstDef p c8v37v8v43minBound
              (\ p -> T.con0 T.mkNoSrcPos p January aJanuary)
        gmaxBound pmaxBound p
          = T.uconstUse pmaxBound p smaxBound
        smaxBound
          = T.uconstDef p c8v37v8v43maxBound
              (\ p -> T.con0 T.mkNoSrcPos p December aDecember)
 
instance Ix Month where
        grange prange p
          = T.ufun1 c8v46v8v47range prange p hrange
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
                                   T.RefExp -> T.R (T.Fun Int Month)
                         
                        hrtoEnum :: T.R Int -> T.RefExp -> T.R Month
                        grtoEnum prtoEnum p
                          = T.ufun1 c8v46v8v47rtoEnum prtoEnum p hrtoEnum
                        artoEnum = c8v46v8v47rtoEnum
                        hrtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h8v46v8v47n p)
                              (y1rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p January aJanuary
                                h8v46v8v47n p = y1rtoEnum fv8v46v8v47n p
                        hrtoEnum fv8v46v8v47n p = y1rtoEnum fv8v46v8v47n p
                        y1rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (1))))
                              (h8v46v8v47n p)
                              (y2rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p February aFebruary
                                h8v46v8v47n p = y2rtoEnum fv8v46v8v47n p
                        y1rtoEnum fv8v46v8v47n p = y2rtoEnum fv8v46v8v47n p
                        y2rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (2))))
                              (h8v46v8v47n p)
                              (y3rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p March aMarch
                                h8v46v8v47n p = y3rtoEnum fv8v46v8v47n p
                        y2rtoEnum fv8v46v8v47n p = y3rtoEnum fv8v46v8v47n p
                        y3rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (3))))
                              (h8v46v8v47n p)
                              (y4rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p April aApril
                                h8v46v8v47n p = y4rtoEnum fv8v46v8v47n p
                        y3rtoEnum fv8v46v8v47n p = y4rtoEnum fv8v46v8v47n p
                        y4rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (4))))
                              (h8v46v8v47n p)
                              (y5rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p = T.con0 T.mkNoSrcPos p May aMay
                                h8v46v8v47n p = y5rtoEnum fv8v46v8v47n p
                        y4rtoEnum fv8v46v8v47n p = y5rtoEnum fv8v46v8v47n p
                        y5rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (5))))
                              (h8v46v8v47n p)
                              (y6rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p June aJune
                                h8v46v8v47n p = y6rtoEnum fv8v46v8v47n p
                        y5rtoEnum fv8v46v8v47n p = y6rtoEnum fv8v46v8v47n p
                        y6rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (6))))
                              (h8v46v8v47n p)
                              (y7rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p July aJuly
                                h8v46v8v47n p = y7rtoEnum fv8v46v8v47n p
                        y6rtoEnum fv8v46v8v47n p = y7rtoEnum fv8v46v8v47n p
                        y7rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (7))))
                              (h8v46v8v47n p)
                              (y8rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p August aAugust
                                h8v46v8v47n p = y8rtoEnum fv8v46v8v47n p
                        y7rtoEnum fv8v46v8v47n p = y8rtoEnum fv8v46v8v47n p
                        y8rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (8))))
                              (h8v46v8v47n p)
                              (y9rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p September aSeptember
                                h8v46v8v47n p = y9rtoEnum fv8v46v8v47n p
                        y8rtoEnum fv8v46v8v47n p = y9rtoEnum fv8v46v8v47n p
                        y9rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (9))))
                              (h8v46v8v47n p)
                              (y10rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p October aOctober
                                h8v46v8v47n p = y10rtoEnum fv8v46v8v47n p
                        y9rtoEnum fv8v46v8v47n p = y10rtoEnum fv8v46v8v47n p
                        y10rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (10))))
                              (h8v46v8v47n p)
                              (y11rtoEnum fv8v46v8v47n p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p November aNovember
                                h8v46v8v47n p = y11rtoEnum fv8v46v8v47n p
                        y10rtoEnum fv8v46v8v47n p = y11rtoEnum fv8v46v8v47n p
                        y11rtoEnum fv8v46v8v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv8v46v8v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (11))))
                              (h8v46v8v47n p)
                              (T.fatal p)
                          where h8v46v8v47n p
                                  = T.con0 T.mkNoSrcPos p December aDecember
                                h8v46v8v47n p = T.fatal p
                        y11rtoEnum _ p = T.fatal p
                         
                        grfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Month Int)
                         
                        hrfromEnum :: T.R Month -> T.RefExp -> T.R Int
                        grfromEnum prfromEnum p
                          = T.ufun1 c8v46v8v47rfromEnum prfromEnum p hrfromEnum
                        arfromEnum = c8v46v8v47rfromEnum
                        hrfromEnum (T.R January _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hrfromEnum (T.R February _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hrfromEnum (T.R March _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hrfromEnum (T.R April _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hrfromEnum (T.R May _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hrfromEnum (T.R June _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hrfromEnum (T.R July _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hrfromEnum (T.R August _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hrfromEnum (T.R September _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hrfromEnum (T.R October _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hrfromEnum (T.R November _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hrfromEnum (T.R December _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hrfromEnum _ p = T.fatal p
        gindex pindex p
          = T.ufun2 c8v46v8v47index pindex p hindex
          where hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                      (T.uwrapForward p (hifromEnum fy3 p))
                      (T.uwrapForward p (hifromEnum fy2 p))
                  where  
                        gifromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Month Int)
                         
                        hifromEnum :: T.R Month -> T.RefExp -> T.R Int
                        gifromEnum pifromEnum p
                          = T.ufun1 c8v46v8v47ifromEnum pifromEnum p hifromEnum
                        aifromEnum = c8v46v8v47ifromEnum
                        hifromEnum (T.R January _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hifromEnum (T.R February _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hifromEnum (T.R March _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hifromEnum (T.R April _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hifromEnum (T.R May _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hifromEnum (T.R June _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hifromEnum (T.R July _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hifromEnum (T.R August _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hifromEnum (T.R September _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hifromEnum (T.R October _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hifromEnum (T.R November _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hifromEnum (T.R December _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hifromEnum _ p = T.fatal p
        ginRange pinRange p
          = T.ufun2 c8v46v8v47inRange pinRange p hinRange
          where hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uwrapForward p (hnfromEnum fy1 p))
                         (T.uwrapForward p (hnfromEnum fy2 p)))
                      (T.uwrapForward p (hnfromEnum fy3 p))
                  where  
                        gnfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Month Int)
                         
                        hnfromEnum :: T.R Month -> T.RefExp -> T.R Int
                        gnfromEnum pnfromEnum p
                          = T.ufun1 c8v46v8v47nfromEnum pnfromEnum p hnfromEnum
                        anfromEnum = c8v46v8v47nfromEnum
                        hnfromEnum (T.R January _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hnfromEnum (T.R February _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hnfromEnum (T.R March _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hnfromEnum (T.R April _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hnfromEnum (T.R May _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hnfromEnum (T.R June _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hnfromEnum (T.R July _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hnfromEnum (T.R August _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hnfromEnum (T.R September _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hnfromEnum (T.R October _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hnfromEnum (T.R November _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hnfromEnum (T.R December _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hnfromEnum _ p = T.fatal p
 
instance Read Month where
        greadsPrec preadsPrec p
          = T.ufun1 c8v50v8v53readsPrec preadsPrec p hreadsPrec
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
                                     (T.con0 T.mkNoSrcPos p January aJanuary))
                                  (T.fromLitString T.mkNoSrcPos p "January")
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
                                        (T.con0 T.mkNoSrcPos p February
                                           aFebruary))
                                     (T.fromLitString T.mkNoSrcPos p "February")
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
                                           (T.con0 T.mkNoSrcPos p March aMarch))
                                        (T.fromLitString T.mkNoSrcPos p "March")
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
                                              (Hat.PreludeBasic.gyield
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.con0 T.mkNoSrcPos p April
                                                 aApril))
                                           (T.fromLitString T.mkNoSrcPos p
                                              "April")
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
                                                 (Hat.PreludeBasic.gyield
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.con0 T.mkNoSrcPos p May
                                                    aMay))
                                              (T.fromLitString T.mkNoSrcPos p
                                                 "May")
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
                                                    (Hat.PreludeBasic.gyield
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.con0 T.mkNoSrcPos p June
                                                       aJune))
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "June")
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
                                                       (Hat.PreludeBasic.gyield
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.con0 T.mkNoSrcPos p
                                                          July
                                                          aJuly))
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "July")
                                                    p))
                                              p))
                                        (T.uap2 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.galt T.mkNoSrcPos
                                              p)
                                           (T.uwrapForward p
                                              (hreadParen
                                                 (T.con0 T.mkNoSrcPos p
                                                    Hat.PreludeBuiltinTypes.False
                                                    Hat.PreludeBuiltinTypes.aFalse)
                                                 (T.uwrapForward p
                                                    (Hat.PreludeBasic.hthenLex
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.gyield
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.con0 T.mkNoSrcPos p
                                                             August
                                                             aAugust))
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "August")
                                                       p))
                                                 p))
                                           (T.uap2 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.galt
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.uwrapForward p
                                                 (hreadParen
                                                    (T.con0 T.mkNoSrcPos p
                                                       Hat.PreludeBuiltinTypes.False
                                                       Hat.PreludeBuiltinTypes.aFalse)
                                                    (T.uwrapForward p
                                                       (Hat.PreludeBasic.hthenLex
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (Hat.PreludeBasic.gyield
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.con0
                                                                T.mkNoSrcPos
                                                                p
                                                                September
                                                                aSeptember))
                                                          (T.fromLitString
                                                             T.mkNoSrcPos
                                                             p
                                                             "September")
                                                          p))
                                                    p))
                                              (T.uap2 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.galt
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.uwrapForward p
                                                    (hreadParen
                                                       (T.con0 T.mkNoSrcPos p
                                                          Hat.PreludeBuiltinTypes.False
                                                          Hat.PreludeBuiltinTypes.aFalse)
                                                       (T.uwrapForward p
                                                          (Hat.PreludeBasic.hthenLex
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (Hat.PreludeBasic.gyield
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.con0
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   October
                                                                   aOctober))
                                                             (T.fromLitString
                                                                T.mkNoSrcPos
                                                                p
                                                                "October")
                                                             p))
                                                       p))
                                                 (T.uap2 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.galt
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.uwrapForward p
                                                       (hreadParen
                                                          (T.con0 T.mkNoSrcPos p
                                                             Hat.PreludeBuiltinTypes.False
                                                             Hat.PreludeBuiltinTypes.aFalse)
                                                          (T.uwrapForward p
                                                             (Hat.PreludeBasic.hthenLex
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.gyield
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      November
                                                                      aNovember))
                                                                (T.fromLitString
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   "November")
                                                                p))
                                                          p))
                                                    (T.uwrapForward p
                                                       (hreadParen
                                                          (T.con0 T.mkNoSrcPos p
                                                             Hat.PreludeBuiltinTypes.False
                                                             Hat.PreludeBuiltinTypes.aFalse)
                                                          (T.uwrapForward p
                                                             (Hat.PreludeBasic.hthenLex
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.gyield
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      December
                                                                      aDecember))
                                                                (T.fromLitString
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   "December")
                                                                p))
                                                          p))))))))))))
 
instance Show Month where
        gshowsPrec pshowsPrec p
          = T.ufun2 c8v56v8v59showsPrec pshowsPrec p hshowsPrec
          where hshowsPrec fy1 (T.R January _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "January")
                hshowsPrec fy1 (T.R February _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "February")
                hshowsPrec fy1 (T.R March _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "March")
                hshowsPrec fy1 (T.R April _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "April")
                hshowsPrec fy1 (T.R May _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "May")
                hshowsPrec fy1 (T.R June _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "June")
                hshowsPrec fy1 (T.R July _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "July")
                hshowsPrec fy1 (T.R August _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "August")
                hshowsPrec fy1 (T.R September _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "September")
                hshowsPrec fy1 (T.R October _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "October")
                hshowsPrec fy1 (T.R November _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "November")
                hshowsPrec fy1 (T.R December _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "December")
                hshowsPrec _ _ p = T.fatal p
 
data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
 
instance T.WrapVal Day where
        wrapVal pwrapVal kwrapVal@Sunday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSunday)
        wrapVal pwrapVal kwrapVal@Monday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aMonday)
        wrapVal pwrapVal kwrapVal@Tuesday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aTuesday)
        wrapVal pwrapVal kwrapVal@Wednesday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aWednesday)
        wrapVal pwrapVal kwrapVal@Thursday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aThursday)
        wrapVal pwrapVal kwrapVal@Friday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aFriday)
        wrapVal pwrapVal kwrapVal@Saturday p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSaturday)
 
instance Eq Day where
        (%==) !== p = T.ufun2 (+#$=$$=#$=$%==) (%==) p (*==)
          where (T.R Sunday _ *== T.R Sunday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Monday _ *== T.R Monday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Tuesday _ *== T.R Tuesday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Wednesday _ *== T.R Wednesday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Thursday _ *== T.R Thursday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Friday _ *== T.R Friday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Saturday _ *== T.R Saturday _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord Day where
        gcompare pcompare p
          = T.ufun2 c12v26v12v28compare pcompare p hcompare
          where hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c12v26v12v28localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c12v26v12v28localFromEnum
                        hlocalFromEnum (T.R Sunday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Monday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Tuesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Wednesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Thursday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Friday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Saturday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Enum Day where
        gfromEnum pfromEnum p
          = T.ufun1 c12v31v12v34fromEnum pfromEnum p hfromEnum
          where hfromEnum (T.R Sunday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                hfromEnum (T.R Monday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))
                hfromEnum (T.R Tuesday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (2))
                hfromEnum (T.R Wednesday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (3))
                hfromEnum (T.R Thursday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (4))
                hfromEnum (T.R Friday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (5))
                hfromEnum (T.R Saturday _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (6))
                hfromEnum _ p = T.fatal p
        gtoEnum ptoEnum p
          = T.ufun1 c12v31v12v34toEnum ptoEnum p htoEnum
          where htoEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h12v31v12v34n p)
                      (y1toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Sunday aSunday
                        h12v31v12v34n p = y1toEnum fv12v31v12v34n p
                htoEnum fv12v31v12v34n p = y1toEnum fv12v31v12v34n p
                y1toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      (h12v31v12v34n p)
                      (y2toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Monday aMonday
                        h12v31v12v34n p = y2toEnum fv12v31v12v34n p
                y1toEnum fv12v31v12v34n p = y2toEnum fv12v31v12v34n p
                y2toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (2))))
                      (h12v31v12v34n p)
                      (y3toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Tuesday aTuesday
                        h12v31v12v34n p = y3toEnum fv12v31v12v34n p
                y2toEnum fv12v31v12v34n p = y3toEnum fv12v31v12v34n p
                y3toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (3))))
                      (h12v31v12v34n p)
                      (y4toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Wednesday aWednesday
                        h12v31v12v34n p = y4toEnum fv12v31v12v34n p
                y3toEnum fv12v31v12v34n p = y4toEnum fv12v31v12v34n p
                y4toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (4))))
                      (h12v31v12v34n p)
                      (y5toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Thursday aThursday
                        h12v31v12v34n p = y5toEnum fv12v31v12v34n p
                y4toEnum fv12v31v12v34n p = y5toEnum fv12v31v12v34n p
                y5toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (5))))
                      (h12v31v12v34n p)
                      (y6toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Friday aFriday
                        h12v31v12v34n p = y6toEnum fv12v31v12v34n p
                y5toEnum fv12v31v12v34n p = y6toEnum fv12v31v12v34n p
                y6toEnum fv12v31v12v34n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv12v31v12v34n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (6))))
                      (h12v31v12v34n p)
                      (y7toEnum fv12v31v12v34n p)
                  where h12v31v12v34n p
                          = T.con0 T.mkNoSrcPos p Saturday aSaturday
                        h12v31v12v34n p = y7toEnum fv12v31v12v34n p
                y6toEnum fv12v31v12v34n p = y7toEnum fv12v31v12v34n p
                y7toEnum _ p
                  = T.uwrapForward p
                      (herror
                         (T.fromLitString T.mkNoSrcPos p
                            "toEnum: argument out of bounds")
                         p)
        genumFrom penumFrom p
          = T.ufun1 c12v31v12v34enumFrom penumFrom p henumFrom
          where henumFrom fy1 p
                  = T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                      fy1
                      (T.con0 T.mkNoSrcPos p Saturday aSaturday)
        genumFromThen penumFromThen p
          = T.ufun2 c12v31v12v34enumFromThen penumFromThen p
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
                         (T.con0 T.mkNoSrcPos p Saturday aSaturday)
                         (T.con0 T.mkNoSrcPos p Sunday aSunday))
 
instance Bounded Day where
        gminBound pminBound p
          = T.uconstUse pminBound p sminBound
        sminBound
          = T.uconstDef p c12v37v12v43minBound
              (\ p -> T.con0 T.mkNoSrcPos p Sunday aSunday)
        gmaxBound pmaxBound p
          = T.uconstUse pmaxBound p smaxBound
        smaxBound
          = T.uconstDef p c12v37v12v43maxBound
              (\ p -> T.con0 T.mkNoSrcPos p Saturday aSaturday)
 
instance Ix Day where
        grange prange p
          = T.ufun1 c12v46v12v47range prange p hrange
          where hrange (T.R (T.Tuple2 fy1 fy2) _) p
                  = T.uwrapForward p
                      (hmap (grtoEnum T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                            (T.uwrapForward p (hrfromEnum fy1 p))
                            (T.uwrapForward p (hrfromEnum fy2 p)))
                         p)
                  where  
                        grtoEnum ::
                                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Day)
                         
                        hrtoEnum :: T.R Int -> T.RefExp -> T.R Day
                        grtoEnum prtoEnum p
                          = T.ufun1 c12v46v12v47rtoEnum prtoEnum p hrtoEnum
                        artoEnum = c12v46v12v47rtoEnum
                        hrtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h12v46v12v47n p)
                              (y1rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Sunday aSunday
                                h12v46v12v47n p = y1rtoEnum fv12v46v12v47n p
                        hrtoEnum fv12v46v12v47n p
                          = y1rtoEnum fv12v46v12v47n p
                        y1rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (1))))
                              (h12v46v12v47n p)
                              (y2rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Monday aMonday
                                h12v46v12v47n p = y2rtoEnum fv12v46v12v47n p
                        y1rtoEnum fv12v46v12v47n p
                          = y2rtoEnum fv12v46v12v47n p
                        y2rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (2))))
                              (h12v46v12v47n p)
                              (y3rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Tuesday aTuesday
                                h12v46v12v47n p = y3rtoEnum fv12v46v12v47n p
                        y2rtoEnum fv12v46v12v47n p
                          = y3rtoEnum fv12v46v12v47n p
                        y3rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (3))))
                              (h12v46v12v47n p)
                              (y4rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Wednesday aWednesday
                                h12v46v12v47n p = y4rtoEnum fv12v46v12v47n p
                        y3rtoEnum fv12v46v12v47n p
                          = y4rtoEnum fv12v46v12v47n p
                        y4rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (4))))
                              (h12v46v12v47n p)
                              (y5rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Thursday aThursday
                                h12v46v12v47n p = y5rtoEnum fv12v46v12v47n p
                        y4rtoEnum fv12v46v12v47n p
                          = y5rtoEnum fv12v46v12v47n p
                        y5rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (5))))
                              (h12v46v12v47n p)
                              (y6rtoEnum fv12v46v12v47n p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Friday aFriday
                                h12v46v12v47n p = y6rtoEnum fv12v46v12v47n p
                        y5rtoEnum fv12v46v12v47n p
                          = y6rtoEnum fv12v46v12v47n p
                        y6rtoEnum fv12v46v12v47n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv12v46v12v47n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (6))))
                              (h12v46v12v47n p)
                              (T.fatal p)
                          where h12v46v12v47n p
                                  = T.con0 T.mkNoSrcPos p Saturday aSaturday
                                h12v46v12v47n p = T.fatal p
                        y6rtoEnum _ p = T.fatal p
                         
                        grfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Day Int)
                         
                        hrfromEnum :: T.R Day -> T.RefExp -> T.R Int
                        grfromEnum prfromEnum p
                          = T.ufun1 c12v46v12v47rfromEnum prfromEnum p
                              hrfromEnum
                        arfromEnum = c12v46v12v47rfromEnum
                        hrfromEnum (T.R Sunday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hrfromEnum (T.R Monday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hrfromEnum (T.R Tuesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hrfromEnum (T.R Wednesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hrfromEnum (T.R Thursday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hrfromEnum (T.R Friday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hrfromEnum (T.R Saturday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hrfromEnum _ p = T.fatal p
        gindex pindex p
          = T.ufun2 c12v46v12v47index pindex p hindex
          where hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                      (T.uwrapForward p (hifromEnum fy3 p))
                      (T.uwrapForward p (hifromEnum fy2 p))
                  where  
                        gifromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Day Int)
                         
                        hifromEnum :: T.R Day -> T.RefExp -> T.R Int
                        gifromEnum pifromEnum p
                          = T.ufun1 c12v46v12v47ifromEnum pifromEnum p
                              hifromEnum
                        aifromEnum = c12v46v12v47ifromEnum
                        hifromEnum (T.R Sunday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hifromEnum (T.R Monday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hifromEnum (T.R Tuesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hifromEnum (T.R Wednesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hifromEnum (T.R Thursday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hifromEnum (T.R Friday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hifromEnum (T.R Saturday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hifromEnum _ p = T.fatal p
        ginRange pinRange p
          = T.ufun2 c12v46v12v47inRange pinRange p hinRange
          where hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uwrapForward p (hnfromEnum fy1 p))
                         (T.uwrapForward p (hnfromEnum fy2 p)))
                      (T.uwrapForward p (hnfromEnum fy3 p))
                  where  
                        gnfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun Day Int)
                         
                        hnfromEnum :: T.R Day -> T.RefExp -> T.R Int
                        gnfromEnum pnfromEnum p
                          = T.ufun1 c12v46v12v47nfromEnum pnfromEnum p
                              hnfromEnum
                        anfromEnum = c12v46v12v47nfromEnum
                        hnfromEnum (T.R Sunday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hnfromEnum (T.R Monday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hnfromEnum (T.R Tuesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hnfromEnum (T.R Wednesday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hnfromEnum (T.R Thursday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hnfromEnum (T.R Friday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hnfromEnum (T.R Saturday _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hnfromEnum _ p = T.fatal p
 
instance Read Day where
        greadsPrec preadsPrec p
          = T.ufun1 c12v50v12v53readsPrec preadsPrec p
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
                                     (T.con0 T.mkNoSrcPos p Sunday aSunday))
                                  (T.fromLitString T.mkNoSrcPos p "Sunday")
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
                                        (T.con0 T.mkNoSrcPos p Monday aMonday))
                                     (T.fromLitString T.mkNoSrcPos p "Monday")
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
                                           (T.con0 T.mkNoSrcPos p Tuesday
                                              aTuesday))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "Tuesday")
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
                                              (Hat.PreludeBasic.gyield
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.con0 T.mkNoSrcPos p Wednesday
                                                 aWednesday))
                                           (T.fromLitString T.mkNoSrcPos p
                                              "Wednesday")
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
                                                 (Hat.PreludeBasic.gyield
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.con0 T.mkNoSrcPos p Thursday
                                                    aThursday))
                                              (T.fromLitString T.mkNoSrcPos p
                                                 "Thursday")
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
                                                    (Hat.PreludeBasic.gyield
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.con0 T.mkNoSrcPos p
                                                       Friday
                                                       aFriday))
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "Friday")
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
                                                    (Hat.PreludeBasic.gyield
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.con0 T.mkNoSrcPos p
                                                       Saturday
                                                       aSaturday))
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "Saturday")
                                                 p))
                                           p)))))))
 
instance Show Day where
        gshowsPrec pshowsPrec p
          = T.ufun2 c12v56v12v59showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fy1 (T.R Sunday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Sunday")
                hshowsPrec fy1 (T.R Monday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Monday")
                hshowsPrec fy1 (T.R Tuesday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Tuesday")
                hshowsPrec fy1 (T.R Wednesday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Wednesday")
                hshowsPrec fy1 (T.R Thursday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Thursday")
                hshowsPrec fy1 (T.R Friday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Friday")
                hshowsPrec fy1 (T.R Saturday _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Saturday")
                hshowsPrec _ _ p = T.fatal p
 
data CalendarTime = CalendarTime{bctYear :: T.R Int,
                                 bctMonth :: T.R Month,
                                 bctDay, bctHour, bctMin, bctSec :: T.R Int,
                                 bctPicosec :: T.R Integer, bctWDay :: T.R Day,
                                 bctYDay :: T.R Int, bctTZName :: T.R String,
                                 bctTZ :: T.R Int, bctIsDST :: T.R Bool}
 
instance T.WrapVal CalendarTime where
        wrapVal pwrapVal
          kwrapVal@(CalendarTime (T.R _ z1wrapVal)
                      (T.R _ z2wrapVal) (T.R _ z3wrapVal) (T.R _ z4wrapVal)
                      (T.R _ z5wrapVal) (T.R _ z6wrapVal) (T.R _ z7wrapVal)
                      (T.R _ z8wrapVal) (T.R _ z9wrapVal)
                      (T.R _ z10wrapVal) (T.R _ z11wrapVal)
                      (T.R _ z12wrapVal))
          p
          = T.R kwrapVal
              (T.mkValueApp12 p pwrapVal aCalendarTime z1wrapVal
                 z2wrapVal
                 z3wrapVal
                 z4wrapVal
                 z5wrapVal
                 z6wrapVal
                 z7wrapVal
                 z8wrapVal
                 z9wrapVal
                 z10wrapVal
                 z11wrapVal
                 z12wrapVal)
gctYear pctYear p = T.ufun1 actYear pctYear p hctYear
hctYear (T.R z1ctYear _) p
  = T.projection T.mkNoSrcPos p (bctYear z1ctYear)
gctMonth pctMonth p
  = T.ufun1 actMonth pctMonth p hctMonth
hctMonth (T.R z1ctMonth _) p
  = T.projection T.mkNoSrcPos p (bctMonth z1ctMonth)
gctDay pctDay p = T.ufun1 actDay pctDay p hctDay
hctDay (T.R z1ctDay _) p
  = T.projection T.mkNoSrcPos p (bctDay z1ctDay)
gctHour pctHour p = T.ufun1 actHour pctHour p hctHour
hctHour (T.R z1ctHour _) p
  = T.projection T.mkNoSrcPos p (bctHour z1ctHour)
gctMin pctMin p = T.ufun1 actMin pctMin p hctMin
hctMin (T.R z1ctMin _) p
  = T.projection T.mkNoSrcPos p (bctMin z1ctMin)
gctSec pctSec p = T.ufun1 actSec pctSec p hctSec
hctSec (T.R z1ctSec _) p
  = T.projection T.mkNoSrcPos p (bctSec z1ctSec)
gctPicosec pctPicosec p
  = T.ufun1 actPicosec pctPicosec p hctPicosec
hctPicosec (T.R z1ctPicosec _) p
  = T.projection T.mkNoSrcPos p
      (bctPicosec z1ctPicosec)
gctWDay pctWDay p = T.ufun1 actWDay pctWDay p hctWDay
hctWDay (T.R z1ctWDay _) p
  = T.projection T.mkNoSrcPos p (bctWDay z1ctWDay)
gctYDay pctYDay p = T.ufun1 actYDay pctYDay p hctYDay
hctYDay (T.R z1ctYDay _) p
  = T.projection T.mkNoSrcPos p (bctYDay z1ctYDay)
gctTZName pctTZName p
  = T.ufun1 actTZName pctTZName p hctTZName
hctTZName (T.R z1ctTZName _) p
  = T.projection T.mkNoSrcPos p (bctTZName z1ctTZName)
gctTZ pctTZ p = T.ufun1 actTZ pctTZ p hctTZ
hctTZ (T.R z1ctTZ _) p
  = T.projection T.mkNoSrcPos p (bctTZ z1ctTZ)
gctIsDST pctIsDST p
  = T.ufun1 actIsDST pctIsDST p hctIsDST
hctIsDST (T.R z1ctIsDST _) p
  = T.projection T.mkNoSrcPos p (bctIsDST z1ctIsDST)
 
instance Eq CalendarTime where
        (%==) !== p = T.ufun2 (+$&=$#=$&=$$==) (%==) p (*==)
          where (T.R
                   (CalendarTime fy1 fy2 fy3 fy4 fy5 fy6 fy7 fy8 fy9
                      fy10 fy11 fy12)
                   _
                   *==
                   T.R
                     (CalendarTime fy13 fy14 fy15 fy16 fy17 fy18 fy19 fy20
                        fy21 fy22 fy23 fy24)
                     _)
                  p
                  = T.uwrapForward p
                      ((Hat.PreludeBasic.*&&)
                         (T.uap2 T.mkNoSrcPos p
                            ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                            fy1
                            fy13)
                         (T.uwrapForward p
                            ((Hat.PreludeBasic.*&&)
                               (T.uap2 T.mkNoSrcPos p
                                  ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                  fy2
                                  fy14)
                               (T.uwrapForward p
                                  ((Hat.PreludeBasic.*&&)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                        fy3
                                        fy15)
                                     (T.uwrapForward p
                                        ((Hat.PreludeBasic.*&&)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((Hat.PreludeBasic.!==)
                                                 T.mkNoSrcPos
                                                 p)
                                              fy4
                                              fy16)
                                           (T.uwrapForward p
                                              ((Hat.PreludeBasic.*&&)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((Hat.PreludeBasic.!==)
                                                       T.mkNoSrcPos
                                                       p)
                                                    fy5
                                                    fy17)
                                                 (T.uwrapForward p
                                                    ((Hat.PreludeBasic.*&&)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((Hat.PreludeBasic.!==)
                                                             T.mkNoSrcPos
                                                             p)
                                                          fy6
                                                          fy18)
                                                       (T.uwrapForward p
                                                          ((Hat.PreludeBasic.*&&)
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                ((Hat.PreludeBasic.!==)
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                fy7
                                                                fy19)
                                                             (T.uwrapForward p
                                                                ((Hat.PreludeBasic.*&&)
                                                                   (T.uap2
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      ((Hat.PreludeBasic.!==)
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      fy8
                                                                      fy20)
                                                                   (T.uwrapForward
                                                                      p
                                                                      ((Hat.PreludeBasic.*&&)
                                                                         (T.uap2
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            ((Hat.PreludeBasic.!==)
                                                                               T.mkNoSrcPos
                                                                               p)
                                                                            fy9
                                                                            fy21)
                                                                         (T.uwrapForward
                                                                            p
                                                                            ((Hat.PreludeBasic.*&&)
                                                                               (T.uap2
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  ((Hat.PreludeBasic.!==)
                                                                                     T.mkNoSrcPos
                                                                                     p)
                                                                                  fy10
                                                                                  fy22)
                                                                               (T.uwrapForward
                                                                                  p
                                                                                  ((Hat.PreludeBasic.*&&)
                                                                                     (T.uap2
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        ((Hat.PreludeBasic.!==)
                                                                                           T.mkNoSrcPos
                                                                                           p)
                                                                                        fy11
                                                                                        fy23)
                                                                                     (T.uwrapForward
                                                                                        p
                                                                                        ((Hat.PreludeBasic.*&&)
                                                                                           (T.uap2
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              ((Hat.PreludeBasic.!==)
                                                                                                 T.mkNoSrcPos
                                                                                                 p)
                                                                                              fy12
                                                                                              fy24)
                                                                                           (T.con0
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              Hat.PreludeBuiltinTypes.True
                                                                                              Hat.PreludeBuiltinTypes.aTrue)
                                                                                           p))
                                                                                     p))
                                                                               p))
                                                                         p))
                                                                   p))
                                                             p))
                                                       p))
                                                 p))
                                           p))
                                     p))
                               p))
                         p)
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord CalendarTime where
        gcompare pcompare p
          = T.ufun2 c24v25v24v27compare pcompare p hcompare
          where hcompare
                  (T.R
                     (CalendarTime fy3 fy4 fy5 fy6 fy7 fy8 fy9 fy10 fy11
                        fy12 fy13 fy14)
                     _)
                  (T.R
                     (CalendarTime fy15 fy16 fy17 fy18 fy19 fy20 fy21 fy22
                        fy23 fy24 fy25 fy26)
                     _)
                  p
                  = T.uccase T.mkNoSrcPos p
                      (let v24v25v24v27v1 (T.R EQ _) p
                             = T.uccase T.mkNoSrcPos p
                                 (let v24v25v24v27v1 (T.R EQ _) p
                                        = T.uccase T.mkNoSrcPos p
                                            (let v24v25v24v27v1 (T.R EQ _) p
                                                   = T.uccase T.mkNoSrcPos p
                                                       (let v24v25v24v27v1
                                                              (T.R EQ _) p
                                                              = T.uccase
                                                                  T.mkNoSrcPos
                                                                  p
                                                                  (let v24v25v24v27v1
                                                                         (T.R EQ
                                                                            _)
                                                                         p
                                                                         = T.uccase
                                                                             T.mkNoSrcPos
                                                                             p
                                                                             (let v24v25v24v27v1
                                                                                    (T.R
                                                                                       EQ
                                                                                       _)
                                                                                    p
                                                                                    = T.uccase
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        (let v24v25v24v27v1
                                                                                               (T.R
                                                                                                  EQ
                                                                                                  _)
                                                                                               p
                                                                                               = T.uccase
                                                                                                   T.mkNoSrcPos
                                                                                                   p
                                                                                                   (let v24v25v24v27v1
                                                                                                          (T.R
                                                                                                             EQ
                                                                                                             _)
                                                                                                          p
                                                                                                          = T.uccase
                                                                                                              T.mkNoSrcPos
                                                                                                              p
                                                                                                              (let v24v25v24v27v1
                                                                                                                     (T.R
                                                                                                                        EQ
                                                                                                                        _)
                                                                                                                     p
                                                                                                                     = T.uccase
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         (let v24v25v24v27v1
                                                                                                                                (T.R
                                                                                                                                   EQ
                                                                                                                                   _)
                                                                                                                                p
                                                                                                                                = T.uccase
                                                                                                                                    T.mkNoSrcPos
                                                                                                                                    p
                                                                                                                                    (let v24v25v24v27v1
                                                                                                                                           (T.R
                                                                                                                                              EQ
                                                                                                                                              _)
                                                                                                                                           p
                                                                                                                                           = T.uap2
                                                                                                                                               T.mkNoSrcPos
                                                                                                                                               p
                                                                                                                                               (gcompare
                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                  p)
                                                                                                                                               fy14
                                                                                                                                               fy26
                                                                                                                                         v24v25v24v27v1
                                                                                                                                           fy1
                                                                                                                                           p
                                                                                                                                           = T.projection
                                                                                                                                               T.mkNoSrcPos
                                                                                                                                               p
                                                                                                                                               fy1
                                                                                                                                       in
                                                                                                                                       v24v25v24v27v1)
                                                                                                                                    (T.uap2
                                                                                                                                       T.mkNoSrcPos
                                                                                                                                       p
                                                                                                                                       (gcompare
                                                                                                                                          T.mkNoSrcPos
                                                                                                                                          p)
                                                                                                                                       fy13
                                                                                                                                       fy25)
                                                                                                                              v24v25v24v27v1
                                                                                                                                fy1
                                                                                                                                p
                                                                                                                                = T.projection
                                                                                                                                    T.mkNoSrcPos
                                                                                                                                    p
                                                                                                                                    fy1
                                                                                                                            in
                                                                                                                            v24v25v24v27v1)
                                                                                                                         (T.uap2
                                                                                                                            T.mkNoSrcPos
                                                                                                                            p
                                                                                                                            (gcompare
                                                                                                                               T.mkNoSrcPos
                                                                                                                               p)
                                                                                                                            fy12
                                                                                                                            fy24)
                                                                                                                   v24v25v24v27v1
                                                                                                                     fy1
                                                                                                                     p
                                                                                                                     = T.projection
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         fy1
                                                                                                                 in
                                                                                                                 v24v25v24v27v1)
                                                                                                              (T.uap2
                                                                                                                 T.mkNoSrcPos
                                                                                                                 p
                                                                                                                 (gcompare
                                                                                                                    T.mkNoSrcPos
                                                                                                                    p)
                                                                                                                 fy11
                                                                                                                 fy23)
                                                                                                        v24v25v24v27v1
                                                                                                          fy1
                                                                                                          p
                                                                                                          = T.projection
                                                                                                              T.mkNoSrcPos
                                                                                                              p
                                                                                                              fy1
                                                                                                      in
                                                                                                      v24v25v24v27v1)
                                                                                                   (T.uap2
                                                                                                      T.mkNoSrcPos
                                                                                                      p
                                                                                                      (gcompare
                                                                                                         T.mkNoSrcPos
                                                                                                         p)
                                                                                                      fy10
                                                                                                      fy22)
                                                                                             v24v25v24v27v1
                                                                                               fy1
                                                                                               p
                                                                                               = T.projection
                                                                                                   T.mkNoSrcPos
                                                                                                   p
                                                                                                   fy1
                                                                                           in
                                                                                           v24v25v24v27v1)
                                                                                        (T.uap2
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           (gcompare
                                                                                              T.mkNoSrcPos
                                                                                              p)
                                                                                           fy9
                                                                                           fy21)
                                                                                  v24v25v24v27v1
                                                                                    fy1
                                                                                    p
                                                                                    = T.projection
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        fy1
                                                                                in
                                                                                v24v25v24v27v1)
                                                                             (T.uap2
                                                                                T.mkNoSrcPos
                                                                                p
                                                                                (gcompare
                                                                                   T.mkNoSrcPos
                                                                                   p)
                                                                                fy8
                                                                                fy20)
                                                                       v24v25v24v27v1
                                                                         fy1 p
                                                                         = T.projection
                                                                             T.mkNoSrcPos
                                                                             p
                                                                             fy1
                                                                     in
                                                                     v24v25v24v27v1)
                                                                  (T.uap2
                                                                     T.mkNoSrcPos
                                                                     p
                                                                     (gcompare
                                                                        T.mkNoSrcPos
                                                                        p)
                                                                     fy7
                                                                     fy19)
                                                            v24v25v24v27v1 fy1 p
                                                              = T.projection
                                                                  T.mkNoSrcPos
                                                                  p
                                                                  fy1
                                                          in v24v25v24v27v1)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          (gcompare T.mkNoSrcPos
                                                             p)
                                                          fy6
                                                          fy18)
                                                 v24v25v24v27v1 fy1 p
                                                   = T.projection T.mkNoSrcPos p
                                                       fy1
                                               in v24v25v24v27v1)
                                            (T.uap2 T.mkNoSrcPos p
                                               (gcompare T.mkNoSrcPos p)
                                               fy5
                                               fy17)
                                      v24v25v24v27v1 fy1 p
                                        = T.projection T.mkNoSrcPos p fy1
                                    in v24v25v24v27v1)
                                 (T.uap2 T.mkNoSrcPos p
                                    (gcompare T.mkNoSrcPos p)
                                    fy4
                                    fy16)
                           v24v25v24v27v1 fy1 p
                             = T.projection T.mkNoSrcPos p fy1
                         in v24v25v24v27v1)
                      (T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p) fy3
                         fy15)
                hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c24v25v24v27localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c24v25v24v27localFromEnum
                        hlocalFromEnum
                          (T.R (CalendarTime _ _ _ _ _ _ _ _ _ _ _ _) _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Read CalendarTime where
        greadsPrec preadsPrec p
          = T.ufun1 c24v30v24v33readsPrec preadsPrec p
              hreadsPrec
          where hreadsPrec fy1 p
                  = T.uwrapForward p
                      (Hat.PreludeBasic.hthenLex
                         (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hthenLex
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uwrapForward p
                                           (Hat.PreludeBasic.hthenLex
                                              (T.uap2 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.gthenAp
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.uwrapForward p
                                                    (Hat.PreludeBasic.hthenLex
                                                       (T.uwrapForward p
                                                          (Hat.PreludeBasic.hthenLex
                                                             (T.uwrapForward p
                                                                (Hat.PreludeBasic.hthenLex
                                                                   (T.uap2
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (Hat.PreludeBasic.gthenAp
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.uwrapForward
                                                                         p
                                                                         (Hat.PreludeBasic.hthenLex
                                                                            (T.uwrapForward
                                                                               p
                                                                               (Hat.PreludeBasic.hthenLex
                                                                                  (T.uwrapForward
                                                                                     p
                                                                                     (Hat.PreludeBasic.hthenLex
                                                                                        (T.uap2
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           (Hat.PreludeBasic.gthenAp
                                                                                              T.mkNoSrcPos
                                                                                              p)
                                                                                           (T.uwrapForward
                                                                                              p
                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                 (T.uwrapForward
                                                                                                    p
                                                                                                    (Hat.PreludeBasic.hthenLex
                                                                                                       (T.uwrapForward
                                                                                                          p
                                                                                                          (Hat.PreludeBasic.hthenLex
                                                                                                             (T.uap2
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                (Hat.PreludeBasic.gthenAp
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p)
                                                                                                                (T.uwrapForward
                                                                                                                   p
                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                      (T.uwrapForward
                                                                                                                         p
                                                                                                                         (Hat.PreludeBasic.hthenLex
                                                                                                                            (T.uwrapForward
                                                                                                                               p
                                                                                                                               (Hat.PreludeBasic.hthenLex
                                                                                                                                  (T.uap2
                                                                                                                                     T.mkNoSrcPos
                                                                                                                                     p
                                                                                                                                     (Hat.PreludeBasic.gthenAp
                                                                                                                                        T.mkNoSrcPos
                                                                                                                                        p)
                                                                                                                                     (T.uwrapForward
                                                                                                                                        p
                                                                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                                                                           (T.uwrapForward
                                                                                                                                              p
                                                                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                                                                 (T.uwrapForward
                                                                                                                                                    p
                                                                                                                                                    (Hat.PreludeBasic.hthenLex
                                                                                                                                                       (T.uap2
                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                          p
                                                                                                                                                          (Hat.PreludeBasic.gthenAp
                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                             p)
                                                                                                                                                          (T.uwrapForward
                                                                                                                                                             p
                                                                                                                                                             (Hat.PreludeBasic.hthenLex
                                                                                                                                                                (T.uwrapForward
                                                                                                                                                                   p
                                                                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                                                                      (T.uwrapForward
                                                                                                                                                                         p
                                                                                                                                                                         (Hat.PreludeBasic.hthenLex
                                                                                                                                                                            (T.uap2
                                                                                                                                                                               T.mkNoSrcPos
                                                                                                                                                                               p
                                                                                                                                                                               (Hat.PreludeBasic.gthenAp
                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                  p)
                                                                                                                                                                               (T.uwrapForward
                                                                                                                                                                                  p
                                                                                                                                                                                  (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                     (T.uwrapForward
                                                                                                                                                                                        p
                                                                                                                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                           (T.uwrapForward
                                                                                                                                                                                              p
                                                                                                                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                 (T.uap2
                                                                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                                                                    p
                                                                                                                                                                                                    (Hat.PreludeBasic.gthenAp
                                                                                                                                                                                                       T.mkNoSrcPos
                                                                                                                                                                                                       p)
                                                                                                                                                                                                    (T.uwrapForward
                                                                                                                                                                                                       p
                                                                                                                                                                                                       (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                          (T.uwrapForward
                                                                                                                                                                                                             p
                                                                                                                                                                                                             (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                (T.uwrapForward
                                                                                                                                                                                                                   p
                                                                                                                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                      (T.uap2
                                                                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                                                                         p
                                                                                                                                                                                                                         (Hat.PreludeBasic.gthenAp
                                                                                                                                                                                                                            T.mkNoSrcPos
                                                                                                                                                                                                                            p)
                                                                                                                                                                                                                         (T.uwrapForward
                                                                                                                                                                                                                            p
                                                                                                                                                                                                                            (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                               (T.uwrapForward
                                                                                                                                                                                                                                  p
                                                                                                                                                                                                                                  (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                     (T.uwrapForward
                                                                                                                                                                                                                                        p
                                                                                                                                                                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                           (T.uap2
                                                                                                                                                                                                                                              T.mkNoSrcPos
                                                                                                                                                                                                                                              p
                                                                                                                                                                                                                                              (Hat.PreludeBasic.gthenAp
                                                                                                                                                                                                                                                 T.mkNoSrcPos
                                                                                                                                                                                                                                                 p)
                                                                                                                                                                                                                                              (T.uwrapForward
                                                                                                                                                                                                                                                 p
                                                                                                                                                                                                                                                 (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                    (T.uwrapForward
                                                                                                                                                                                                                                                       p
                                                                                                                                                                                                                                                       (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                          (T.uwrapForward
                                                                                                                                                                                                                                                             p
                                                                                                                                                                                                                                                             (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                                (T.uap2
                                                                                                                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                                                                                                                   p
                                                                                                                                                                                                                                                                   (Hat.PreludeBasic.gthenAp
                                                                                                                                                                                                                                                                      T.mkNoSrcPos
                                                                                                                                                                                                                                                                      p)
                                                                                                                                                                                                                                                                   (T.uwrapForward
                                                                                                                                                                                                                                                                      p
                                                                                                                                                                                                                                                                      (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                                         (T.uwrapForward
                                                                                                                                                                                                                                                                            p
                                                                                                                                                                                                                                                                            (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                                               (T.uwrapForward
                                                                                                                                                                                                                                                                                  p
                                                                                                                                                                                                                                                                                  (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                                                     (T.uwrapForward
                                                                                                                                                                                                                                                                                        p
                                                                                                                                                                                                                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                                                                                                                           (T.uap1
                                                                                                                                                                                                                                                                                              T.mkNoSrcPos
                                                                                                                                                                                                                                                                                              p
                                                                                                                                                                                                                                                                                              (Hat.PreludeBasic.gyield
                                                                                                                                                                                                                                                                                                 T.mkNoSrcPos
                                                                                                                                                                                                                                                                                                 p)
                                                                                                                                                                                                                                                                                              (T.pa0
                                                                                                                                                                                                                                                                                                 CalendarTime
                                                                                                                                                                                                                                                                                                 T.cn12
                                                                                                                                                                                                                                                                                                 T.mkNoSrcPos
                                                                                                                                                                                                                                                                                                 p
                                                                                                                                                                                                                                                                                                 aCalendarTime))
                                                                                                                                                                                                                                                                                           (T.fromLitString
                                                                                                                                                                                                                                                                                              T.mkNoSrcPos
                                                                                                                                                                                                                                                                                              p
                                                                                                                                                                                                                                                                                              "CalendarTime")
                                                                                                                                                                                                                                                                                           p))
                                                                                                                                                                                                                                                                                     (T.fromLitString
                                                                                                                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                                                                                                                        p
                                                                                                                                                                                                                                                                                        "{")
                                                                                                                                                                                                                                                                                     p))
                                                                                                                                                                                                                                                                               (T.fromLitString
                                                                                                                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                                                                                                                  p
                                                                                                                                                                                                                                                                                  "ctYear")
                                                                                                                                                                                                                                                                               p))
                                                                                                                                                                                                                                                                         (T.fromLitString
                                                                                                                                                                                                                                                                            T.mkNoSrcPos
                                                                                                                                                                                                                                                                            p
                                                                                                                                                                                                                                                                            "=")
                                                                                                                                                                                                                                                                         p))
                                                                                                                                                                                                                                                                   (T.uap1
                                                                                                                                                                                                                                                                      T.mkNoSrcPos
                                                                                                                                                                                                                                                                      p
                                                                                                                                                                                                                                                                      (greadsPrec
                                                                                                                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                                                                                                                         p)
                                                                                                                                                                                                                                                                      (T.uap1
                                                                                                                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                                                                                                                         p
                                                                                                                                                                                                                                                                         (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                                                                                                                            T.mkNoSrcPos
                                                                                                                                                                                                                                                                            p)
                                                                                                                                                                                                                                                                         (T.conInteger
                                                                                                                                                                                                                                                                            T.mkNoSrcPos
                                                                                                                                                                                                                                                                            p
                                                                                                                                                                                                                                                                            (0)))))
                                                                                                                                                                                                                                                                (T.fromLitString
                                                                                                                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                                                                                                                   p
                                                                                                                                                                                                                                                                   ",")
                                                                                                                                                                                                                                                                p))
                                                                                                                                                                                                                                                          (T.fromLitString
                                                                                                                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                                                                                                                             p
                                                                                                                                                                                                                                                             "ctMonth")
                                                                                                                                                                                                                                                          p))
                                                                                                                                                                                                                                                    (T.fromLitString
                                                                                                                                                                                                                                                       T.mkNoSrcPos
                                                                                                                                                                                                                                                       p
                                                                                                                                                                                                                                                       "=")
                                                                                                                                                                                                                                                    p))
                                                                                                                                                                                                                                              (T.uap1
                                                                                                                                                                                                                                                 T.mkNoSrcPos
                                                                                                                                                                                                                                                 p
                                                                                                                                                                                                                                                 (greadsPrec
                                                                                                                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                                                                                                                    p)
                                                                                                                                                                                                                                                 (T.uap1
                                                                                                                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                                                                                                                    p
                                                                                                                                                                                                                                                    (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                                                                                                       T.mkNoSrcPos
                                                                                                                                                                                                                                                       p)
                                                                                                                                                                                                                                                    (T.conInteger
                                                                                                                                                                                                                                                       T.mkNoSrcPos
                                                                                                                                                                                                                                                       p
                                                                                                                                                                                                                                                       (0)))))
                                                                                                                                                                                                                                           (T.fromLitString
                                                                                                                                                                                                                                              T.mkNoSrcPos
                                                                                                                                                                                                                                              p
                                                                                                                                                                                                                                              ",")
                                                                                                                                                                                                                                           p))
                                                                                                                                                                                                                                     (T.fromLitString
                                                                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                                                                        p
                                                                                                                                                                                                                                        "ctDay")
                                                                                                                                                                                                                                     p))
                                                                                                                                                                                                                               (T.fromLitString
                                                                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                                                                  p
                                                                                                                                                                                                                                  "=")
                                                                                                                                                                                                                               p))
                                                                                                                                                                                                                         (T.uap1
                                                                                                                                                                                                                            T.mkNoSrcPos
                                                                                                                                                                                                                            p
                                                                                                                                                                                                                            (greadsPrec
                                                                                                                                                                                                                               T.mkNoSrcPos
                                                                                                                                                                                                                               p)
                                                                                                                                                                                                                            (T.uap1
                                                                                                                                                                                                                               T.mkNoSrcPos
                                                                                                                                                                                                                               p
                                                                                                                                                                                                                               (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                                                                  p)
                                                                                                                                                                                                                               (T.conInteger
                                                                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                                                                  p
                                                                                                                                                                                                                                  (0)))))
                                                                                                                                                                                                                      (T.fromLitString
                                                                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                                                                         p
                                                                                                                                                                                                                         ",")
                                                                                                                                                                                                                      p))
                                                                                                                                                                                                                (T.fromLitString
                                                                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                                                                   p
                                                                                                                                                                                                                   "ctHour")
                                                                                                                                                                                                                p))
                                                                                                                                                                                                          (T.fromLitString
                                                                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                                                                             p
                                                                                                                                                                                                             "=")
                                                                                                                                                                                                          p))
                                                                                                                                                                                                    (T.uap1
                                                                                                                                                                                                       T.mkNoSrcPos
                                                                                                                                                                                                       p
                                                                                                                                                                                                       (greadsPrec
                                                                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                                                                          p)
                                                                                                                                                                                                       (T.uap1
                                                                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                                                                          p
                                                                                                                                                                                                          (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                                                                             p)
                                                                                                                                                                                                          (T.conInteger
                                                                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                                                                             p
                                                                                                                                                                                                             (0)))))
                                                                                                                                                                                                 (T.fromLitString
                                                                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                                                                    p
                                                                                                                                                                                                    ",")
                                                                                                                                                                                                 p))
                                                                                                                                                                                           (T.fromLitString
                                                                                                                                                                                              T.mkNoSrcPos
                                                                                                                                                                                              p
                                                                                                                                                                                              "ctMin")
                                                                                                                                                                                           p))
                                                                                                                                                                                     (T.fromLitString
                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                        p
                                                                                                                                                                                        "=")
                                                                                                                                                                                     p))
                                                                                                                                                                               (T.uap1
                                                                                                                                                                                  T.mkNoSrcPos
                                                                                                                                                                                  p
                                                                                                                                                                                  (greadsPrec
                                                                                                                                                                                     T.mkNoSrcPos
                                                                                                                                                                                     p)
                                                                                                                                                                                  (T.uap1
                                                                                                                                                                                     T.mkNoSrcPos
                                                                                                                                                                                     p
                                                                                                                                                                                     (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                        p)
                                                                                                                                                                                     (T.conInteger
                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                        p
                                                                                                                                                                                        (0)))))
                                                                                                                                                                            (T.fromLitString
                                                                                                                                                                               T.mkNoSrcPos
                                                                                                                                                                               p
                                                                                                                                                                               ",")
                                                                                                                                                                            p))
                                                                                                                                                                      (T.fromLitString
                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                         p
                                                                                                                                                                         "ctSec")
                                                                                                                                                                      p))
                                                                                                                                                                (T.fromLitString
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p
                                                                                                                                                                   "=")
                                                                                                                                                                p))
                                                                                                                                                          (T.uap1
                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                             p
                                                                                                                                                             (greadsPrec
                                                                                                                                                                T.mkNoSrcPos
                                                                                                                                                                p)
                                                                                                                                                             (T.uap1
                                                                                                                                                                T.mkNoSrcPos
                                                                                                                                                                p
                                                                                                                                                                (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p)
                                                                                                                                                                (T.conInteger
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p
                                                                                                                                                                   (0)))))
                                                                                                                                                       (T.fromLitString
                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                          p
                                                                                                                                                          ",")
                                                                                                                                                       p))
                                                                                                                                                 (T.fromLitString
                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                    p
                                                                                                                                                    "ctPicosec")
                                                                                                                                                 p))
                                                                                                                                           (T.fromLitString
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p
                                                                                                                                              "=")
                                                                                                                                           p))
                                                                                                                                     (T.uap1
                                                                                                                                        T.mkNoSrcPos
                                                                                                                                        p
                                                                                                                                        (greadsPrec
                                                                                                                                           T.mkNoSrcPos
                                                                                                                                           p)
                                                                                                                                        (T.uap1
                                                                                                                                           T.mkNoSrcPos
                                                                                                                                           p
                                                                                                                                           (Hat.PreludeBasic.gfromInteger
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p)
                                                                                                                                           (T.conInteger
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p
                                                                                                                                              (0)))))
                                                                                                                                  (T.fromLitString
                                                                                                                                     T.mkNoSrcPos
                                                                                                                                     p
                                                                                                                                     ",")
                                                                                                                                  p))
                                                                                                                            (T.fromLitString
                                                                                                                               T.mkNoSrcPos
                                                                                                                               p
                                                                                                                               "ctWDay")
                                                                                                                            p))
                                                                                                                      (T.fromLitString
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         "=")
                                                                                                                      p))
                                                                                                                (T.uap1
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   (greadsPrec
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p)
                                                                                                                   (T.uap1
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p
                                                                                                                      (Hat.PreludeBasic.gfromInteger
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p)
                                                                                                                      (T.conInteger
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         (0)))))
                                                                                                             (T.fromLitString
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                ",")
                                                                                                             p))
                                                                                                       (T.fromLitString
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          "ctYDay")
                                                                                                       p))
                                                                                                 (T.fromLitString
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    "=")
                                                                                                 p))
                                                                                           (T.uap1
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              (greadsPrec
                                                                                                 T.mkNoSrcPos
                                                                                                 p)
                                                                                              (T.uap1
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 (Hat.PreludeBasic.gfromInteger
                                                                                                    T.mkNoSrcPos
                                                                                                    p)
                                                                                                 (T.conInteger
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    (0)))))
                                                                                        (T.fromLitString
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           ",")
                                                                                        p))
                                                                                  (T.fromLitString
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     "ctTZName")
                                                                                  p))
                                                                            (T.fromLitString
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               "=")
                                                                            p))
                                                                      (T.uap1
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         (greadsPrec
                                                                            T.mkNoSrcPos
                                                                            p)
                                                                         (T.uap1
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            (Hat.PreludeBasic.gfromInteger
                                                                               T.mkNoSrcPos
                                                                               p)
                                                                            (T.conInteger
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               (0)))))
                                                                   (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      ",")
                                                                   p))
                                                             (T.fromLitString
                                                                T.mkNoSrcPos
                                                                p
                                                                "ctTZ")
                                                             p))
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "=")
                                                       p))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (greadsPrec T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.gfromInteger
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.conInteger
                                                          T.mkNoSrcPos
                                                          p
                                                          (0)))))
                                              (T.fromLitString T.mkNoSrcPos p
                                                 ",")
                                              p))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "ctIsDST")
                                        p))
                                  (T.fromLitString T.mkNoSrcPos p "=")
                                  p))
                            (T.uap1 T.mkNoSrcPos p (greadsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (0)))))
                         (T.fromLitString T.mkNoSrcPos p "}")
                         p)
 
instance Show CalendarTime where
        gshowsPrec pshowsPrec p
          = T.ufun2 c24v36v24v39showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fy1
                  (T.R
                     (CalendarTime fy2 fy3 fy4 fy5 fy6 fy7 fy8 fy9 fy10
                        fy11 fy12 fy13)
                     _)
                  p
                  = T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                            (T.fromLitString T.mkNoSrcPos p "CalendarTime{"))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowString T.mkNoSrcPos p)
                                        (T.fromLitString T.mkNoSrcPos p
                                           "ctYear"))
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowChar T.mkNoSrcPos p)
                                        (T.conChar T.mkNoSrcPos p '=')))
                                  (T.uap2 T.mkNoSrcPos p
                                     (gshowsPrec T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gfromInteger
                                           T.mkNoSrcPos
                                           p)
                                        (T.conInteger T.mkNoSrcPos p (0)))
                                     fy2))
                               (T.uap1 T.mkNoSrcPos p (gshowChar T.mkNoSrcPos p)
                                  (T.conChar T.mkNoSrcPos p ',')))
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowString T.mkNoSrcPos p)
                                           (T.fromLitString T.mkNoSrcPos p
                                              "ctMonth"))
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowChar T.mkNoSrcPos p)
                                           (T.conChar T.mkNoSrcPos p '=')))
                                     (T.uap2 T.mkNoSrcPos p
                                        (gshowsPrec T.mkNoSrcPos p)
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gfromInteger
                                              T.mkNoSrcPos
                                              p)
                                           (T.conInteger T.mkNoSrcPos p (0)))
                                        fy3))
                                  (T.uap1 T.mkNoSrcPos p
                                     (gshowChar T.mkNoSrcPos p)
                                     (T.conChar T.mkNoSrcPos p ',')))
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowString T.mkNoSrcPos p)
                                              (T.fromLitString T.mkNoSrcPos p
                                                 "ctDay"))
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowChar T.mkNoSrcPos p)
                                              (T.conChar T.mkNoSrcPos p '=')))
                                        (T.uap2 T.mkNoSrcPos p
                                           (gshowsPrec T.mkNoSrcPos p)
                                           (T.uap1 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.gfromInteger
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.conInteger T.mkNoSrcPos p (0)))
                                           fy4))
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowChar T.mkNoSrcPos p)
                                        (T.conChar T.mkNoSrcPos p ',')))
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowString T.mkNoSrcPos p)
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "ctHour"))
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowChar T.mkNoSrcPos p)
                                                 (T.conChar T.mkNoSrcPos p
                                                    '=')))
                                           (T.uap2 T.mkNoSrcPos p
                                              (gshowsPrec T.mkNoSrcPos p)
                                              (T.uap1 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.gfromInteger
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.conInteger T.mkNoSrcPos p
                                                    (0)))
                                              fy5))
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowChar T.mkNoSrcPos p)
                                           (T.conChar T.mkNoSrcPos p ',')))
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowString T.mkNoSrcPos p)
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "ctMin"))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowChar T.mkNoSrcPos p)
                                                    (T.conChar T.mkNoSrcPos p
                                                       '=')))
                                              (T.uap2 T.mkNoSrcPos p
                                                 (gshowsPrec T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gfromInteger
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.conInteger T.mkNoSrcPos p
                                                       (0)))
                                                 fy6))
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowChar T.mkNoSrcPos p)
                                              (T.conChar T.mkNoSrcPos p ',')))
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((!.) T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (gshowString T.mkNoSrcPos
                                                          p)
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "ctSec"))
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (gshowChar T.mkNoSrcPos
                                                          p)
                                                       (T.conChar T.mkNoSrcPos p
                                                          '=')))
                                                 (T.uap2 T.mkNoSrcPos p
                                                    (gshowsPrec T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.gfromInteger
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.conInteger
                                                          T.mkNoSrcPos
                                                          p
                                                          (0)))
                                                    fy7))
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowChar T.mkNoSrcPos p)
                                                 (T.conChar T.mkNoSrcPos p
                                                    ',')))
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((!.) T.mkNoSrcPos p)
                                                    (T.uap2 T.mkNoSrcPos p
                                                       ((!.) T.mkNoSrcPos p)
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (gshowString
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.fromLitString
                                                             T.mkNoSrcPos
                                                             p
                                                             "ctPicosec"))
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (gshowChar
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.conChar
                                                             T.mkNoSrcPos
                                                             p
                                                             '=')))
                                                    (T.uap2 T.mkNoSrcPos p
                                                       (gshowsPrec T.mkNoSrcPos
                                                          p)
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.gfromInteger
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.conInteger
                                                             T.mkNoSrcPos
                                                             p
                                                             (0)))
                                                       fy8))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowChar T.mkNoSrcPos p)
                                                    (T.conChar T.mkNoSrcPos p
                                                       ',')))
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((!.) T.mkNoSrcPos p)
                                                    (T.uap2 T.mkNoSrcPos p
                                                       ((!.) T.mkNoSrcPos p)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((!.) T.mkNoSrcPos p)
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (gshowString
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.fromLitString
                                                                T.mkNoSrcPos
                                                                p
                                                                "ctWDay"))
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (gshowChar
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.conChar
                                                                T.mkNoSrcPos
                                                                p
                                                                '=')))
                                                       (T.uap2 T.mkNoSrcPos p
                                                          (gshowsPrec
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (Hat.PreludeBasic.gfromInteger
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.conInteger
                                                                T.mkNoSrcPos
                                                                p
                                                                (0)))
                                                          fy9))
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (gshowChar T.mkNoSrcPos
                                                          p)
                                                       (T.conChar T.mkNoSrcPos p
                                                          ',')))
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((!.) T.mkNoSrcPos p)
                                                    (T.uap2 T.mkNoSrcPos p
                                                       ((!.) T.mkNoSrcPos p)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((!.) T.mkNoSrcPos p)
                                                          (T.uap2 T.mkNoSrcPos p
                                                             ((!.) T.mkNoSrcPos
                                                                p)
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (gshowString
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.fromLitString
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   "ctYDay"))
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (gshowChar
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.conChar
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   '=')))
                                                          (T.uap2 T.mkNoSrcPos p
                                                             (gshowsPrec
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (Hat.PreludeBasic.gfromInteger
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.conInteger
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (0)))
                                                             fy10))
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (gshowChar
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.conChar
                                                             T.mkNoSrcPos
                                                             p
                                                             ',')))
                                                    (T.uap2 T.mkNoSrcPos p
                                                       ((!.) T.mkNoSrcPos p)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((!.) T.mkNoSrcPos p)
                                                          (T.uap2 T.mkNoSrcPos p
                                                             ((!.) T.mkNoSrcPos
                                                                p)
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                ((!.)
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (gshowString
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      "ctTZName"))
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (gshowChar
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.conChar
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      '=')))
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                (gshowsPrec
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.gfromInteger
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.conInteger
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (0)))
                                                                fy11))
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (gshowChar
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.conChar
                                                                T.mkNoSrcPos
                                                                p
                                                                ',')))
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((!.) T.mkNoSrcPos p)
                                                          (T.uap2 T.mkNoSrcPos p
                                                             ((!.) T.mkNoSrcPos
                                                                p)
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                ((!.)
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uap2
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   ((!.)
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.uap1
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (gshowString
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.fromLitString
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         "ctTZ"))
                                                                   (T.uap1
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (gshowChar
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.conChar
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         '=')))
                                                                (T.uap2
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (gshowsPrec
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.uap1
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (Hat.PreludeBasic.gfromInteger
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.conInteger
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         (0)))
                                                                   fy12))
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (gshowChar
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.conChar
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   ',')))
                                                          (T.uap2 T.mkNoSrcPos p
                                                             ((!.) T.mkNoSrcPos
                                                                p)
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                ((!.)
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (gshowString
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      "ctIsDST"))
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (gshowChar
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.conChar
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      '=')))
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                (gshowsPrec
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.gfromInteger
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.conInteger
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (0)))
                                                                fy13))))))))))))))
                      (T.uap1 T.mkNoSrcPos p (gshowChar T.mkNoSrcPos p)
                         (T.conChar T.mkNoSrcPos p '}'))
                hshowsPrec _ _ p = T.fatal p
 
data TimeDiff = TimeDiff{btdYear, btdMonth, btdDay,
                         btdHour, btdMin, btdSec :: T.R Int,
                         btdPicosec :: T.R Integer}
 
instance T.WrapVal TimeDiff where
        wrapVal pwrapVal
          kwrapVal@(TimeDiff (T.R _ z1wrapVal)
                      (T.R _ z2wrapVal) (T.R _ z3wrapVal) (T.R _ z4wrapVal)
                      (T.R _ z5wrapVal) (T.R _ z6wrapVal)
                      (T.R _ z7wrapVal))
          p
          = T.R kwrapVal
              (T.mkValueApp7 p pwrapVal aTimeDiff z1wrapVal
                 z2wrapVal
                 z3wrapVal
                 z4wrapVal
                 z5wrapVal
                 z6wrapVal
                 z7wrapVal)
gtdYear ptdYear p = T.ufun1 atdYear ptdYear p htdYear
htdYear (T.R z1tdYear _) p
  = T.projection T.mkNoSrcPos p (btdYear z1tdYear)
gtdMonth ptdMonth p
  = T.ufun1 atdMonth ptdMonth p htdMonth
htdMonth (T.R z1tdMonth _) p
  = T.projection T.mkNoSrcPos p (btdMonth z1tdMonth)
gtdDay ptdDay p = T.ufun1 atdDay ptdDay p htdDay
htdDay (T.R z1tdDay _) p
  = T.projection T.mkNoSrcPos p (btdDay z1tdDay)
gtdHour ptdHour p = T.ufun1 atdHour ptdHour p htdHour
htdHour (T.R z1tdHour _) p
  = T.projection T.mkNoSrcPos p (btdHour z1tdHour)
gtdMin ptdMin p = T.ufun1 atdMin ptdMin p htdMin
htdMin (T.R z1tdMin _) p
  = T.projection T.mkNoSrcPos p (btdMin z1tdMin)
gtdSec ptdSec p = T.ufun1 atdSec ptdSec p htdSec
htdSec (T.R z1tdSec _) p
  = T.projection T.mkNoSrcPos p (btdSec z1tdSec)
gtdPicosec ptdPicosec p
  = T.ufun1 atdPicosec ptdPicosec p htdPicosec
htdPicosec (T.R z1tdPicosec _) p
  = T.projection T.mkNoSrcPos p
      (btdPicosec z1tdPicosec)
 
instance Eq TimeDiff where
        (%==) !== p = T.ufun2 (+$>=$#=$>=$$==) (%==) p (*==)
          where (T.R (TimeDiff fy1 fy2 fy3 fy4 fy5 fy6 fy7) _
                   *==
                   T.R (TimeDiff fy8 fy9 fy10 fy11 fy12 fy13 fy14) _)
                  p
                  = T.uwrapForward p
                      ((Hat.PreludeBasic.*&&)
                         (T.uap2 T.mkNoSrcPos p
                            ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                            fy1
                            fy8)
                         (T.uwrapForward p
                            ((Hat.PreludeBasic.*&&)
                               (T.uap2 T.mkNoSrcPos p
                                  ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                  fy2
                                  fy9)
                               (T.uwrapForward p
                                  ((Hat.PreludeBasic.*&&)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                        fy3
                                        fy10)
                                     (T.uwrapForward p
                                        ((Hat.PreludeBasic.*&&)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((Hat.PreludeBasic.!==)
                                                 T.mkNoSrcPos
                                                 p)
                                              fy4
                                              fy11)
                                           (T.uwrapForward p
                                              ((Hat.PreludeBasic.*&&)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((Hat.PreludeBasic.!==)
                                                       T.mkNoSrcPos
                                                       p)
                                                    fy5
                                                    fy12)
                                                 (T.uwrapForward p
                                                    ((Hat.PreludeBasic.*&&)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          ((Hat.PreludeBasic.!==)
                                                             T.mkNoSrcPos
                                                             p)
                                                          fy6
                                                          fy13)
                                                       (T.uwrapForward p
                                                          ((Hat.PreludeBasic.*&&)
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                ((Hat.PreludeBasic.!==)
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                fy7
                                                                fy14)
                                                             (T.con0
                                                                T.mkNoSrcPos
                                                                p
                                                                Hat.PreludeBuiltinTypes.True
                                                                Hat.PreludeBuiltinTypes.aTrue)
                                                             p))
                                                       p))
                                                 p))
                                           p))
                                     p))
                               p))
                         p)
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord TimeDiff where
        gcompare pcompare p
          = T.ufun2 c29v25v29v27compare pcompare p hcompare
          where hcompare
                  (T.R (TimeDiff fy3 fy4 fy5 fy6 fy7 fy8 fy9) _)
                  (T.R (TimeDiff fy10 fy11 fy12 fy13 fy14 fy15 fy16) _)
                  p
                  = T.uccase T.mkNoSrcPos p
                      (let v29v25v29v27v1 (T.R EQ _) p
                             = T.uccase T.mkNoSrcPos p
                                 (let v29v25v29v27v1 (T.R EQ _) p
                                        = T.uccase T.mkNoSrcPos p
                                            (let v29v25v29v27v1 (T.R EQ _) p
                                                   = T.uccase T.mkNoSrcPos p
                                                       (let v29v25v29v27v1
                                                              (T.R EQ _) p
                                                              = T.uccase
                                                                  T.mkNoSrcPos
                                                                  p
                                                                  (let v29v25v29v27v1
                                                                         (T.R EQ
                                                                            _)
                                                                         p
                                                                         = T.uccase
                                                                             T.mkNoSrcPos
                                                                             p
                                                                             (let v29v25v29v27v1
                                                                                    (T.R
                                                                                       EQ
                                                                                       _)
                                                                                    p
                                                                                    = T.uap2
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        (gcompare
                                                                                           T.mkNoSrcPos
                                                                                           p)
                                                                                        fy9
                                                                                        fy16
                                                                                  v29v25v29v27v1
                                                                                    fy1
                                                                                    p
                                                                                    = T.projection
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        fy1
                                                                                in
                                                                                v29v25v29v27v1)
                                                                             (T.uap2
                                                                                T.mkNoSrcPos
                                                                                p
                                                                                (gcompare
                                                                                   T.mkNoSrcPos
                                                                                   p)
                                                                                fy8
                                                                                fy15)
                                                                       v29v25v29v27v1
                                                                         fy1 p
                                                                         = T.projection
                                                                             T.mkNoSrcPos
                                                                             p
                                                                             fy1
                                                                     in
                                                                     v29v25v29v27v1)
                                                                  (T.uap2
                                                                     T.mkNoSrcPos
                                                                     p
                                                                     (gcompare
                                                                        T.mkNoSrcPos
                                                                        p)
                                                                     fy7
                                                                     fy14)
                                                            v29v25v29v27v1 fy1 p
                                                              = T.projection
                                                                  T.mkNoSrcPos
                                                                  p
                                                                  fy1
                                                          in v29v25v29v27v1)
                                                       (T.uap2 T.mkNoSrcPos p
                                                          (gcompare T.mkNoSrcPos
                                                             p)
                                                          fy6
                                                          fy13)
                                                 v29v25v29v27v1 fy1 p
                                                   = T.projection T.mkNoSrcPos p
                                                       fy1
                                               in v29v25v29v27v1)
                                            (T.uap2 T.mkNoSrcPos p
                                               (gcompare T.mkNoSrcPos p)
                                               fy5
                                               fy12)
                                      v29v25v29v27v1 fy1 p
                                        = T.projection T.mkNoSrcPos p fy1
                                    in v29v25v29v27v1)
                                 (T.uap2 T.mkNoSrcPos p
                                    (gcompare T.mkNoSrcPos p)
                                    fy4
                                    fy11)
                           v29v25v29v27v1 fy1 p
                             = T.projection T.mkNoSrcPos p fy1
                         in v29v25v29v27v1)
                      (T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p) fy3
                         fy10)
                hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c29v25v29v27localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c29v25v29v27localFromEnum
                        hlocalFromEnum (T.R (TimeDiff _ _ _ _ _ _ _) _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Read TimeDiff where
        greadsPrec preadsPrec p
          = T.ufun1 c29v30v29v33readsPrec preadsPrec p
              hreadsPrec
          where hreadsPrec fy1 p
                  = T.uwrapForward p
                      (Hat.PreludeBasic.hthenLex
                         (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hthenLex
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uwrapForward p
                                           (Hat.PreludeBasic.hthenLex
                                              (T.uap2 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.gthenAp
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.uwrapForward p
                                                    (Hat.PreludeBasic.hthenLex
                                                       (T.uwrapForward p
                                                          (Hat.PreludeBasic.hthenLex
                                                             (T.uwrapForward p
                                                                (Hat.PreludeBasic.hthenLex
                                                                   (T.uap2
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (Hat.PreludeBasic.gthenAp
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.uwrapForward
                                                                         p
                                                                         (Hat.PreludeBasic.hthenLex
                                                                            (T.uwrapForward
                                                                               p
                                                                               (Hat.PreludeBasic.hthenLex
                                                                                  (T.uwrapForward
                                                                                     p
                                                                                     (Hat.PreludeBasic.hthenLex
                                                                                        (T.uap2
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           (Hat.PreludeBasic.gthenAp
                                                                                              T.mkNoSrcPos
                                                                                              p)
                                                                                           (T.uwrapForward
                                                                                              p
                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                 (T.uwrapForward
                                                                                                    p
                                                                                                    (Hat.PreludeBasic.hthenLex
                                                                                                       (T.uwrapForward
                                                                                                          p
                                                                                                          (Hat.PreludeBasic.hthenLex
                                                                                                             (T.uap2
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                (Hat.PreludeBasic.gthenAp
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p)
                                                                                                                (T.uwrapForward
                                                                                                                   p
                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                      (T.uwrapForward
                                                                                                                         p
                                                                                                                         (Hat.PreludeBasic.hthenLex
                                                                                                                            (T.uwrapForward
                                                                                                                               p
                                                                                                                               (Hat.PreludeBasic.hthenLex
                                                                                                                                  (T.uap2
                                                                                                                                     T.mkNoSrcPos
                                                                                                                                     p
                                                                                                                                     (Hat.PreludeBasic.gthenAp
                                                                                                                                        T.mkNoSrcPos
                                                                                                                                        p)
                                                                                                                                     (T.uwrapForward
                                                                                                                                        p
                                                                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                                                                           (T.uwrapForward
                                                                                                                                              p
                                                                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                                                                 (T.uwrapForward
                                                                                                                                                    p
                                                                                                                                                    (Hat.PreludeBasic.hthenLex
                                                                                                                                                       (T.uap2
                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                          p
                                                                                                                                                          (Hat.PreludeBasic.gthenAp
                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                             p)
                                                                                                                                                          (T.uwrapForward
                                                                                                                                                             p
                                                                                                                                                             (Hat.PreludeBasic.hthenLex
                                                                                                                                                                (T.uwrapForward
                                                                                                                                                                   p
                                                                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                                                                      (T.uwrapForward
                                                                                                                                                                         p
                                                                                                                                                                         (Hat.PreludeBasic.hthenLex
                                                                                                                                                                            (T.uwrapForward
                                                                                                                                                                               p
                                                                                                                                                                               (Hat.PreludeBasic.hthenLex
                                                                                                                                                                                  (T.uap1
                                                                                                                                                                                     T.mkNoSrcPos
                                                                                                                                                                                     p
                                                                                                                                                                                     (Hat.PreludeBasic.gyield
                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                        p)
                                                                                                                                                                                     (T.pa0
                                                                                                                                                                                        TimeDiff
                                                                                                                                                                                        T.cn7
                                                                                                                                                                                        T.mkNoSrcPos
                                                                                                                                                                                        p
                                                                                                                                                                                        aTimeDiff))
                                                                                                                                                                                  (T.fromLitString
                                                                                                                                                                                     T.mkNoSrcPos
                                                                                                                                                                                     p
                                                                                                                                                                                     "TimeDiff")
                                                                                                                                                                                  p))
                                                                                                                                                                            (T.fromLitString
                                                                                                                                                                               T.mkNoSrcPos
                                                                                                                                                                               p
                                                                                                                                                                               "{")
                                                                                                                                                                            p))
                                                                                                                                                                      (T.fromLitString
                                                                                                                                                                         T.mkNoSrcPos
                                                                                                                                                                         p
                                                                                                                                                                         "tdYear")
                                                                                                                                                                      p))
                                                                                                                                                                (T.fromLitString
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p
                                                                                                                                                                   "=")
                                                                                                                                                                p))
                                                                                                                                                          (T.uap1
                                                                                                                                                             T.mkNoSrcPos
                                                                                                                                                             p
                                                                                                                                                             (greadsPrec
                                                                                                                                                                T.mkNoSrcPos
                                                                                                                                                                p)
                                                                                                                                                             (T.uap1
                                                                                                                                                                T.mkNoSrcPos
                                                                                                                                                                p
                                                                                                                                                                (Hat.PreludeBasic.gfromInteger
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p)
                                                                                                                                                                (T.conInteger
                                                                                                                                                                   T.mkNoSrcPos
                                                                                                                                                                   p
                                                                                                                                                                   (0)))))
                                                                                                                                                       (T.fromLitString
                                                                                                                                                          T.mkNoSrcPos
                                                                                                                                                          p
                                                                                                                                                          ",")
                                                                                                                                                       p))
                                                                                                                                                 (T.fromLitString
                                                                                                                                                    T.mkNoSrcPos
                                                                                                                                                    p
                                                                                                                                                    "tdMonth")
                                                                                                                                                 p))
                                                                                                                                           (T.fromLitString
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p
                                                                                                                                              "=")
                                                                                                                                           p))
                                                                                                                                     (T.uap1
                                                                                                                                        T.mkNoSrcPos
                                                                                                                                        p
                                                                                                                                        (greadsPrec
                                                                                                                                           T.mkNoSrcPos
                                                                                                                                           p)
                                                                                                                                        (T.uap1
                                                                                                                                           T.mkNoSrcPos
                                                                                                                                           p
                                                                                                                                           (Hat.PreludeBasic.gfromInteger
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p)
                                                                                                                                           (T.conInteger
                                                                                                                                              T.mkNoSrcPos
                                                                                                                                              p
                                                                                                                                              (0)))))
                                                                                                                                  (T.fromLitString
                                                                                                                                     T.mkNoSrcPos
                                                                                                                                     p
                                                                                                                                     ",")
                                                                                                                                  p))
                                                                                                                            (T.fromLitString
                                                                                                                               T.mkNoSrcPos
                                                                                                                               p
                                                                                                                               "tdDay")
                                                                                                                            p))
                                                                                                                      (T.fromLitString
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         "=")
                                                                                                                      p))
                                                                                                                (T.uap1
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   (greadsPrec
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p)
                                                                                                                   (T.uap1
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p
                                                                                                                      (Hat.PreludeBasic.gfromInteger
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p)
                                                                                                                      (T.conInteger
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         (0)))))
                                                                                                             (T.fromLitString
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                ",")
                                                                                                             p))
                                                                                                       (T.fromLitString
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          "tdHour")
                                                                                                       p))
                                                                                                 (T.fromLitString
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    "=")
                                                                                                 p))
                                                                                           (T.uap1
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              (greadsPrec
                                                                                                 T.mkNoSrcPos
                                                                                                 p)
                                                                                              (T.uap1
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 (Hat.PreludeBasic.gfromInteger
                                                                                                    T.mkNoSrcPos
                                                                                                    p)
                                                                                                 (T.conInteger
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    (0)))))
                                                                                        (T.fromLitString
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           ",")
                                                                                        p))
                                                                                  (T.fromLitString
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     "tdMin")
                                                                                  p))
                                                                            (T.fromLitString
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               "=")
                                                                            p))
                                                                      (T.uap1
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         (greadsPrec
                                                                            T.mkNoSrcPos
                                                                            p)
                                                                         (T.uap1
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            (Hat.PreludeBasic.gfromInteger
                                                                               T.mkNoSrcPos
                                                                               p)
                                                                            (T.conInteger
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               (0)))))
                                                                   (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      ",")
                                                                   p))
                                                             (T.fromLitString
                                                                T.mkNoSrcPos
                                                                p
                                                                "tdSec")
                                                             p))
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "=")
                                                       p))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (greadsPrec T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.gfromInteger
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.conInteger
                                                          T.mkNoSrcPos
                                                          p
                                                          (0)))))
                                              (T.fromLitString T.mkNoSrcPos p
                                                 ",")
                                              p))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "tdPicosec")
                                        p))
                                  (T.fromLitString T.mkNoSrcPos p "=")
                                  p))
                            (T.uap1 T.mkNoSrcPos p (greadsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (0)))))
                         (T.fromLitString T.mkNoSrcPos p "}")
                         p)
 
instance Show TimeDiff where
        gshowsPrec pshowsPrec p
          = T.ufun2 c29v36v29v39showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fy1
                  (T.R (TimeDiff fy2 fy3 fy4 fy5 fy6 fy7 fy8) _) p
                  = T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                            (T.fromLitString T.mkNoSrcPos p "TimeDiff{"))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowString T.mkNoSrcPos p)
                                        (T.fromLitString T.mkNoSrcPos p
                                           "tdYear"))
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowChar T.mkNoSrcPos p)
                                        (T.conChar T.mkNoSrcPos p '=')))
                                  (T.uap2 T.mkNoSrcPos p
                                     (gshowsPrec T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gfromInteger
                                           T.mkNoSrcPos
                                           p)
                                        (T.conInteger T.mkNoSrcPos p (0)))
                                     fy2))
                               (T.uap1 T.mkNoSrcPos p (gshowChar T.mkNoSrcPos p)
                                  (T.conChar T.mkNoSrcPos p ',')))
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowString T.mkNoSrcPos p)
                                           (T.fromLitString T.mkNoSrcPos p
                                              "tdMonth"))
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowChar T.mkNoSrcPos p)
                                           (T.conChar T.mkNoSrcPos p '=')))
                                     (T.uap2 T.mkNoSrcPos p
                                        (gshowsPrec T.mkNoSrcPos p)
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gfromInteger
                                              T.mkNoSrcPos
                                              p)
                                           (T.conInteger T.mkNoSrcPos p (0)))
                                        fy3))
                                  (T.uap1 T.mkNoSrcPos p
                                     (gshowChar T.mkNoSrcPos p)
                                     (T.conChar T.mkNoSrcPos p ',')))
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowString T.mkNoSrcPos p)
                                              (T.fromLitString T.mkNoSrcPos p
                                                 "tdDay"))
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowChar T.mkNoSrcPos p)
                                              (T.conChar T.mkNoSrcPos p '=')))
                                        (T.uap2 T.mkNoSrcPos p
                                           (gshowsPrec T.mkNoSrcPos p)
                                           (T.uap1 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.gfromInteger
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.conInteger T.mkNoSrcPos p (0)))
                                           fy4))
                                     (T.uap1 T.mkNoSrcPos p
                                        (gshowChar T.mkNoSrcPos p)
                                        (T.conChar T.mkNoSrcPos p ',')))
                                  (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowString T.mkNoSrcPos p)
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "tdHour"))
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowChar T.mkNoSrcPos p)
                                                 (T.conChar T.mkNoSrcPos p
                                                    '=')))
                                           (T.uap2 T.mkNoSrcPos p
                                              (gshowsPrec T.mkNoSrcPos p)
                                              (T.uap1 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.gfromInteger
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.conInteger T.mkNoSrcPos p
                                                    (0)))
                                              fy5))
                                        (T.uap1 T.mkNoSrcPos p
                                           (gshowChar T.mkNoSrcPos p)
                                           (T.conChar T.mkNoSrcPos p ',')))
                                     (T.uap2 T.mkNoSrcPos p
                                        ((!.) T.mkNoSrcPos p)
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowString T.mkNoSrcPos p)
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "tdMin"))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowChar T.mkNoSrcPos p)
                                                    (T.conChar T.mkNoSrcPos p
                                                       '=')))
                                              (T.uap2 T.mkNoSrcPos p
                                                 (gshowsPrec T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gfromInteger
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.conInteger T.mkNoSrcPos p
                                                       (0)))
                                                 fy6))
                                           (T.uap1 T.mkNoSrcPos p
                                              (gshowChar T.mkNoSrcPos p)
                                              (T.conChar T.mkNoSrcPos p ',')))
                                        (T.uap2 T.mkNoSrcPos p
                                           ((!.) T.mkNoSrcPos p)
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap2 T.mkNoSrcPos p
                                                    ((!.) T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (gshowString T.mkNoSrcPos
                                                          p)
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "tdSec"))
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (gshowChar T.mkNoSrcPos
                                                          p)
                                                       (T.conChar T.mkNoSrcPos p
                                                          '=')))
                                                 (T.uap2 T.mkNoSrcPos p
                                                    (gshowsPrec T.mkNoSrcPos p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.gfromInteger
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.conInteger
                                                          T.mkNoSrcPos
                                                          p
                                                          (0)))
                                                    fy7))
                                              (T.uap1 T.mkNoSrcPos p
                                                 (gshowChar T.mkNoSrcPos p)
                                                 (T.conChar T.mkNoSrcPos p
                                                    ',')))
                                           (T.uap2 T.mkNoSrcPos p
                                              ((!.) T.mkNoSrcPos p)
                                              (T.uap2 T.mkNoSrcPos p
                                                 ((!.) T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowString T.mkNoSrcPos p)
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "tdPicosec"))
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (gshowChar T.mkNoSrcPos p)
                                                    (T.conChar T.mkNoSrcPos p
                                                       '=')))
                                              (T.uap2 T.mkNoSrcPos p
                                                 (gshowsPrec T.mkNoSrcPos p)
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gfromInteger
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.conInteger T.mkNoSrcPos p
                                                       (0)))
                                                 fy8)))))))))
                      (T.uap1 T.mkNoSrcPos p (gshowChar T.mkNoSrcPos p)
                         (T.conChar T.mkNoSrcPos p '}'))
                hshowsPrec _ _ p = T.fatal p
aApril
  = T.mkConstructor tTimeBuiltinTypes 50049 50053 3 (0)
      "April"
aAugust
  = T.mkConstructor tTimeBuiltinTypes 60049 60054 3 (0)
      "August"
aCalendarTime
  = T.mkConstructorWFields tTimeBuiltinTypes 140021
      140032
      3
      (12)
      "CalendarTime"
      ((:) actYear
         ((:) actMonth
            ((:) actDay
               ((:) actHour
                  ((:) actMin
                     ((:) actSec
                        ((:) actPicosec
                           ((:) actWDay
                              ((:) actYDay
                                 ((:) actTZName
                                    ((:) actTZ ((:) actIsDST []))))))))))))
aDecember
  = T.mkConstructor tTimeBuiltinTypes 70049 70056 3 (0)
      "December"
aFebruary
  = T.mkConstructor tTimeBuiltinTypes 50027 50034 3 (0)
      "February"
aFriday
  = T.mkConstructor tTimeBuiltinTypes 110015 110020 3
      (0)
      "Friday"
aJanuary
  = T.mkConstructor tTimeBuiltinTypes 50015 50021 3 (0)
      "January"
aJuly
  = T.mkConstructor tTimeBuiltinTypes 60038 60041 3 (0)
      "July"
aJune
  = T.mkConstructor tTimeBuiltinTypes 60027 60030 3 (0)
      "June"
aMarch
  = T.mkConstructor tTimeBuiltinTypes 50038 50042 3 (0)
      "March"
aMay
  = T.mkConstructor tTimeBuiltinTypes 60015 60017 3 (0)
      "May"
aMonday
  = T.mkConstructor tTimeBuiltinTypes 100024 100029 3
      (0)
      "Monday"
aNovember
  = T.mkConstructor tTimeBuiltinTypes 70038 70045 3 (0)
      "November"
aOctober
  = T.mkConstructor tTimeBuiltinTypes 70027 70033 3 (0)
      "October"
aSaturday
  = T.mkConstructor tTimeBuiltinTypes 110024 110031 3
      (0)
      "Saturday"
aSeptember
  = T.mkConstructor tTimeBuiltinTypes 70015 70023 3 (0)
      "September"
aSunday
  = T.mkConstructor tTimeBuiltinTypes 100015 100020 3
      (0)
      "Sunday"
aThursday
  = T.mkConstructor tTimeBuiltinTypes 100057 100064 3
      (0)
      "Thursday"
aTimeDiff
  = T.mkConstructorWFields tTimeBuiltinTypes 260017
      260024
      3
      (7)
      "TimeDiff"
      ((:) atdYear
         ((:) atdMonth
            ((:) atdDay
               ((:) atdHour
                  ((:) atdMin ((:) atdSec ((:) atdPicosec [])))))))
aTuesday
  = T.mkConstructor tTimeBuiltinTypes 100034 100040 3
      (0)
      "Tuesday"
aWednesday
  = T.mkConstructor tTimeBuiltinTypes 100045 100053 3
      (0)
      "Wednesday"
actDay
  = T.mkVariable tTimeBuiltinTypes 170017 170021 3 (1)
      "ctDay"
      Prelude.False
actHour
  = T.mkVariable tTimeBuiltinTypes 170024 170029 3 (1)
      "ctHour"
      Prelude.False
actIsDST
  = T.mkVariable tTimeBuiltinTypes 230017 230023 3 (1)
      "ctIsDST"
      Prelude.False
actMin
  = T.mkVariable tTimeBuiltinTypes 170032 170036 3 (1)
      "ctMin"
      Prelude.False
actMonth
  = T.mkVariable tTimeBuiltinTypes 160017 160023 3 (1)
      "ctMonth"
      Prelude.False
actPicosec
  = T.mkVariable tTimeBuiltinTypes 180017 180025 3 (1)
      "ctPicosec"
      Prelude.False
actSec
  = T.mkVariable tTimeBuiltinTypes 170039 170043 3 (1)
      "ctSec"
      Prelude.False
actTZ
  = T.mkVariable tTimeBuiltinTypes 220017 220020 3 (1)
      "ctTZ"
      Prelude.False
actTZName
  = T.mkVariable tTimeBuiltinTypes 210017 210024 3 (1)
      "ctTZName"
      Prelude.False
actWDay
  = T.mkVariable tTimeBuiltinTypes 190017 190022 3 (1)
      "ctWDay"
      Prelude.False
actYDay
  = T.mkVariable tTimeBuiltinTypes 200017 200022 3 (1)
      "ctYDay"
      Prelude.False
actYear
  = T.mkVariable tTimeBuiltinTypes 150017 150022 3 (1)
      "ctYear"
      Prelude.False
atdDay
  = T.mkVariable tTimeBuiltinTypes 270034 270038 3 (1)
      "tdDay"
      Prelude.False
atdHour
  = T.mkVariable tTimeBuiltinTypes 270041 270046 3 (1)
      "tdHour"
      Prelude.False
atdMin
  = T.mkVariable tTimeBuiltinTypes 270049 270053 3 (1)
      "tdMin"
      Prelude.False
atdMonth
  = T.mkVariable tTimeBuiltinTypes 270025 270031 3 (1)
      "tdMonth"
      Prelude.False
atdPicosec
  = T.mkVariable tTimeBuiltinTypes 280017 280025 3 (1)
      "tdPicosec"
      Prelude.False
atdSec
  = T.mkVariable tTimeBuiltinTypes 270056 270060 3 (1)
      "tdSec"
      Prelude.False
atdYear
  = T.mkVariable tTimeBuiltinTypes 270017 270022 3 (1)
      "tdYear"
      Prelude.False
(+@=$$=@=$%==)
  = T.mkVariable tTimeBuiltinTypes 80022 80023 3 (-1)
      "=="
      Prelude.False
c8v26v8v28localFromEnum
  = T.mkVariable tTimeBuiltinTypes 80026 80028 3 (1)
      "localFromEnum"
      Prelude.True
c8v26v8v28compare
  = T.mkVariable tTimeBuiltinTypes 80026 80028 3 (-1)
      "compare"
      Prelude.False
c8v31v8v34enumFrom
  = T.mkVariable tTimeBuiltinTypes 80031 80034 3 (-1)
      "enumFrom"
      Prelude.False
c8v31v8v34enumFromThen
  = T.mkVariable tTimeBuiltinTypes 80031 80034 3 (-1)
      "enumFromThen"
      Prelude.False
c8v31v8v34fromEnum
  = T.mkVariable tTimeBuiltinTypes 80031 80034 3 (-1)
      "fromEnum"
      Prelude.False
c8v31v8v34toEnum
  = T.mkVariable tTimeBuiltinTypes 80031 80034 3 (-1)
      "toEnum"
      Prelude.False
c8v37v8v43maxBound
  = T.mkVariable tTimeBuiltinTypes 80037 80043 3 (-1)
      "maxBound"
      Prelude.False
c8v37v8v43minBound
  = T.mkVariable tTimeBuiltinTypes 80037 80043 3 (-1)
      "minBound"
      Prelude.False
c8v46v8v47rfromEnum
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (1)
      "rfromEnum"
      Prelude.True
c8v46v8v47rtoEnum
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (1)
      "rtoEnum"
      Prelude.True
c8v46v8v47ifromEnum
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (1)
      "ifromEnum"
      Prelude.True
c8v46v8v47nfromEnum
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (1)
      "nfromEnum"
      Prelude.True
c8v46v8v47inRange
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (-1)
      "inRange"
      Prelude.False
c8v46v8v47index
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (-1)
      "index"
      Prelude.False
c8v46v8v47range
  = T.mkVariable tTimeBuiltinTypes 80046 80047 3 (-1)
      "range"
      Prelude.False
c8v50v8v53readsPrec
  = T.mkVariable tTimeBuiltinTypes 80050 80053 3 (-1)
      "readsPrec"
      Prelude.False
c8v56v8v59showsPrec
  = T.mkVariable tTimeBuiltinTypes 80056 80059 3 (-1)
      "showsPrec"
      Prelude.False
(+#$=$$=#$=$%==)
  = T.mkVariable tTimeBuiltinTypes 120022 120023 3 (-1)
      "=="
      Prelude.False
c12v26v12v28localFromEnum
  = T.mkVariable tTimeBuiltinTypes 120026 120028 3 (1)
      "localFromEnum"
      Prelude.True
c12v26v12v28compare
  = T.mkVariable tTimeBuiltinTypes 120026 120028 3 (-1)
      "compare"
      Prelude.False
c12v31v12v34enumFrom
  = T.mkVariable tTimeBuiltinTypes 120031 120034 3 (-1)
      "enumFrom"
      Prelude.False
c12v31v12v34enumFromThen
  = T.mkVariable tTimeBuiltinTypes 120031 120034 3 (-1)
      "enumFromThen"
      Prelude.False
c12v31v12v34fromEnum
  = T.mkVariable tTimeBuiltinTypes 120031 120034 3 (-1)
      "fromEnum"
      Prelude.False
c12v31v12v34toEnum
  = T.mkVariable tTimeBuiltinTypes 120031 120034 3 (-1)
      "toEnum"
      Prelude.False
c12v37v12v43maxBound
  = T.mkVariable tTimeBuiltinTypes 120037 120043 3 (-1)
      "maxBound"
      Prelude.False
c12v37v12v43minBound
  = T.mkVariable tTimeBuiltinTypes 120037 120043 3 (-1)
      "minBound"
      Prelude.False
c12v46v12v47rfromEnum
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (1)
      "rfromEnum"
      Prelude.True
c12v46v12v47rtoEnum
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (1)
      "rtoEnum"
      Prelude.True
c12v46v12v47ifromEnum
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (1)
      "ifromEnum"
      Prelude.True
c12v46v12v47nfromEnum
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (1)
      "nfromEnum"
      Prelude.True
c12v46v12v47inRange
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (-1)
      "inRange"
      Prelude.False
c12v46v12v47index
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (-1)
      "index"
      Prelude.False
c12v46v12v47range
  = T.mkVariable tTimeBuiltinTypes 120046 120047 3 (-1)
      "range"
      Prelude.False
c12v50v12v53readsPrec
  = T.mkVariable tTimeBuiltinTypes 120050 120053 3 (-1)
      "readsPrec"
      Prelude.False
c12v56v12v59showsPrec
  = T.mkVariable tTimeBuiltinTypes 120056 120059 3 (-1)
      "showsPrec"
      Prelude.False
(+$&=$#=$&=$$==)
  = T.mkVariable tTimeBuiltinTypes 240021 240022 3 (-1)
      "=="
      Prelude.False
c24v25v24v27localFromEnum
  = T.mkVariable tTimeBuiltinTypes 240025 240027 3 (1)
      "localFromEnum"
      Prelude.True
c24v25v24v27compare
  = T.mkVariable tTimeBuiltinTypes 240025 240027 3 (-1)
      "compare"
      Prelude.False
c24v30v24v33readsPrec
  = T.mkVariable tTimeBuiltinTypes 240030 240033 3 (-1)
      "readsPrec"
      Prelude.False
c24v36v24v39showsPrec
  = T.mkVariable tTimeBuiltinTypes 240036 240039 3 (-1)
      "showsPrec"
      Prelude.False
(+$>=$#=$>=$$==)
  = T.mkVariable tTimeBuiltinTypes 290021 290022 3 (-1)
      "=="
      Prelude.False
c29v25v29v27localFromEnum
  = T.mkVariable tTimeBuiltinTypes 290025 290027 3 (1)
      "localFromEnum"
      Prelude.True
c29v25v29v27compare
  = T.mkVariable tTimeBuiltinTypes 290025 290027 3 (-1)
      "compare"
      Prelude.False
c29v30v29v33readsPrec
  = T.mkVariable tTimeBuiltinTypes 290030 290033 3 (-1)
      "readsPrec"
      Prelude.False
c29v36v29v39showsPrec
  = T.mkVariable tTimeBuiltinTypes 290036 290039 3 (-1)
      "showsPrec"
      Prelude.False
p = T.mkRoot
tTimeBuiltinTypes
  = T.mkModule "TimeBuiltinTypes" "TimeBuiltinTypes.hs"
      Prelude.False