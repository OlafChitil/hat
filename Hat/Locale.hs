module Hat.Locale
  (TimeLocale(TimeLocale,bwDays,bmonths,bamPm,bdateTimeFmt,bdateFmt,btimeFmt
      ,btime12Fmt),gwDays,gmonths,gamPm,gdateTimeFmt,gdateFmt,gtimeFmt
    ,gtime12Fmt,hwDays,hmonths,hamPm,hdateTimeFmt,hdateFmt,htimeFmt,htime12Fmt
    ,awDays,amonths,aamPm,adateTimeFmt,adateFmt,atimeFmt,atime12Fmt,aTimeLocale
    ,gdefaultTimeLocale) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

data TimeLocale =
  TimeLocale
    {bwDays :: T.R (T.List (T.Tuple2 String String))
      ,bmonths :: T.R (T.List (T.Tuple2 String String))
      ,bamPm :: T.R (T.Tuple2 String String)
      ,bdateTimeFmt,bdateFmt,btimeFmt,btime12Fmt :: T.R String}

instance T.WrapVal (TimeLocale)
  where
  
  wrapVal pwrapVal
    (kwrapVal@(TimeLocale (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)
        (T.R _ z4wrapVal) (T.R _ z5wrapVal) (T.R _ z6wrapVal)
        (T.R _ z7wrapVal))) p =
    T.R kwrapVal
      (T.mkValueApp7 p pwrapVal aTimeLocale z1wrapVal z2wrapVal z3wrapVal
        z4wrapVal z5wrapVal z6wrapVal z7wrapVal)
  

gwDays pwDays p = T.ufun1 awDays pwDays p hwDays

hwDays (T.R z1wDays _) p = T.projection T.mkNoSrcPos p (bwDays z1wDays)

gmonths pmonths p = T.ufun1 amonths pmonths p hmonths

hmonths (T.R z1months _) p = T.projection T.mkNoSrcPos p (bmonths z1months)

gamPm pamPm p = T.ufun1 aamPm pamPm p hamPm

hamPm (T.R z1amPm _) p = T.projection T.mkNoSrcPos p (bamPm z1amPm)

gdateTimeFmt pdateTimeFmt p = T.ufun1 adateTimeFmt pdateTimeFmt p hdateTimeFmt

hdateTimeFmt (T.R z1dateTimeFmt _) p =
  T.projection T.mkNoSrcPos p (bdateTimeFmt z1dateTimeFmt)

gdateFmt pdateFmt p = T.ufun1 adateFmt pdateFmt p hdateFmt

hdateFmt (T.R z1dateFmt _) p = T.projection T.mkNoSrcPos p (bdateFmt z1dateFmt)

gtimeFmt ptimeFmt p = T.ufun1 atimeFmt ptimeFmt p htimeFmt

htimeFmt (T.R z1timeFmt _) p = T.projection T.mkNoSrcPos p (btimeFmt z1timeFmt)

gtime12Fmt ptime12Fmt p = T.ufun1 atime12Fmt ptime12Fmt p htime12Fmt

htime12Fmt (T.R z1time12Fmt _) p =
  T.projection T.mkNoSrcPos p (btime12Fmt z1time12Fmt)

instance Eq (TimeLocale)
  where
  
  (!==) (%==) p =
    T.ufun2 (+##=$#=##=$$==) (%==) p (*==)
    where
    
    (*==) (T.R (TimeLocale fy1 fy2 fy3 fy4 fy5 fy6 fy7) _)
      (T.R (TimeLocale fy8 fy9 fy10 fy11 fy12 fy13 fy14) _) p =
      T.uwrapForward p
        (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy8)
            Hat.Prelude.*&&
            (T.uwrapForward p
              (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy2 fy9)
                  Hat.Prelude.*&&
                  (T.uwrapForward p
                    (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p)
                          fy3 fy10)
                        Hat.Prelude.*&&
                        (T.uwrapForward p
                          (((T.uap2 T.mkNoSrcPos p
                                (T.mkNoSrcPos Hat.Prelude.!== p) fy4 fy11)
                              Hat.Prelude.*&&
                              (T.uwrapForward p
                                (((T.uap2 T.mkNoSrcPos p
                                      (T.mkNoSrcPos Hat.Prelude.!== p) fy5 fy12)
                                    Hat.Prelude.*&&
                                    (T.uwrapForward p
                                      (((T.uap2 T.mkNoSrcPos p
                                            (T.mkNoSrcPos Hat.Prelude.!== p) fy6
                                            fy13)
                                          Hat.Prelude.*&&
                                          (T.uap2 T.mkNoSrcPos p
                                            (T.mkNoSrcPos Hat.Prelude.!== p) fy7
                                            fy14)) p))) p))) p))) p))) p))) p)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (TimeLocale)
  where
  
  gcompare pcompare p =
    T.ufun2 a11v25v11v27compare pcompare p hcompare
    where
    
    hcompare (T.R (TimeLocale fy3 fy4 fy5 fy6 fy7 fy8 fy9) _)
      (T.R (TimeLocale fy10 fy11 fy12 fy13 fy14 fy15 fy16) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v11v25v11v27v1 (T.R Hat.Prelude.EQ _) p =
            T.uccase T.mkNoSrcPos p
              (let
                v11v25v11v27v1 (T.R Hat.Prelude.EQ _) p =
                  T.uccase T.mkNoSrcPos p
                    (let
                      v11v25v11v27v1 (T.R Hat.Prelude.EQ _) p =
                        T.uccase T.mkNoSrcPos p
                          (let
                            v11v25v11v27v1 (T.R Hat.Prelude.EQ _) p =
                              T.uccase T.mkNoSrcPos p
                                (let
                                  v11v25v11v27v1 (T.R Hat.Prelude.EQ _) p =
                                    T.uccase T.mkNoSrcPos p
                                      (let
                                        v11v25v11v27v1 (T.R Hat.Prelude.EQ _)
                                          p =
                                          T.uap2 T.mkNoSrcPos p
                                            (Hat.Prelude.gcompare T.mkNoSrcPos
                                              p) fy9 fy16
                                        v11v25v11v27v1 fy1 p =
                                          T.projection T.mkNoSrcPos p fy1 in
                                        (v11v25v11v27v1))
                                      (T.uap2 T.mkNoSrcPos p
                                        (Hat.Prelude.gcompare T.mkNoSrcPos p)
                                        fy8 fy15)
                                  v11v25v11v27v1 fy1 p =
                                    T.projection T.mkNoSrcPos p fy1 in
                                  (v11v25v11v27v1))
                                (T.uap2 T.mkNoSrcPos p
                                  (Hat.Prelude.gcompare T.mkNoSrcPos p) fy7
                                  fy14)
                            v11v25v11v27v1 fy1 p =
                              T.projection T.mkNoSrcPos p fy1 in
                            (v11v25v11v27v1))
                          (T.uap2 T.mkNoSrcPos p
                            (Hat.Prelude.gcompare T.mkNoSrcPos p) fy6 fy13)
                      v11v25v11v27v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
                      (v11v25v11v27v1))
                    (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
                      fy5 fy12)
                v11v25v11v27v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
                (v11v25v11v27v1))
              (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy4
                fy11)
          v11v25v11v27v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
          (v11v25v11v27v1))
        (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p) fy3 fy10)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a11v25v11v27localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a11v25v11v27localFromEnum
      
      hlocalFromEnum (T.R (TimeLocale _ _ _ _ _ _ _) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Show (TimeLocale)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a11v30v11v33showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (TimeLocale fy2 fy3 fy4 fy5 fy6 fy7 fy8) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
          (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "TimeLocale{"))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.Prelude.gshowString T.mkNoSrcPos p)
                    (T.fromLitString T.mkNoSrcPos p "wDays"))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                    (T.conChar T.mkNoSrcPos p '=')))
                (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 0)) fy2))
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ',')))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.Prelude.gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "months"))
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                      (T.conChar T.mkNoSrcPos p '=')))
                  (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p 0)) fy3))
                (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                  (T.conChar T.mkNoSrcPos p ',')))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                      (T.uap1 T.mkNoSrcPos p
                        (Hat.Prelude.gshowString T.mkNoSrcPos p)
                        (T.fromLitString T.mkNoSrcPos p "amPm"))
                      (T.uap1 T.mkNoSrcPos p
                        (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                        (T.conChar T.mkNoSrcPos p '=')))
                    (T.uap2 T.mkNoSrcPos p
                      (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p
                        (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                        (T.conInteger T.mkNoSrcPos p 0)) fy4))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                    (T.conChar T.mkNoSrcPos p ',')))
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                        (T.uap1 T.mkNoSrcPos p
                          (Hat.Prelude.gshowString T.mkNoSrcPos p)
                          (T.fromLitString T.mkNoSrcPos p "dateTimeFmt"))
                        (T.uap1 T.mkNoSrcPos p
                          (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                          (T.conChar T.mkNoSrcPos p '=')))
                      (T.uap2 T.mkNoSrcPos p
                        (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                        (T.uap1 T.mkNoSrcPos p
                          (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                          (T.conInteger T.mkNoSrcPos p 0)) fy5))
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                      (T.conChar T.mkNoSrcPos p ',')))
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.Prelude.gshowString T.mkNoSrcPos p)
                            (T.fromLitString T.mkNoSrcPos p "dateFmt"))
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                            (T.conChar T.mkNoSrcPos p '=')))
                        (T.uap2 T.mkNoSrcPos p
                          (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p 0)) fy6))
                      (T.uap1 T.mkNoSrcPos p
                        (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                        (T.conChar T.mkNoSrcPos p ',')))
                    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                            (T.uap1 T.mkNoSrcPos p
                              (Hat.Prelude.gshowString T.mkNoSrcPos p)
                              (T.fromLitString T.mkNoSrcPos p "timeFmt"))
                            (T.uap1 T.mkNoSrcPos p
                              (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                              (T.conChar T.mkNoSrcPos p '=')))
                          (T.uap2 T.mkNoSrcPos p
                            (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p 0)) fy7))
                        (T.uap1 T.mkNoSrcPos p
                          (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                          (T.conChar T.mkNoSrcPos p ',')))
                      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.Prelude.gshowString T.mkNoSrcPos p)
                            (T.fromLitString T.mkNoSrcPos p "time12Fmt"))
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.Prelude.gshowChar T.mkNoSrcPos p)
                            (T.conChar T.mkNoSrcPos p '=')))
                        (T.uap2 T.mkNoSrcPos p
                          (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p 0)) fy8)))))))))
        (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowChar T.mkNoSrcPos p)
          (T.conChar T.mkNoSrcPos p '}'))
    hshowsPrec _ _ p = T.fatal p
    
  

gdefaultTimeLocale :: T.RefSrcPos -> T.RefExp -> T.R TimeLocale

sdefaultTimeLocale :: T.R TimeLocale

gdefaultTimeLocale pdefaultTimeLocale p =
  T.uconstUse pdefaultTimeLocale p sdefaultTimeLocale

sdefaultTimeLocale =
  T.uconstDef T.mkRoot adefaultTimeLocale
    (\ p ->
      T.wrapVal T.mkNoSrcPos
        ((TimeLocale (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p)
            (Hat.PreludeBasic.gundefined T.mkNoSrcPos p))
          {bwDays
            = T.fromExpList T.mkNoSrcPos p
              [T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Sunday")
                  (T.fromLitString T.mkNoSrcPos p "Sun")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Monday")
                  (T.fromLitString T.mkNoSrcPos p "Mon")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Tuesday")
                  (T.fromLitString T.mkNoSrcPos p "Tue")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Wednesday")
                  (T.fromLitString T.mkNoSrcPos p "Wed")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Thursday")
                  (T.fromLitString T.mkNoSrcPos p "Thu")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Friday")
                  (T.fromLitString T.mkNoSrcPos p "Fri")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "Saturday")
                  (T.fromLitString T.mkNoSrcPos p "Sat")]
          ,bmonths
            = T.fromExpList T.mkNoSrcPos p
              [T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "January")
                  (T.fromLitString T.mkNoSrcPos p "Jan")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "February")
                  (T.fromLitString T.mkNoSrcPos p "Feb")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "March")
                  (T.fromLitString T.mkNoSrcPos p "Mar")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "April")
                  (T.fromLitString T.mkNoSrcPos p "Apr")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "May")
                  (T.fromLitString T.mkNoSrcPos p "May")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "June")
                  (T.fromLitString T.mkNoSrcPos p "Jun")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "July")
                  (T.fromLitString T.mkNoSrcPos p "Jul")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "August")
                  (T.fromLitString T.mkNoSrcPos p "Aug")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "September")
                  (T.fromLitString T.mkNoSrcPos p "Sep")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "October")
                  (T.fromLitString T.mkNoSrcPos p "Oct")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "November")
                  (T.fromLitString T.mkNoSrcPos p "Nov")
                ,T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                  (T.fromLitString T.mkNoSrcPos p "December")
                  (T.fromLitString T.mkNoSrcPos p "Dec")]
          ,bamPm
            = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.fromLitString T.mkNoSrcPos p "AM")
              (T.fromLitString T.mkNoSrcPos p "PM")
          ,bdateTimeFmt
            = T.fromLitString T.mkNoSrcPos p "%a %b %e %H:%M:%S %Z %Y"
          ,bdateFmt = T.fromLitString T.mkNoSrcPos p "%m/%d/%y"
          ,btimeFmt = T.fromLitString T.mkNoSrcPos p "%H:%M:%S"
          ,btime12Fmt = T.fromLitString T.mkNoSrcPos p "%I:%M:%S %p"}) p)

tLocale = T.mkModule "Locale" "Locale.hs" Prelude.False

aTimeLocale =
  T.mkConstructorWFields tLocale 50019 50028 3 7 "TimeLocale"
    (awDays
      :
      (amonths
        :
        (aamPm : (adateTimeFmt : (adateFmt : (atimeFmt : (atime12Fmt : [])))))))

awDays = T.mkVariable tLocale 60009 60013 3 1 "wDays" Prelude.False

amonths = T.mkVariable tLocale 70009 70014 3 1 "months" Prelude.False

aamPm = T.mkVariable tLocale 80009 80012 3 1 "amPm" Prelude.False

adateTimeFmt = T.mkVariable tLocale 90009 90019 3 1 "dateTimeFmt" Prelude.False

adateFmt = T.mkVariable tLocale 90022 90028 3 1 "dateFmt" Prelude.False

atimeFmt = T.mkVariable tLocale 100011 100017 3 1 "timeFmt" Prelude.False

atime12Fmt = T.mkVariable tLocale 100020 100028 3 1 "time12Fmt" Prelude.False

adefaultTimeLocale =
  T.mkVariable tLocale 140001 310017 3 0 "defaultTimeLocale" Prelude.False

(+##=$#=##=$$==) = T.mkVariable tLocale 110021 110022 3 2 "==" Prelude.False

a11v25v11v27compare =
  T.mkVariable tLocale 110025 110027 3 2 "compare" Prelude.False

a11v30v11v33showsPrec =
  T.mkVariable tLocale 110030 110033 3 2 "showsPrec" Prelude.False

a11v25v11v27localFromEnum =
  T.mkVariable tLocale 110025 110027 3 1 "localFromEnum" Prelude.True
