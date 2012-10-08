module Hat.Time
  (ClockTime(),Month(January,February,March,April,May,June,July,August,September
      ,October,November,December),aJanuary,aFebruary,aMarch,aApril,aMay,aJune
    ,aJuly,aAugust,aSeptember,aOctober,aNovember,aDecember,Day(Sunday,Monday
      ,Tuesday,Wednesday,Thursday,Friday,Saturday),aSunday,aMonday,aTuesday
    ,aWednesday,aThursday,aFriday,aSaturday,CalendarTime(CalendarTime,bctYear
      ,bctMonth,bctDay,bctHour,bctMin,bctSec,bctPicosec,bctWDay,bctYDay
      ,bctTZName,bctTZ,bctIsDST),gctYear,gctMonth,gctDay,gctHour,gctMin,gctSec
    ,gctPicosec,gctWDay,gctYDay,gctTZName,gctTZ,gctIsDST,hctYear,hctMonth,hctDay
    ,hctHour,hctMin,hctSec,hctPicosec,hctWDay,hctYDay,hctTZName,hctTZ,hctIsDST
    ,actYear,actMonth,actDay,actHour,actMin,actSec,actPicosec,actWDay,actYDay
    ,actTZName,actTZ,actIsDST,aCalendarTime,TimeDiff(TimeDiff,btdYear,btdMonth
      ,btdDay,btdHour,btdMin,btdSec,btdPicosec),gtdYear,gtdMonth,gtdDay,gtdHour
    ,gtdMin,gtdSec,gtdPicosec,htdYear,htdMonth,htdDay,htdHour,htdMin,htdSec
    ,htdPicosec,atdYear,atdMonth,atdDay,atdHour,atdMin,atdSec,atdPicosec
    ,aTimeDiff,ggetClockTime,gaddToClockTime,aaddToClockTime,haddToClockTime
    ,gdiffClockTimes,adiffClockTimes,hdiffClockTimes,gtoCalendarTime
    ,atoCalendarTime,htoCalendarTime,gtoUTCTime,atoUTCTime,htoUTCTime
    ,gtoClockTime,atoClockTime,htoClockTime,gcalendarTimeToString
    ,acalendarTimeToString,hcalendarTimeToString,gformatCalendarTime
    ,aformatCalendarTime,hformatCalendarTime) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Ix  (Ix())
import Hat.Locale  (TimeLocale(TimeLocale,bwDays,bmonths,bamPm,bdateTimeFmt
    ,bdateFmt,btimeFmt,btime12Fmt),gwDays,gmonths,gamPm,gdateTimeFmt,gdateFmt
  ,gtimeFmt,gtime12Fmt,hwDays,hmonths,hamPm,hdateTimeFmt,hdateFmt,htimeFmt
  ,htime12Fmt,awDays,amonths,aamPm,adateTimeFmt,adateFmt,atimeFmt,atime12Fmt
  ,aTimeLocale,gdefaultTimeLocale)
import Hat.Char  (gintToDigit,aintToDigit,hintToDigit)
import Hat.PreludeBuiltinTypes 
import Hat.TimeBuiltinTypes 
import Hat.TimeBuiltin 
import qualified System.Time as Time 

instance Ord (ClockTime)
  where
  
  gcompare pcompare p = T.uconstUse pcompare p scompare
  
  scompare =
    T.uconstDef T.mkRoot a28v3v28v32compare
      (\ p -> gprimClockTimeCompare T.mkNoSrcPos p)
  
  (!<=) (%<=) p = T.uconstUse (%<=) p (|<=)
  
  (|<=) =
    T.uconstDef T.mkRoot (+$>=&=$>=$+<=)
      (\ p -> gprimClockTimeLeEq T.mkNoSrcPos p)
  

instance Eq (ClockTime)
  where
  
  (!==) (%==) p = T.uconstUse (%==) p (|==)
  
  (|==) =
    T.uconstDef T.mkRoot (+%$=&=%$=$+==)
      (\ p -> gprimClockTimeEqEq T.mkNoSrcPos p)
  

gprimClockTimeCompare ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime (T.Fun ClockTime Ordering))

gprimClockTimeCompare pprimClockTimeCompare p =
  T.ufun2 aprimClockTimeCompare pprimClockTimeCompare p hprimClockTimeCompare

hprimClockTimeCompare z1primClockTimeCompare z2primClockTimeCompare
  kprimClockTimeCompare =
  fromOrdering kprimClockTimeCompare
    (Prelude.compare (toClockTime kprimClockTimeCompare z1primClockTimeCompare)
      (toClockTime kprimClockTimeCompare z2primClockTimeCompare))

gprimClockTimeLeEq ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime (T.Fun ClockTime Bool))

gprimClockTimeLeEq pprimClockTimeLeEq p =
  T.ufun2 aprimClockTimeLeEq pprimClockTimeLeEq p hprimClockTimeLeEq

hprimClockTimeLeEq z1primClockTimeLeEq z2primClockTimeLeEq kprimClockTimeLeEq =
  fromBool kprimClockTimeLeEq
    ((toClockTime kprimClockTimeLeEq z1primClockTimeLeEq)
      Prelude.<=
      (toClockTime kprimClockTimeLeEq z2primClockTimeLeEq))

gprimClockTimeEqEq ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime (T.Fun ClockTime Bool))

gprimClockTimeEqEq pprimClockTimeEqEq p =
  T.ufun2 aprimClockTimeEqEq pprimClockTimeEqEq p hprimClockTimeEqEq

hprimClockTimeEqEq z1primClockTimeEqEq z2primClockTimeEqEq kprimClockTimeEqEq =
  fromBool kprimClockTimeEqEq
    ((toClockTime kprimClockTimeEqEq z1primClockTimeEqEq)
      Prelude.==
      (toClockTime kprimClockTimeEqEq z2primClockTimeEqEq))

ggetClockTime :: T.RefSrcPos -> T.RefExp -> T.R (IO ClockTime)

ggetClockTime pgetClockTime p = T.uconstUse pgetClockTime p sgetClockTime

sgetClockTime =
  T.uconstDef T.mkRoot agetClockTime
    (\ p -> (T.fromIO fromClockTime) p Time.getClockTime)

gaddToClockTime ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun TimeDiff (T.Fun ClockTime ClockTime))

gaddToClockTime paddToClockTime p =
  T.ufun2 aaddToClockTime paddToClockTime p haddToClockTime

haddToClockTime z1addToClockTime z2addToClockTime kaddToClockTime =
  fromClockTime kaddToClockTime
    (Time.addToClockTime (toTimeDiff kaddToClockTime z1addToClockTime)
      (toClockTime kaddToClockTime z2addToClockTime))

gdiffClockTimes ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime (T.Fun ClockTime TimeDiff))

gdiffClockTimes pdiffClockTimes p =
  T.ufun2 adiffClockTimes pdiffClockTimes p hdiffClockTimes

hdiffClockTimes z1diffClockTimes z2diffClockTimes kdiffClockTimes =
  fromTimeDiff kdiffClockTimes
    (Time.diffClockTimes (toClockTime kdiffClockTimes z1diffClockTimes)
      (toClockTime kdiffClockTimes z2diffClockTimes))

gtoCalendarTime ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime (IO CalendarTime))

gtoCalendarTime ptoCalendarTime p =
  T.ufun1 atoCalendarTime ptoCalendarTime p htoCalendarTime

htoCalendarTime z1toCalendarTime ktoCalendarTime =
  (T.fromIO fromCalendarTime) ktoCalendarTime
    (Time.toCalendarTime (toClockTime ktoCalendarTime z1toCalendarTime))

gtoUTCTime :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun ClockTime CalendarTime)

gtoUTCTime ptoUTCTime p = T.ufun1 atoUTCTime ptoUTCTime p htoUTCTime

htoUTCTime z1toUTCTime ktoUTCTime =
  fromCalendarTime ktoUTCTime
    (Time.toUTCTime (toClockTime ktoUTCTime z1toUTCTime))

gtoClockTime :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun CalendarTime ClockTime)

gtoClockTime ptoClockTime p = T.ufun1 atoClockTime ptoClockTime p htoClockTime

htoClockTime z1toClockTime ktoClockTime =
  fromClockTime ktoClockTime
    (Time.toClockTime (toCalendarTime ktoClockTime z1toClockTime))

gcalendarTimeToString ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun CalendarTime String)

gcalendarTimeToString pcalendarTimeToString p =
  T.ufun1 acalendarTimeToString pcalendarTimeToString p hcalendarTimeToString

hcalendarTimeToString z1calendarTimeToString kcalendarTimeToString =
  fromString kcalendarTimeToString
    (Time.calendarTimeToString
      (toCalendarTime kcalendarTimeToString z1calendarTimeToString))

gformatCalendarTime ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun TimeLocale (T.Fun String (T.Fun CalendarTime String)))

gformatCalendarTime pformatCalendarTime p =
  T.ufun3 aformatCalendarTime pformatCalendarTime p hformatCalendarTime

hformatCalendarTime z1formatCalendarTime z2formatCalendarTime
  z3formatCalendarTime kformatCalendarTime =
  fromString kformatCalendarTime
    (Time.formatCalendarTime
      (toTimeLocale kformatCalendarTime z1formatCalendarTime)
      (toString kformatCalendarTime z2formatCalendarTime)
      (toCalendarTime kformatCalendarTime z3formatCalendarTime))

tTime = T.mkModule "Time" "Time.hs" Prelude.False

aprimClockTimeCompare =
  T.mkVariable tTime 340001 350059 3 2 "primClockTimeCompare" Prelude.False

aprimClockTimeLeEq =
  T.mkVariable tTime 370001 380052 3 2 "primClockTimeLeEq" Prelude.False

aprimClockTimeEqEq =
  T.mkVariable tTime 400001 410052 3 2 "primClockTimeEqEq" Prelude.False

agetClockTime =
  T.mkVariable tTime 430001 440030 3 0 "getClockTime" Prelude.False

aaddToClockTime =
  T.mkVariable tTime 460001 470066 3 2 "addToClockTime" Prelude.False

adiffClockTimes =
  T.mkVariable tTime 490001 500065 3 2 "diffClockTimes" Prelude.False

atoCalendarTime =
  T.mkVariable tTime 520001 530046 3 1 "toCalendarTime" Prelude.False

atoUTCTime = T.mkVariable tTime 550001 560056 3 1 "toUTCTime" Prelude.False

atoClockTime = T.mkVariable tTime 580001 590053 3 1 "toClockTime" Prelude.False

acalendarTimeToString =
  T.mkVariable tTime 610001 620050 3 1 "calendarTimeToString" Prelude.False

aformatCalendarTime =
  T.mkVariable tTime 640001 650069 3 3 "formatCalendarTime" Prelude.False

a28v3v28v32compare =
  T.mkVariable tTime 280003 280032 3 0 "compare" Prelude.False

(+$>=&=$>=$+<=) = T.mkVariable tTime 290004 290026 16 0 "<=" Prelude.False

(+%$=&=%$=$+==) = T.mkVariable tTime 320004 320026 16 0 "==" Prelude.False
