{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Hat.TimeBuiltin(ClockTime,module Hat.TimeBuiltin) where

import Hat.Hat as T
import Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.TimeBuiltinTypes
import qualified System.Time as Time
import System.Time(ClockTime)
import qualified System.Locale as Locale
import Hat.Locale

toClockTime :: RefExp -> R ClockTime -> Time.ClockTime
toClockTime h (R v _) = v

fromClockTime :: RefExp -> Time.ClockTime -> R ClockTime
fromClockTime h v = R v (T.mkValueUse h mkNoSrcPos aClockTime)

aClockTime :: RefAtom
aClockTime = mkAbstract "ClockTime"


toMonth :: RefExp -> R Month -> Time.Month
toMonth h (R January _) = Time.January
toMonth h (R February _) = Time.February
toMonth h (R March _) = Time.March
toMonth h (R April _) = Time.April
toMonth h (R May _) = Time.May
toMonth h (R June _) = Time.June
toMonth h (R July _) = Time.July
toMonth h (R August _) = Time.August
toMonth h (R September _) = Time.September
toMonth h (R October _) = Time.October
toMonth h (R November _) = Time.November
toMonth h (R December _) = Time.December

fromMonth :: RefExp -> Time.Month -> R Month
fromMonth h Time.January = T.con0 mkNoSrcPos h January aJanuary
fromMonth h Time.February = T.con0 mkNoSrcPos h February aFebruary 
fromMonth h Time.March = T.con0 mkNoSrcPos h March aMarch 
fromMonth h Time.April = T.con0 mkNoSrcPos h April aApril
fromMonth h Time.May = T.con0 mkNoSrcPos h May aMay
fromMonth h Time.June = T.con0 mkNoSrcPos h June aJune
fromMonth h Time.July = T.con0 mkNoSrcPos h July aJuly
fromMonth h Time.August = T.con0 mkNoSrcPos h August aAugust
fromMonth h Time.September = T.con0 mkNoSrcPos h September aSeptember
fromMonth h Time.October = T.con0 mkNoSrcPos h October aOctober
fromMonth h Time.November = T.con0 mkNoSrcPos h November aNovember
fromMonth h Time.December = T.con0 mkNoSrcPos h December aDecember

toDay :: RefExp -> R Day -> Time.Day
toDay h (R Sunday _) = Time.Sunday
toDay h (R Monday _) = Time.Monday
toDay h (R Tuesday _) = Time.Tuesday
toDay h (R Wednesday _) = Time.Wednesday
toDay h (R Thursday _) = Time.Thursday
toDay h (R Friday _) = Time.Friday
toDay h (R Saturday _) = Time.Saturday

fromDay :: RefExp -> Time.Day -> R Day
fromDay h Time.Sunday = T.con0 mkNoSrcPos h Sunday aSunday
fromDay h Time.Monday = T.con0 mkNoSrcPos h Monday aMonday
fromDay h Time.Tuesday = T.con0 mkNoSrcPos h Tuesday aTuesday
fromDay h Time.Wednesday = T.con0 mkNoSrcPos h Wednesday aWednesday
fromDay h Time.Thursday = T.con0 mkNoSrcPos h Thursday aThursday
fromDay h Time.Friday = T.con0 mkNoSrcPos h Friday aFriday
fromDay h Time.Saturday = T.con0 mkNoSrcPos h Saturday aSaturday

toCalendarTime :: RefExp -> R CalendarTime -> Time.CalendarTime
toCalendarTime h 
  (R (CalendarTime year month day hour min sec picosec wday yday tzname tz 
       isdst) _) =
  Time.CalendarTime (toInt h year) (toMonth h month) (toInt h day) 
    (toInt h hour) (toInt h min) (toInt h sec) 
    (T.toInteger h picosec) 
    (toDay h wday) (toInt h yday) (toString h tzname) (toInt h tz) 
    (toBool h isdst)

fromCalendarTime :: RefExp -> Time.CalendarTime -> R CalendarTime
fromCalendarTime h 
  (Time.CalendarTime year month day hour min sec picosec wday yday tzname tz 
    isdst) =
  T.con12 mkNoSrcPos h CalendarTime aCalendarTime 
    (T.wrapForward h (fromInt h year)) (T.wrapForward h (fromMonth h month))
    (T.wrapForward h (fromInt h day))  (T.wrapForward h (fromInt h hour))
    (T.wrapForward h (fromInt h min)) (T.wrapForward h (fromInt h sec)) 
    (T.wrapForward h (T.fromInteger h picosec))
    (T.wrapForward h (fromDay h wday)) (T.wrapForward h (fromInt h yday))
    (T.wrapForward h (fromString h tzname)) (T.wrapForward h (fromInt h tz)) 
    (T.wrapForward h (fromBool h isdst))

toTimeDiff :: RefExp -> R TimeDiff -> Time.TimeDiff
toTimeDiff h (R (TimeDiff year month day hour min sec picosec) _) =
  Time.TimeDiff (toInt h year) (toInt h month) (toInt h day) 
    (toInt h hour) (toInt h min) (toInt h sec) 
    (T.toInteger h picosec)

fromTimeDiff :: RefExp -> Time.TimeDiff -> R TimeDiff
fromTimeDiff h (Time.TimeDiff year month day hour min sec picosec) =
  T.con7 mkNoSrcPos h TimeDiff aTimeDiff 
    (T.wrapForward h (fromInt h year)) (T.wrapForward h (fromInt h month))
    (T.wrapForward h (fromInt h day)) 
    (T.wrapForward h (fromInt h hour)) (T.wrapForward h (fromInt h min)) 
    (T.wrapForward h (fromInt h sec)) 
    (T.wrapForward h (T.fromInteger h picosec))

toTimeLocale :: RefExp -> R TimeLocale -> Locale.TimeLocale
toTimeLocale h 
  (R (TimeLocale twDays tmonths tamPm tdateTimeFmt tdateFmt ttimeFmt 
       ttime12Fmt) _) =
  Locale.TimeLocale{Locale.wDays=(toList (toTuple2 toString toString) h twDays)
    ,Locale.months=(toList (toTuple2 toString toString) h tmonths) 
    ,Locale.amPm=(toTuple2 toString toString h tamPm)
    ,Locale.dateTimeFmt=(toString h tdateTimeFmt)
    ,Locale.dateFmt=(toString h tdateFmt),Locale.timeFmt=(toString h ttimeFmt)
    ,Locale.time12Fmt=(toString h ttime12Fmt)}

fromOrdering :: RefExp -> Prelude.Ordering -> R Hat.Prelude.Ordering
fromOrdering h Prelude.LT = T.con0 mkNoSrcPos h Hat.Prelude.LT aLT
fromOrdering h Prelude.EQ = T.con0 mkNoSrcPos h Hat.Prelude.EQ aLT
fromOrdering h Prelude.GT = T.con0 mkNoSrcPos h Hat.Prelude.GT aLT
